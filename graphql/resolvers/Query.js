import {
  authenticated,
  findValue,
  authorized,
  deviceToParent,
  notificationToParent,
  create2FSecret,
  inheritAuthorized,
  environmentToParent,
  valueToParent,
  authorizationLevel,
} from "./utilities"
import bcrypt from "bcryptjs"
const { Fido2Lib } = require("fido2-lib-clone")
import jwt from "jwt-simple"
import moment, { relativeTimeRounding } from "moment"
require("dotenv").config()

/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}
const QUERY_COST = 1

const f2l = new Fido2Lib()

function ab2str(buf) {
  return String.fromCharCode.apply(null, new Uint8Array(buf))
}
function str2ab(str) {
  return Uint8Array.from(str, c => c.charCodeAt(0))
}

const QueryResolver = ({ User, WebauthnKey }) => ({
  user(root, args, context) {
    return async (resolve, reject) => {
      if (args.email) {
        const userFound = await User.find({ where: { email: args.email } })

        if (userFound) {
          resolve({ id: userFound.id })
          context.billingUpdater.update(QUERY_COST)
        } else {
          reject("User not found")
        }
      } else if (args.id) {
        resolve({ id: args.id })
        context.billingUpdater.update(QUERY_COST)
      } else if (context.auth && context.auth.userId) {
        resolve({ id: context.auth.userId })
        context.billingUpdater.update(QUERY_COST)
      } else {
        reject("Unauthenticated user query requires email or id field")
      }
    }
  },
  device(root, args, context) {
    return (resolve, reject) => {
      const requestedId = args.id || context.auth.deviceId
      if (!requestedId) {
        reject("id field is required")
        return
      }

      return authorized(
        requestedId,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          resolve(deviceFound.dataValues)

          context.billingUpdater.update(QUERY_COST)
        },
        deviceToParent,
        ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
      )(resolve, reject)
    }
  },
  environment(root, args, context) {
    return authorized(
      args.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        resolve(environmentFound.dataValues)
        context.billingUpdater.update(QUERY_COST)
      },
      environmentToParent
    )
  },
  value(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const userFound = await context.dataLoaders.userLoaderById.load(
        context.auth.userId
      )
      let valueFound
      try {
        valueFound = await findValue(context, args.id, userFound)
      } catch (e) {
        if (e.message === "The requested resource does not exist") {
          reject(e)
          return
        }

        throw e
      }
      const environmentFound = await valueToParent(context)(valueFound)

      if (
        (await authorizationLevel(environmentFound, userFound, context)) > 0
      ) {
        resolve(valueFound)
        context.billingUpdater.update(QUERY_COST)
      } else {
        reject("You are not authorized to perform this operation")
      }
    })
  },
  notification(root, args, context) {
    return inheritAuthorized(
      args.id,
      context.dataLoaders.notificationLoaderById,
      User,
      notificationFound => notificationFound.deviceId,
      context,
      context.dataLoaders.deviceLoaderById,
      1,
      async (resolve, reject, notificationFound) => {
        resolve(notificationFound)
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
  plotNode(root, args, context) {
    return inheritAuthorized(
      args.id,
      context.dataLoaders.plotNodeLoaderById,
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      context.dataLoaders.plotValueLoaderById,
      1,
      async (resolve, reject, plotNodeFound) => {
        resolve(plotNodeFound)
      },
      valueToParent
    )
  },
  categoryPlotNode(root, args, context) {
    return inheritAuthorized(
      args.id,
      context.dataLoaders.categoryPlotNodeLoaderById,
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      context.dataLoaders.categoryPlotValueLoaderById,
      1,
      async (resolve, reject, plotNodeFound) => {
        resolve(plotNodeFound)
      },
      valueToParent
    )
  },
  getNewTotpSecret(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const userFound = await context.dataLoaders.userLoaderById.load(
        context.auth.userId
      )
      if (!userFound) {
        reject("User doesn't exist. Use `signUp` to create one")
      } else {
        const { secret, qrCode } = create2FSecret(userFound.email)
        resolve({ secret, qrCode })
      }
    })
  },
  getWebAuthnEnableChallenge(root, args, context) {
    return authenticated(
      context,
      async (resolve, reject) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

        if (!userFound) {
          reject("user does not exist")
          return
        }

        let registrationOptions = await f2l.attestationOptions()
        registrationOptions.challenge = ab2str(registrationOptions.challenge)

        const jwtChallenge = jwt.encode(
          {
            exp: moment()
              .utc()
              .add({ minutes: 15 })
              .unix(),
            challenge: registrationOptions.challenge,
          },
          process.env.JWT_SECRET,
          "HS512"
        )

        registrationOptions.rp = {
          name: "Igloo",
          icon:
            "https://raw.githubusercontent.com/IglooCloud/IglooBering/master/IglooLogo.png",
        }
        registrationOptions.user = {
          name: userFound.email,
          displayName: userFound.email,
          id: userFound.id,
        }
        registrationOptions.pubKeyCredParams = [
          { alg: -7, type: "public-key" },
          { alg: -257, type: "public-key" },
        ]
        registrationOptions.timeout = 60000
        registrationOptions.attestation = "none"

        resolve({
          jwtChallenge,
          publicKeyOptions: JSON.stringify(registrationOptions),
        })
      },
      ["TEMPORARY", "CHANGE_AUTHENTICATION"]
    )
  },
  getWebAuthnLogInChallenge(root, args, context) {
    return async (resolve, reject) => {
      const userFound = await User.find({ where: { email: args.email } })
      if (!userFound) {
        reject("This user doesn't exist")
        return
      }

      const keys = await WebauthnKey.findAll({
        where: { userId: userFound.id },
      })

      if (keys.length === 0) {
        reject("No webauthn keys registered")
        return
      }

      const authnOptions = await f2l.assertionOptions()

      authnOptions.challenge = ab2str(authnOptions.challenge)
      authnOptions.allowCredentials = keys.reduce(
        (acc, { credId }) => [
          ...acc,
          {
            type: "public-key",
            alg: -7,
            id: credId,
          },
          {
            type: "public-key",
            alg: -257,
            id: credId,
          },
        ],
        []
      )

      authnOptions.rp = {
        name: "Igloo",
        icon:
          "https://raw.githubusercontent.com/IglooCloud/IglooBering/master/IglooLogo.png",
      }
      authnOptions.user = {
        id: userFound.id,
        name: args.email,
        displayName: args.email,
      }
      authnOptions.userVerification = "discouraged"
      authnOptions.timeout = 60000
      authnOptions.attestation = "none"

      const jwtChallenge = jwt.encode(
        {
          exp: moment()
            .utc()
            .add({ minutes: 15 })
            .unix(),
          challenge: authnOptions.challenge,
          userId: userFound.id,
        },
        process.env.JWT_SECRET,
        "HS512"
      )

      resolve({
        jwtChallenge,
        publicKeyOptions: JSON.stringify(authnOptions),
      })
    }
  },
})

export default QueryResolver
