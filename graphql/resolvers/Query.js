import {
  authenticated,
  findValue,
  authorized,
  deviceToParent,
  create2FSecret,
  inheritAuthorized,
  environmentToParent,
  valueToParent,
  authorizationLevel,
  deviceAuthorized,
  valueAuthorized,
  deviceInheritAuthorized,
} from "./utilities"
const { Fido2Lib } = require("fido2-lib-clone")
import jwt from "jwt-simple"
import moment from "moment"
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
// function str2ab(str) {
//   return Uint8Array.from(str, c => c.charCodeAt(0))
// }

const QueryResolver = ({ User, WebauthnKey }) => ({
  user(root, args, context) {
    return async (resolve, reject) => {
      if (args.email && args.id) {
        reject("Cannot pass both email and id")
      } else if (args.email) {
        const userFound = await User.find({ where: { email: args.email } })

        if (userFound) {
          resolve({ id: userFound.id })
        } else {
          reject("User not found")
        }
      } else if (args.id) {
        resolve({ id: args.id })
      } else if (context.auth && context.auth.userId) {
        resolve({ id: context.auth.userId })
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

      return deviceAuthorized(
        requestedId,
        context,
        1,
        async (resolve, reject, deviceFound) => {
          resolve(deviceFound.dataValues)
        }
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
      },
      environmentToParent
    )
  },
  value(root, args, context) {
    return valueAuthorized(
      args.id,
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve(valueFound)
      }
    )
  },
  floatValue(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.floatValueLoaderById,
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve(valueFound)
      }
    )
  },
  stringValue(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.stringValueLoaderById,
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve(valueFound)
      }
    )
  },
  booleanValue(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.booleanValueLoaderById,
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve(valueFound)
      }
    )
  },
  floatSeriesValue(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.floatSeriesValueLoaderById,
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve(valueFound)
      }
    )
  },
  categorySeriesValue(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.categorySeriesValueLoaderById,
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve(valueFound)
      }
    )
  },
  notification(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.notificationLoaderById,
      context,
      1,
      async (resolve, reject, notificationFound) => {
        resolve(notificationFound)
      }
    )
  },
  floatSeriesNode(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.floatSeriesNodeLoaderById,
      context,
      1,
      async (resolve, reject, floatSeriesNodeFound) => {
        resolve(floatSeriesNodeFound)
      }
    )
  },
  categorySeriesNode(root, args, context) {
    return deviceInheritAuthorized(
      args.id,
      context.dataLoaders.categorySeriesNodeLoaderById,
      context,
      1,
      async (resolve, reject, floatSeriesNodeFound) => {
        resolve(floatSeriesNodeFound)
      }
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
