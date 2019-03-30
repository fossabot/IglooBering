import bcrypt from "bcryptjs"
import {
  authenticated,
  generateAuthenticationToken,
  generatePermanentAuthenticationToken,
  CreateGenericValue,
  genericValueMutation,
  check2FCode,
  sendVerificationEmail,
  sendPasswordUpdatedEmail,
  sendTokenCreatedEmail,
  authorized,
  deviceToParent,
  authorizedValue,
  instanceToSharedIds,
  inheritAuthorized,
  randomEnvironmentPicture,
  randomUserIconColor,
  instanceToRole,
  authorizationLevel,
  deviceAuthorized,
  environmentToParent,
  sendEnvironmentSharedEmail,
  runInParallel,
  findValue,
  sendPushNotification,
  sendOwnerChangeEmail,
  sendAccountDeletedEmail,
  generateDeviceAuthenticationToken,
  sendOwnerChangeAcceptedEmail,
  sendEnvironmentShareAcceptedEmail,
  sendConfirmationEmail,
  deviceInheritAuthorized,
} from "./utilities"
const { Fido2Lib } = require("fido2-lib-clone")
import Stripe from "stripe"
import moment from "moment"
import jwt from "jwt-simple"
import zxcvbn from "zxcvbn"
import { Op } from "sequelize"
import QRCode from "qrcode-svg"

const f2l = new Fido2Lib()

require("dotenv").config()
/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

const SALT_ROUNDS = 10
const MAX_STORAGE = 250000 // BETA

const stripe = Stripe("sk_test_pku6xMd2Tjlv5EU4GkZHw7aS")

const isNotNullNorUndefined = value => value !== undefined && value !== null
const isOutOfBoundaries = (min, max, value) => {
  if (isNotNullNorUndefined(min) && value < min) return true
  if (isNotNullNorUndefined(max) && value > max) return true
  return false
}

const touch = async (Model, id, updatedAt = new Date()) =>
  await Model.update({ updatedAt }, { where: { id }, silent: true }) // FIXME: updated at is always set to current date by sequelize

function ab2str(buf) {
  return String.fromCharCode.apply(null, new Uint8Array(buf))
}
function str2ab(str) {
  return Uint8Array.from(str, c => c.charCodeAt(0))
}

function checkAuthenticationMethod(
  method,
  passwordCert,
  webauthnCert,
  totpCert
) {
  switch (method) {
    case "PASSWORD":
      return passwordCert !== null
    case "WEBAUTHN":
      return webauthnCert !== null
    case "TOTP":
      return totpCert !== null
    default:
      return false
  }
}

const validateCertificates = (
  passwordCertificate,
  webAuthnCertificate,
  totpCertificate,
  emailCertificate,
  reject,
  JWT_SECRET
) => {
  let decodedPasswordCertificate
  let decodedWebAuthnCertificate
  let decodedTotpCertificate
  let decodedEmailCertificate
  try {
    decodedPasswordCertificate = passwordCertificate
      ? jwt.decode(passwordCertificate, JWT_SECRET)
      : null
  } catch (e) {
    reject("Invalid, expired or malformed password certificate")
    return false
  }
  try {
    decodedEmailCertificate = emailCertificate
      ? jwt.decode(emailCertificate, JWT_SECRET)
      : null
  } catch (e) {
    reject("Invalid, expired or malformed email certificate")
    return false
  }
  try {
    decodedWebAuthnCertificate = webAuthnCertificate
      ? jwt.decode(webAuthnCertificate, JWT_SECRET)
      : null
  } catch (e) {
    reject("Invalid, expired or malformed WebAuthn certificate")
    return false
  }
  try {
    decodedTotpCertificate = totpCertificate
      ? jwt.decode(totpCertificate, JWT_SECRET)
      : null
  } catch (e) {
    reject("Invalid, expired or malformed TOTP certificate")
    return false
  }

  // check that the passed certificate attest the correct authentication method
  if (
    decodedPasswordCertificate &&
    decodedPasswordCertificate.certificateType !== "PASSWORD"
  ) {
    reject("Value passed to passwordCertificate is not a password certificate")
    return false
  } else if (
    decodedWebAuthnCertificate &&
    decodedWebAuthnCertificate.certificateType !== "WEBAUTHN"
  ) {
    reject("Value passed to webAuthnCertificate is not a WebAuthn certificate")
    return false
  } else if (
    decodedEmailCertificate &&
    decodedEmailCertificate.certificateType !== "EMAIL"
  ) {
    reject("Value passed to emailCertificate is not an email certificate")
    return false
  } else if (
    decodedTotpCertificate &&
    decodedTotpCertificate.certificateType !== "TOTP"
  ) {
    reject("Value passed to totpCertificate is not a TOTP certificate")
    return false
  }

  if (
    !decodedPasswordCertificate &&
    !decodedWebAuthnCertificate &&
    !decodedTotpCertificate &&
    !decodedEmailCertificate
  ) {
    reject("No certificate passed")
    return false
  }

  const userId =
    (decodedEmailCertificate && decodedEmailCertificate.userId) ||
    ((decodedPasswordCertificate && decodedPasswordCertificate.userId) ||
      (decodedWebAuthnCertificate && decodedWebAuthnCertificate.userId))

  if (
    (decodedPasswordCertificate &&
      decodedPasswordCertificate.userId !== userId) ||
    ((decodedWebAuthnCertificate &&
      decodedWebAuthnCertificate.userId !== userId) ||
      (decodedTotpCertificate && decodedTotpCertificate.userId !== userId))
  ) {
    reject("The various certificates refer to different users")
    return false
  }

  return {
    decodedPasswordCertificate,
    decodedWebAuthnCertificate,
    decodedTotpCertificate,
    decodedEmailCertificate,
    userId,
  }
}

const MutationResolver = (
  {
    User,
    PermanentToken,
    Device,
    Environment,
    FloatValue,
    StringValue,
    BooleanValue,
    PlotValue,
    PlotNode,
    CategoryPlotValue,
    CategoryPlotNode,
    Notification,
    PendingEnvironmentShare,
    PendingOwnerChange,
    EnvironmentAdmin,
    EnvironmentEditor,
    EnvironmentSpectator,
    WebauthnKey,
    EmailLoginToken,
  },
  WebPushNotification,
  pubsub,
  JWT_SECRET
) => {
  const resolvers = {
    // checks if the user exists, if so
    // compares the given password with the hash
    // and returns an access token
    logIn(root, args, context) {
      return async (resolve, reject) => {
        if (
          !args.passwordCertificate &&
          !args.webAuthnCertificate &&
          !args.emailCertificate
        ) {
          reject("No primary authentication method certificate passed")
          return
        }

        // decode the certificates passed
        const validatedCertificates = validateCertificates(
          args.passwordCertificate,
          args.webAuthnCertificate,
          args.totpCertificate,
          args.emailCertificate,
          reject,
          JWT_SECRET
        )

        if (!validatedCertificates) {
          return
        }

        let {
          decodedPasswordCertificate,
          decodedWebAuthnCertificate,
          decodedTotpCertificate,
          decodedEmailCertificate,
          userId,
        } = validatedCertificates

        const userFound = await context.dataLoaders.userLoaderById.load(userId)
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else {
          if (decodedEmailCertificate) {
            // setting context so that the resolvers for user know that the user is authenticated
            context.auth = {
              userId: userFound.id,
              accessLevel: "OWNER",
              tokenType: "TEMPORARY",
            }

            resolve({
              token: generateAuthenticationToken(userId, JWT_SECRET),
              user: userFound,
            })
            return
          }

          const {
            primaryAuthenticationMethods,
            secondaryAuthenticationMethods,
          } = userFound

          let primaryPassed = false
          for (let authenticationMethod of primaryAuthenticationMethods) {
            primaryPassed =
              primaryPassed ||
              checkAuthenticationMethod(
                authenticationMethod,
                decodedPasswordCertificate,
                decodedWebAuthnCertificate,
                decodedTotpCertificate
              )
          }
          if (!primaryPassed) {
            reject(
              "You didn't pass any certificate that matches your primaryAuthenticationMethods"
            )
            return
          }

          let secondaryPassed = secondaryAuthenticationMethods.length === 0
          for (let authenticationMethod of secondaryAuthenticationMethods) {
            secondaryPassed =
              secondaryPassed ||
              checkAuthenticationMethod(
                authenticationMethod,
                decodedPasswordCertificate,
                decodedWebAuthnCertificate,
                decodedTotpCertificate
              )
          }
          if (!secondaryPassed) {
            reject(
              "You didn't pass any certificate that matches your secondaryAuthenticationMethods"
            )
            return
          }

          // setting context so that the resolvers for user know that the user is authenticated
          context.auth = {
            userId: userFound.id,
            accessLevel: "OWNER",
            tokenType: "TEMPORARY",
          }

          resolve({
            token: generateAuthenticationToken(userId, JWT_SECRET),
            user: userFound,
          })
        }
      }
    },
    sendConfirmationEmail(root, args, context) {
      return async (resolve, reject) => {
        const userFound = await User.find({ where: { email: args.email } })

        if (!userFound) {
          reject("User not found")
        } else {
          const loginToken = await EmailLoginToken.create({
            userId: userFound.id,
          })

          sendConfirmationEmail(userFound.email, loginToken.id, args.operation)

          resolve(true)
        }
      }
    },
    verifyPassword(root, args, context) {
      return async (resolve, reject) => {
        const userFound = await User.find({
          where: { email: args.email },
        })
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else if (!userFound.dataValues.password) {
          reject("this user does not have a password")
        } else if (
          !bcrypt.compareSync(args.password, userFound.dataValues.password)
        ) {
          reject("Wrong password")
        } else {
          const certificate = jwt.encode(
            {
              exp: moment()
                .utc()
                .add({ minutes: 15 })
                .unix(),
              userId: userFound.id,
              certificateType: "PASSWORD",
            },
            JWT_SECRET,
            "HS512"
          )

          resolve(certificate)
        }
      }
    },
    verifyEmailToken(root, args, context) {
      return async (resolve, reject) => {
        const loginTokenFound = await EmailLoginToken.find({
          where: { id: args.token },
        })

        if (!loginTokenFound) {
          reject("Invalid, used or expired emailCertificate")
          return
        }
        const certificate = jwt.encode(
          {
            exp: moment()
              .utc()
              .add({ minutes: 15 })
              .unix(),
            userId: loginTokenFound.userId,
            certificateType: "EMAIL",
          },
          JWT_SECRET,
          "HS512"
        )

        await loginTokenFound.destroy()
        resolve(certificate)
      }
    },
    verifyTotp(root, args, context) {
      return async (resolve, reject) => {
        const userFound = await User.find({
          where: { email: args.email },
        })
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else if (!userFound.dataValues.twoFactorSecret) {
          reject("this user does not have TOTP authentication set up")
        } else if (
          !check2FCode(args.code, userFound.dataValues.twoFactorSecret)
        ) {
          reject("Code and secret do not match")
        } else {
          const certificate = jwt.encode(
            {
              exp: moment()
                .utc()
                .add({ minutes: 15 })
                .unix(),
              userId: userFound.id,
              certificateType: "TOTP",
            },
            JWT_SECRET,
            "HS512"
          )

          resolve(certificate)
        }
      }
    },
    verifyWebAuthn(root, args, context) {
      return async (resolve, reject) => {
        let clientAssertionResponse
        try {
          clientAssertionResponse = JSON.parse(args.challengeResponse)
        } catch (e) {
          if (e instanceof SyntaxError) {
            reject("Invaild JSON passed for challengeResponse")
            return
          } else {
            reject("Internal error")
          }
        }

        const keyFound = await WebauthnKey.find({
          where: { credId: ab2str(clientAssertionResponse.rawId) },
        })

        clientAssertionResponse.rawId = new Int8Array(
          clientAssertionResponse.rawId
        ).buffer
        clientAssertionResponse.response.clientDataJSON = new Int8Array(
          clientAssertionResponse.response.clientDataJSON
        ).buffer
        clientAssertionResponse.response.authenticatorData = new Int8Array(
          clientAssertionResponse.response.authenticatorData
        ).buffer
        clientAssertionResponse.response.signature = new Int8Array(
          clientAssertionResponse.response.signature
        ).buffer

        let decodedJwt
        try {
          decodedJwt = jwt.decode(args.jwtChallenge, process.env.JWT_SECRET)
        } catch (e) {
          if (e.message === "Not enough or too many segments") {
            reject("jwtChallenge is not a valid JWT")
          } else if (e.message === "No token supplied") {
            reject("No jwtChallenge supplied")
          } else if (
            e.message === "Algorithm not supported" ||
            e.message === "Signature verification failed"
          ) {
            reject("jwtChallenge was not created by Igloo")
          } else if (e.message === "Token expired") {
            reject("jwtChallenge expired")
          }

          return
        }
        if (!decodedJwt.challenge) {
          reject("jwtChallenge is not a challenge token")
          return
        }

        const { challenge: decodedChallenge, userId } = decodedJwt

        var assertionExpectations = {
          challenge: str2ab(decodedChallenge),
          origin: "https://aurora.igloo.ooo",
          factor: "either",
          publicKey: keyFound.publicKey,
          userHandle: null,
          prevCounter: keyFound.counter,
        }

        try {
          var authnResult = await f2l.assertionResult(
            clientAssertionResponse,
            assertionExpectations
          )

          await keyFound.update({
            counter: authnResult.authnrData.get("counter"),
          })

          const certificate = jwt.encode(
            {
              exp: moment()
                .utc()
                .add({ minutes: 15 })
                .unix(),
              userId,
              certificateType: "WEBAUTHN",
            },
            JWT_SECRET,
            "HS512"
          )

          resolve(certificate)
        } catch (e) {
          console.log(e)
          reject(e.message)
        }
      }
    },
    changeAuthenticationSettings(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )

          const webauthnCount = await WebauthnKey.count({
            where: { userId: userFound.id },
          })

          if (
            args.primaryAuthenticationMethods.length === 0 &&
            args.secondaryAuthenticationMethods.length !== 0
          ) {
            reject("Cannot set only secondary authentication methods")
          } else if (
            (args.primaryAuthenticationMethods.indexOf("PASSWORD") !== -1 ||
              args.secondaryAuthenticationMethods.indexOf("PASSWORD") !== -1) &&
            userFound.password === null
          ) {
            reject(
              "You cannot use password as authentication method as it is not set"
            )
          } else if (
            (args.primaryAuthenticationMethods.indexOf("WEBAUTHN") !== -1 ||
              args.secondaryAuthenticationMethods.indexOf("WEBAUTHN") !== -1) &&
            webauthnCount === 0
          ) {
            reject(
              "You cannot use WebAuthn as authentication method as you have no registered device"
            )
          } else {
            for (let method of args.primaryAuthenticationMethods) {
              if (args.secondaryAuthenticationMethods.indexOf(method) !== -1) {
                reject(
                  "Cannot use the same authentication method both as primary and secondary"
                )
                return
              }
            }
            const newUser = await userFound.update(args)

            resolve(newUser)

            pubsub.publish("userUpdated", {
              userUpdated: newUser,
              userId: newUser.id,
            })
          }
        },
        ["CHANGE_AUTHENTICATION"]
      )
    },
    createToken(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          // decode the certificates passed
          const validatedCertificates = validateCertificates(
            args.passwordCertificate,
            args.webAuthnCertificate,
            args.totpCertificate,
            args.emailCertificate,
            reject,
            JWT_SECRET
          )

          if (!validatedCertificates) {
            return
          }

          let {
            userId,
            decodedPasswordCertificate,
            decodedWebAuthnCertificate,
            decodedTotpCertificate,
            decodedEmailCertificate,
          } = validatedCertificates

          if (userId !== context.auth.userId) {
            reject("Certificate user not matching authenticated user")
            return
          }

          if (decodedEmailCertificate) {
            resolve(
              jwt.encode(
                {
                  userId: userId,
                  tokenType: args.tokenType,
                  exp: moment()
                    .utc()
                    .add({ minutes: 15 })
                    .unix(),
                },
                JWT_SECRET,
                "HS512"
              )
            )
          }

          const userFound = await context.dataLoaders.userLoaderById.load(
            userId
          )
          if (!userFound) {
            reject("User doesn't exist. Use `signUp` to create one")
          } else {
            const {
              primaryAuthenticationMethods,
              secondaryAuthenticationMethods,
            } = userFound

            let enabledFactorPassed = false
            for (let authenticationMethod of [
              ...primaryAuthenticationMethods,
              ...secondaryAuthenticationMethods,
            ]) {
              enabledFactorPassed =
                enabledFactorPassed ||
                checkAuthenticationMethod(
                  authenticationMethod,
                  decodedPasswordCertificate,
                  decodedWebAuthnCertificate,
                  decodedTotpCertificate
                )
            }

            if (!enabledFactorPassed) {
              reject("Did not pass any enabled authentication method")
              return
            }

            resolve(
              jwt.encode(
                {
                  userId: userId,
                  tokenType: args.tokenType,
                  exp: moment()
                    .utc()
                    .add({ minutes: 15 })
                    .unix(),
                },
                JWT_SECRET,
                "HS512"
              )
            )
          }
        },
        ["TEMPORARY"]
      )
    },
    setWebAuthn(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          let clientAttestationResponse
          try {
            clientAttestationResponse = JSON.parse(args.challengeResponse)
          } catch (e) {
            if (e instanceof SyntaxError) {
              reject("Invaild JSON passed for challengeResponse")
              return
            } else {
              reject("Internal error")
            }
          }

          clientAttestationResponse.rawId = new Int8Array(
            clientAttestationResponse.rawId
          ).buffer
          clientAttestationResponse.response.attestationObject = new Int8Array(
            clientAttestationResponse.response.attestationObject
          ).buffer
          clientAttestationResponse.response.clientDataJSON = new Int8Array(
            clientAttestationResponse.response.clientDataJSON
          ).buffer

          let decodedJwt
          try {
            decodedJwt = jwt.decode(args.jwtChallenge, process.env.JWT_SECRET)
          } catch (e) {
            reject("Invalid or expired jwtChallenge")
            return
          }
          const decoded = decodedJwt.challenge

          var attestationExpectations = {
            challenge: str2ab(decoded),
            origin: "https://aurora.igloo.ooo",
            factor: "either",
          }

          let regResult
          try {
            regResult = await f2l.attestationResult(
              clientAttestationResponse,
              attestationExpectations
            )
          } catch (e) {
            reject("Invalid challengeResponse")
            return
          }

          const publicKey = regResult.authnrData.get("credentialPublicKeyPem")
          const credId = ab2str(regResult.authnrData.get("credId"))
          const counter = regResult.authnrData.get("counter")

          await WebauthnKey.create({
            userId: context.auth.userId,
            publicKey,
            credId,
            counter,
          })

          context.auth = {
            userId: context.auth.userId,
            accessLevel: "OWNER",
            tokenType: "TEMPORARY",
          }

          resolve({
            token: generateAuthenticationToken(context.auth.userId, JWT_SECRET),
            user: {
              id: context.auth.userId,
            },
          })
        },
        ["CHANGE_AUTHENTICATION"]
      )
    },
    createPermanentToken(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          if (args.name === "") {
            reject("Empty name is not allowed")
          } else {
            const databaseToken = await PermanentToken.create({
              name: args.name,
              userId: context.auth.userId,
            })

            resolve({
              id: databaseToken.id,
              token: generatePermanentAuthenticationToken(
                context.auth.userId,
                databaseToken.id,
                "DEVICE",
                JWT_SECRET
              ),
            })

            const resolveObj = {
              id: databaseToken.id,
              name: databaseToken.name,
              user: { id: context.auth.userId },
            }
            pubsub.publish("permanentTokenCreated", {
              permanentTokenCreated: resolveObj,
              userId: context.auth.userId,
            })

            const userFound = await context.dataLoaders.userLoaderById.load(
              context.auth.userId
            )

            if (userFound.settings_permanentTokenCreatedEmail) {
              sendTokenCreatedEmail(userFound.email)
            }
          }
        },
        ["MANAGE_PERMANENT_TOKENS"]
      )
    },
    regeneratePermanentToken(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const databaseToken = await context.dataLoaders.permanentTokenLoaderById.load(
            args.id
          )

          if (!databaseToken) {
            reject("This token doesn't exist")
          } else if (databaseToken.userId !== context.auth.userId) {
            reject("This token is not yours")
          } else {
            const regeneratedToken = generatePermanentAuthenticationToken(
              context.auth.userId,
              databaseToken.id,
              "DEVICE",
              JWT_SECRET
            )
            resolve(regeneratedToken)
          }
        },
        ["MANAGE_PERMANENT_TOKENS"]
      )
    },
    deletePermanentToken(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const databaseToken = await context.dataLoaders.permanentTokenLoaderById.load(
            args.id
          )

          if (!databaseToken) {
            reject("This token doesn't exist")
          } else if (databaseToken.userId !== context.auth.userId) {
            reject("This token is not yours")
          } else {
            await databaseToken.destroy()

            resolve(args.id)
            pubsub.publish("permanentTokenDeleted", {
              permanentTokenDeleted: args.id,
              userId: context.auth.userId,
            })
          }
        },
        ["MANAGE_PERMANENT_TOKENS"]
      )
    },
    signUp(root, args, context) {
      return async (resolve, reject) => {
        if (!args.name) {
          reject("name required")
          return
        }

        const userFound = await User.find({ where: { email: args.email } })
        if (userFound) {
          reject("A user with this email already exists")
        } else {
          try {
            const newUser = await User.create({
              email: args.email,
              quietMode: false,
              devMode: true, // BETA: after beta the default should be false
              emailIsVerified: false,
              name: args.name,
              profileIconColor: randomUserIconColor(),
              settings_language: "en-GB",
              settings_lengthAndMass: "SI",
              settings_temperature: "CELSIUS",
              settings_dateFormat: "DMY",
              settings_timeFormat: "H24",
              settings_passwordChangeEmail: true,
              settings_pendingOwnerChangeReceivedEmail: true,
              settings_pendingEnvironmentShareReceivedEmail: true,
              settings_pendingOwnerChangeAcceptedEmail: true,
              settings_pendingEnvironmentShareAcceptedEmail: true,
              settings_permanentTokenCreatedEmail: true,
            })

            resolve({
              changeAuthenticationToken: jwt.encode(
                {
                  userId: newUser.id,
                  tokenType: "CHANGE_AUTHENTICATION",
                  exp: moment()
                    .utc()
                    .add({ minutes: 15 })
                    .unix(),
                },
                JWT_SECRET,
                "HS512"
              ),
              user: {
                id: newUser.id,
              },
            })

            sendVerificationEmail(args.email, newUser.id)
          } catch (e) {
            console.log(e)
            if (e.errors[0].validatorKey === "isEmail") {
              reject("Invalid email")
            } else {
              /* istanbul ignore next */
              throw e
            }
          }
        }
      }
    },
    setTotp(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )
          if (!userFound) {
            reject("User doesn't exist. Use `signUp` to create one")
          } else if (check2FCode(args.code, args.secret)) {
            await userFound.update({ twoFactorSecret: args.secret })
            resolve(true)
          } else {
            reject("Code and secret do not match")
          }
        },
        ["CHANGE_AUTHENTICATION"]
      )
    },
    // changes the password and returns an access token
    setPassword(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )
          if (!userFound) {
            reject("User doesn't exist. Use `signUp` to create one")
          } else {
            // check password strength
            const zxcvbnDictionary = [
              userFound.email,
              userFound.email.split("@")[0],
              userFound.name,
              "igloo",
              "igloo aurora",
              "aurora",
            ]

            if (zxcvbn(args.password, zxcvbnDictionary).score < 2) {
              reject(
                "Password too weak, avoid easily guessable password or short ones"
              )
              return
            }

            const encryptedPass = bcrypt.hashSync(args.password, SALT_ROUNDS)

            const sendUpdatedPasswordEmail = !!userFound.password
            const newUser = await userFound.update({
              password: encryptedPass,
            })

            context.auth = {
              userId: context.auth.userId,
              accessLevel: "OWNER",
              tokenType: "TEMPORARY",
            }

            resolve({
              token: generateAuthenticationToken(
                newUser.dataValues.id,
                JWT_SECRET
              ),
              user: {
                id: userFound.id,
              },
            })

            if (
              sendUpdatedPasswordEmail &&
              userFound.settings_passwordChangeEmail
            ) {
              sendPasswordUpdatedEmail(userFound.email)
            }
          }
        },
        ["CHANGE_AUTHENTICATION"]
      )
    },
    resendVerificationEmail(root, args, context) {
      return async (resolve, reject) => {
        const userFound = await User.find({ where: { email: args.email } })
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else if (userFound.emailIsVerified) {
          reject("This user has already verified their email")
        } else {
          resolve(true)
          sendVerificationEmail(userFound.email, userFound.id)
        }
      }
    },
    shareEnvironment: (root, args, context) =>
      authorized(
        args.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        3,
        async (resolve, reject, environmentFound, _, senderFound) => {
          if (!senderFound.emailIsVerified) {
            reject("Unverified user can not share environments")
            return
          }

          let receiverFound
          if (args.email) {
            receiverFound = await User.find({
              where: { email: args.email },
            })
          } else if (args.userId) {
            receiverFound = await User.find({
              where: { id: args.userId },
            })
          } else {
            reject("userId or email required")
          }

          if (!receiverFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (receiverFound.id === context.auth.userId) {
            reject("You can't share a resource with yourself")
          } else {
            const role = await instanceToRole(
              environmentFound,
              receiverFound,
              context
            )
            if (role !== null) {
              reject("This user already has a role on this environment")
              return
            }

            // if receiver has already a pending share throw error
            const otherPendingShare = await PendingEnvironmentShare.find({
              where: {
                receiverId: receiverFound.id,
                environmentId: args.environmentId,
              },
            })
            if (otherPendingShare) {
              reject(`There is already a pending environmentShare`)
              return
            }

            // if receiver has already an owner change throw error
            const otherOwnerChange = await PendingOwnerChange.find({
              where: {
                receiverId: receiverFound.id,
                environmentId: args.environmentId,
              },
            })
            if (otherOwnerChange) {
              reject(`There is already a pending ownerChange`)
              return
            }

            let newPendingShare = await PendingEnvironmentShare.create({
              senderId: senderFound.id,
              receiverId: receiverFound.id,
              environmentId: environmentFound.id,
              role: args.role,
            })

            resolve({
              id: newPendingShare.id,
              receiver: {
                id: newPendingShare.receiverId,
              },
              sender: {
                id: newPendingShare.senderId,
              },
              environment: {
                id: newPendingShare.environmentId,
              },
              role: newPendingShare.role,
            })

            touch(Environment, args.environmentId, newPendingShare.updatedAt)

            pubsub.publish("pendingEnvironmentShareReceived", {
              pendingEnvironmentShareReceived: newPendingShare,
              userId: receiverFound.id,
            })
            if (receiverFound.settings_pendingEnvironmentShareReceivedEmail) {
              sendEnvironmentSharedEmail(
                receiverFound.email,
                senderFound.name,
                environmentFound.name
              )
            }
            if (!receiverFound.quietMode && !environmentFound.muted) {
              sendPushNotification(
                [receiverFound.id],
                {
                  content: `The user ${
                    senderFound.name
                  } wants to share the environment "${
                    environmentFound.name
                  }" with you`,
                  date: new Date(),
                  type: "SHARE_RECEIVED_NOTIFICATION",
                },
                WebPushNotification
              )
            }

            const usersWithAccessIds = (await instanceToSharedIds(
              environmentFound,
              context
            )).filter(id => id !== receiverFound.id)

            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound,
              userIds: usersWithAccessIds,
            })
          }
        },
        environmentToParent
      ),
    pendingEnvironmentShare: (root, args, context) =>
      inheritAuthorized(
        args.id,
        context.dataLoaders.pendingEnvironmentShareLoaderById,
        User,
        pendingEnvironmentShare => pendingEnvironmentShare.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        3,
        async (
          resolve,
          reject,
          pendingEnvironmentShareFound,
          environmentFound
        ) => {
          const newPendingEnvironmentShare = await pendingEnvironmentShareFound.update(
            args
          )

          resolve(newPendingEnvironmentShare)

          touch(
            Environment,
            environmentFound.id,
            newPendingEnvironmentShare.updatedAt
          )

          pubsub.publish("pendingEnvironmentShareUpdated", {
            pendingEnvironmentShareUpdated: newPendingEnvironmentShare,
            userIds: [newPendingEnvironmentShare.receiverId],
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound,
            context
          )).filter(id => id !== newPendingEnvironmentShare.receiverId)

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })
        },
        environmentToParent
      ),
    acceptPendingEnvironmentShare: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingEnvironmentFound = await PendingEnvironmentShare.find({
          where: { id: args.pendingEnvironmentShareId },
        })

        if (!pendingEnvironmentFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingEnvironmentFound.receiverId) {
          reject("You are not the receiver of this environment share")
        } else {
          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            pendingEnvironmentFound.environmentId
          )
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )

          if (!userFound.emailIsVerified) {
            reject("Unverified users can not accept environment share")
            return
          }

          if (pendingEnvironmentFound.role === "ADMIN") {
            await EnvironmentAdmin.create({
              userId: userFound.id,
              environmentId: environmentFound.id,
            })
          } else if (pendingEnvironmentFound.role === "EDITOR") {
            await EnvironmentEditor.create({
              userId: userFound.id,
              environmentId: environmentFound.id,
            })
          } else {
            await EnvironmentSpectator.create({
              userId: userFound.id,
              environmentId: environmentFound.id,
            })
          }

          const payload = {
            id: pendingEnvironmentFound.id,
            environment: { id: pendingEnvironmentFound.environmentId },
            receiver: { id: pendingEnvironmentFound.receiverId },
            sender: { id: pendingEnvironmentFound.senderId },
            role: pendingEnvironmentFound.role,
          }
          resolve(payload)

          touch(Environment, environmentFound.id)

          pubsub.publish("pendingEnvironmentShareAccepted", {
            pendingEnvironmentShareAccepted: payload,
            userId: userFound.id,
          })

          const senderFound = await context.dataLoaders.userLoaderById.load(
            pendingEnvironmentFound.senderId
          )
          if (senderFound.settings_pendingEnvironmentShareAcceptedEmail) {
            sendEnvironmentShareAcceptedEmail(
              senderFound.email,
              userFound.name,
              environmentFound.name
            )
          }
          if (!senderFound.quietMode && !environmentFound.muted) {
            sendPushNotification(
              [senderFound.id],
              {
                content: `The user ${
                  userFound.name
                } accepted the environment "${environmentFound.name}"`,
                date: new Date(),
                type: "SHARE_ACCEPTED_NOTIFICATION",
              },
              WebPushNotification
            )
          }
          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound,
            context
          )).filter(id => id !== context.auth.userId)

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })

          await pendingEnvironmentFound.destroy()
        }
      }),
    declinePendingEnvironmentShare: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingEnvironmentFound = await PendingEnvironmentShare.find({
          where: { id: args.pendingEnvironmentShareId },
        })

        if (!pendingEnvironmentFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingEnvironmentFound.receiverId) {
          reject("You are not the receiver of this environment share")
        } else {
          const pendingEnvironmentFoundId = pendingEnvironmentFound.id
          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            pendingEnvironmentFound.environmentId
          )

          await pendingEnvironmentFound.destroy()

          resolve(pendingEnvironmentFoundId)

          touch(Environment, environmentFound.id)

          pubsub.publish("pendingEnvironmentShareDeclined", {
            pendingEnvironmentShareDeclined: pendingEnvironmentFoundId,
            userId: context.auth.userId,
          })

          const usersWithAccessIds = await instanceToSharedIds(
            environmentFound,
            context
          )

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })
        }
      }),
    revokePendingEnvironmentShare: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingEnvironmentFound = await PendingEnvironmentShare.find({
          where: { id: args.pendingEnvironmentShareId },
        })

        if (!pendingEnvironmentFound) {
          reject("The requested resource does not exist")
        } else {
          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            pendingEnvironmentFound.environmentId
          )
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )

          if (
            (await authorizationLevel(environmentFound, userFound, context)) < 3
          ) {
            reject("You are not authorized to perform this operation")
          } else {
            const revokedId = pendingEnvironmentFound.id
            const receiverId = pendingEnvironmentFound.receiverId
            await pendingEnvironmentFound.destroy()

            resolve(revokedId)

            touch(Environment, environmentFound.id)

            const usersWithAccessIds = await instanceToSharedIds(
              environmentFound,
              context
            )

            pubsub.publish("pendingEnvironmentShareRevoked", {
              pendingEnvironmentShareRevoked: revokedId,
              userIds: [receiverId],
            })
            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound,
              userIds: usersWithAccessIds,
            })
          }
        }
      }),
    changeOwner: (root, args, context) =>
      authorized(
        args.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        4,
        async (resolve, reject, environmentFound, _, senderFound) => {
          let receiverFound
          if (args.email) {
            receiverFound = await User.find({
              where: { email: args.email },
            })
          } else if (args.userId) {
            receiverFound = await User.find({
              where: { id: args.userId },
            })
          } else {
            reject("userId or email required")
          }

          if (!receiverFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (receiverFound.id === context.auth.userId) {
            reject("You already are the owner of this environment")
          } else {
            // if the environment already has a pending owner change remove it
            const otherOwnerChange = await PendingOwnerChange.find({
              where: {
                environmentId: args.environmentId,
              },
            })
            if (otherOwnerChange) {
              await otherOwnerChange.destroy()
            }

            const otherPendingShare = await PendingEnvironmentShare.find({
              where: {
                receiverId: receiverFound.id,
                environmentId: args.environmentId,
              },
            })
            if (otherPendingShare) {
              await otherPendingShare.destroy()

              pubsub.publish("pendingEnvironmentShareRevoked", {
                pendingEnvironmentShareRevoked: otherPendingShare.id,
                userIds: [receiverFound.id],
              })
            }

            let newOwnerChange = await PendingOwnerChange.create({
              senderId: senderFound.id,
              receiverId: receiverFound.id,
              environmentId: environmentFound.id,
            })

            resolve({
              id: newOwnerChange.id,
              sender: {
                id: newOwnerChange.senderId,
              },
              receiver: {
                id: newOwnerChange.receiverId,
              },
              environment: {
                id: newOwnerChange.environmentId,
              },
            })

            touch(Environment, args.environmentId, newOwnerChange.updatedAt)

            pubsub.publish("pendingOwnerChangeReceived", {
              pendingOwnerChangeReceived: newOwnerChange,
              userId: receiverFound.id,
            })
            if (receiverFound.settings_pendingOwnerChangeReceivedEmail) {
              sendOwnerChangeEmail(
                receiverFound.email,
                senderFound.name,
                environmentFound.name
              )
            }
            if (!receiverFound.quietMode && !environmentFound.muted) {
              sendPushNotification(
                [receiverFound.id],
                {
                  content: `The user ${
                    senderFound.name
                  } wants to make you the new owner of the environment "${
                    environmentFound.name
                  }"`,
                  date: new Date(),
                  type: "CHANGE_OWNER_RECEIVED_NOTIFICATION",
                },
                WebPushNotification
              )
            }

            const usersWithAccessIds = (await instanceToSharedIds(
              environmentFound,
              context
            )).filter(id => id !== receiverFound.id)

            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound,
              userIds: usersWithAccessIds,
            })
          }
        },
        environmentToParent
      ),
    revokePendingOwnerChange: (root, args, context) =>
      inheritAuthorized(
        args.pendingOwnerChangeId,
        context.dataLoaders.pendingOwnerChangeLoaderById,
        User,
        pendingEnvironmentShare => pendingEnvironmentShare.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        3,
        async (resolve, reject, pendingOwnerChangeFound, environmentFound) => {
          const targetUserId = pendingOwnerChangeFound.receiverId
          await pendingOwnerChangeFound.destroy()

          resolve(args.pendingOwnerChangeId)

          touch(Environment, environmentFound.id)

          pubsub.publish("pendingOwnerChangeRevoked", {
            pendingOwnerChangeRevoked: args.pendingOwnerChangeId,
            userId: targetUserId,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound,
            context
          )).filter(id => id !== targetUserId)

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })
        },
        environmentToParent
      ),
    acceptPendingOwnerChange: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingOwnerChangeFound = await PendingOwnerChange.find({
          where: { id: args.pendingOwnerChangeId },
        })

        if (!pendingOwnerChangeFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingOwnerChangeFound.receiverId) {
          reject("You are not the receiver of this owner change")
        } else {
          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            pendingOwnerChangeFound.environmentId
          )
          const userFound = await context.dataLoaders.userLoaderById.load(
            pendingOwnerChangeFound.receiverId
          )

          // remove old roles
          await runInParallel(
            () =>
              EnvironmentAdmin.destroy({
                where: {
                  userId: userFound.id,
                  environmentId: environmentFound.id,
                },
              }),
            () =>
              EnvironmentEditor.destroy({
                where: {
                  userId: userFound.id,
                  environmentId: environmentFound.id,
                },
              }),
            () =>
              EnvironmentSpectator.destroy({
                where: {
                  userId: userFound.id,
                  environmentId: environmentFound.id,
                },
              })
          )

          await environmentFound.update({ ownerId: userFound.id })

          const oldOwnerFound = await await context.dataLoaders.userLoaderById.load(
            pendingOwnerChangeFound.senderId
          )

          await EnvironmentAdmin.create({
            userId: oldOwnerFound.id,
            environmentId: environmentFound.id,
          })

          const payload = {
            id: pendingOwnerChangeFound.id,
            environment: { id: pendingOwnerChangeFound.environmentId },
            receiver: { id: pendingOwnerChangeFound.receiverId },
            sender: { id: pendingOwnerChangeFound.senderId },
          }
          resolve(payload)

          touch(Environment, environmentFound.id)

          await pendingOwnerChangeFound.destroy()

          pubsub.publish("pendingOwnerChangeAccepted", {
            pendingOwnerChangeAccepted: payload,
            userId: userFound.id,
          })

          const senderFound = await context.dataLoaders.userLoaderById.load(
            pendingOwnerChangeFound.senderId
          )
          if (senderFound.settings_pendingOwnerChangeAcceptedEmail) {
            sendOwnerChangeAcceptedEmail(
              senderFound.email,
              userFound.name,
              environmentFound.name
            )
          }
          if (!senderFound.quietMode && !environmentFound.muted) {
            sendPushNotification(
              [senderFound.id],
              {
                content: `The user ${
                  userFound.name
                } accepted the environment "${environmentFound.name}"`,
                date: new Date(),
                type: "OWNER_CHANGE_ACCEPTED_NOTIFICATION",
              },
              WebPushNotification
            )
          }

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound,
            context
          )).filter(id => id !== context.auth.userId)

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })
        }
      }),
    declinePendingOwnerChange: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingOwnerChangeFound = await PendingOwnerChange.find({
          where: { id: args.pendingOwnerChangeId },
        })

        if (!pendingOwnerChangeFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingOwnerChangeFound.receiverId) {
          reject("You are not the receiver of this owner change")
        } else {
          const targetUserId = pendingOwnerChangeFound.receiverId
          await pendingOwnerChangeFound.destroy()

          resolve(args.pendingOwnerChangeId)

          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            pendingOwnerChangeFound.environmentId
          )
          touch(Environment, args.environmentId)

          pubsub.publish("pendingOwnerChangeDeclined", {
            pendingOwnerChangeDeclined: args.pendingOwnerChangeId,
            userId: targetUserId,
          })

          // sending environmentUpdated subscription also to the target user
          // for ease of handling on the client side
          const usersWithAccessIds = await instanceToSharedIds(
            environmentFound,
            context
          )
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })
        }
      }),
    changeRole(root, args, context) {
      return authorized(
        args.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        3,
        async (resolve, reject, environmentFound, _, userFound) => {
          const targetUserFound = await User.find({
            where: { email: args.email },
          })
          const currentRole = await instanceToRole(
            environmentFound,
            targetUserFound,
            context
          )

          if (!targetUserFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (currentRole === "OWNER") {
            reject("You cannot change the role of the owner")
          } else if (!currentRole) {
            reject(
              "This user doesn't have a role on this environment you should use the `shareEnvironment` mutation"
            )
          } else {
            // remove old role
            await runInParallel(
              () =>
                EnvironmentAdmin.destroy({
                  where: {
                    userId: targetUserFound.id,
                    environmentId: environmentFound.id,
                  },
                }),
              () =>
                EnvironmentEditor.destroy({
                  where: {
                    userId: targetUserFound.id,
                    environmentId: environmentFound.id,
                  },
                }),
              () =>
                EnvironmentSpectator.destroy({
                  where: {
                    userId: targetUserFound.id,
                    environmentId: environmentFound.id,
                  },
                })
            )

            if (args.newRole === "ADMIN") {
              await EnvironmentAdmin.create({
                userId: targetUserFound.id,
                environmentId: environmentFound.id,
              })
            } else if (args.newRole === "EDITOR") {
              await EnvironmentEditor.create({
                userId: targetUserFound.id,
                environmentId: environmentFound.id,
              })
            } else {
              await EnvironmentSpectator.create({
                userId: targetUserFound.id,
                environmentId: environmentFound.id,
              })
            }

            resolve(environmentFound)

            touch(Environment, args.environmentId)

            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound,
              userIds: await instanceToSharedIds(environmentFound, context),
            })
          }
        },
        environmentToParent
      )
    },
    leaveEnvironment(root, args, context) {
      return authorized(
        args.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        1,
        async (resolve, reject, environmentFound, _, userFound) => {
          if (
            (await instanceToRole(environmentFound, userFound, context)) ===
            "OWNER"
          ) {
            reject("You cannot leave an environment that you own")
            return
          }

          await runInParallel(
            () =>
              EnvironmentAdmin.destroy({
                where: {
                  userId: userFound.id,
                  environmentId: environmentFound.id,
                },
              }),
            () =>
              EnvironmentEditor.destroy({
                where: {
                  userId: userFound.id,
                  environmentId: environmentFound.id,
                },
              }),
            () =>
              EnvironmentSpectator.destroy({
                where: {
                  userId: userFound.id,
                  environmentId: environmentFound.id,
                },
              })
          )
          resolve(environmentFound.id)

          touch(Environment, args.environmentId)

          pubsub.publish("environmentStoppedSharingWithYou", {
            environmentUpdated: environmentFound.id,
            userId: userFound.id,
          })

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
        },
        environmentToParent
      )
    },
    stopSharingEnvironment: (root, args, context) =>
      authorized(
        args.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        3,
        async (resolve, reject, environmentFound) => {
          let userFound
          if (args.email) {
            userFound = await User.find({
              where: { email: args.email },
            })
          } else if (args.userId) {
            userFound = await User.find({
              where: { id: args.userId },
            })
          } else {
            reject("userId or email required")
          }

          if (!userFound) {
            reject("This user doesn't exist, check that the email is correct")
            return
          }

          const role = await instanceToRole(
            environmentFound,
            userFound,
            context
          )

          if (!role) {
            reject("This resource isn't shared with that user")
          } else if (userFound.id === context.auth.userId) {
            reject(
              "You cannot stopSharing the environment with youself, use the `leaveEnvironment` mutation instead"
            )
          } else if (role === "OWNER") {
            reject("You cannot stop sharing a resource with its owner")
          } else {
            await runInParallel(
              () =>
                EnvironmentAdmin.destroy({
                  where: {
                    userId: userFound.id,
                    environmentId: environmentFound.id,
                  },
                }),
              () =>
                EnvironmentEditor.destroy({
                  where: {
                    userId: userFound.id,
                    environmentId: environmentFound.id,
                  },
                }),
              () =>
                EnvironmentSpectator.destroy({
                  where: {
                    userId: userFound.id,
                    environmentId: environmentFound.id,
                  },
                })
            )

            resolve(environmentFound)

            touch(Environment, args.environmentId)

            pubsub.publish("environmentStoppedSharingWithYou", {
              environmentStoppedSharingWithYou: args.environmentId,
              userId: userFound.id,
            })

            const usersWithAccessIds = await instanceToSharedIds(
              environmentFound,
              context
            )

            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound,
              userIds: usersWithAccessIds,
            })
          }
        },
        environmentToParent
      ),
    createEnvironment(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        if (args.name === "" || args.name === null) {
          reject("name cannot be null or an empty string")
          return
        }

        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        const environmentCount = await Environment.count({
          where: { ownerId: context.auth.userId },
        })
        if (environmentCount >= 100) {
          reject("Cannot have more than 100 environments")
          return
        }
        if (!userFound.emailIsVerified && environmentCount !== 0) {
          reject("Unverified users can only have one environment")
          return
        }

        const newEnvironment = await Environment.create({
          ...args,
          ownerId: userFound.id,
          picture: args.picture || randomEnvironmentPicture(),
          // if muted is not passed then set it to false
          muted: !!args.muted,
          index:
            args.index !== null && args.index !== undefined
              ? args.index
              : (await Environment.max("index", {
                  where: { ownerId: context.auth.userId },
                })) + 1 || 0, // or 0 replaces NaN when there are no other devices
        })

        await userFound.addOwnEnvironment(newEnvironment)
        await newEnvironment.setOwner(userFound)

        const resolveValue = {
          ...newEnvironment.dataValues,
          owner: { id: newEnvironment.ownerId },
          devices: [],
        }

        pubsub.publish("environmentCreated", {
          environmentCreated: resolveValue,
          userId: context.auth.userId,
        })

        resolve(resolveValue)
      })
    },
    createDevice(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        if (!userFound.devMode) {
          reject("Only dev users can create devices, set devMode to true")
          return
        }

        const newDevice = await Device.create({
          ...args,
          producerId: userFound.id,
          storageUsed: 0,
        })

        const id = newDevice.id

        resolve({
          id,
          jwtToken: generateDeviceAuthenticationToken(newDevice.id, JWT_SECRET),
          qrCode: new QRCode({ content: id }).svg(),
        })

        pubsub.publish("deviceCreated", {
          deviceCreated: {
            id,
          },
          userIds: [userFound.id],
        })
      })
    },
    claimDevice(root, args, context) {
      return async (resolve, reject) => {
        return authorized(
          args.environmentId,
          context,
          context.dataLoaders.environmentLoaderById,
          User,
          2,
          async (resolve, reject, environmentFound, _, userFound) => {
            if (args.name === "") {
              reject("Custom name cannot be an empty string")
              return
            } else if (args.muted === null) {
              reject("muted cannot be null")
              return
            }

            const index =
              args.index !== null && args.index !== undefined
                ? args.index
                : (await Device.max("index", {
                    where: { environmentId: args.environmentId },
                  })) + 1 || 0 // or 0 replaces NaN when there are no other devices

            const deviceCount = await Device.count({
              where: { environmentId: args.environmentId },
            })
            if (!userFound.emailIsVerified && deviceCount !== 0) {
              reject("Unverified users can only have one device")
              return
            }

            const deviceFound = await Device.find({
              where: { id: args.deviceId },
            })

            if (!deviceFound) {
              reject("The requested resource does not exist")
              return
            }

            if (deviceFound.environmentId) {
              const parentEnvironment = await context.dataLoaders.environmentLoaderById.load(
                deviceFound.environmentId
              )
              const parentUser = await context.dataLoaders.userLoaderById.load(
                parentEnvironment.ownerId
              )
              if (parentUser.emailIsVerified) {
                reject("This device has already been claimed")
                return
              }
            }

            const newDevice = await deviceFound.update({
              ...args,
              muted: !!args.muted,
              environmentId: args.environmentId,
              index,
            })

            const resolveValue = {
              ...newDevice.dataValues,
              environment: { id: newDevice.environmentId },
            }

            pubsub.publish("deviceClaimed", {
              deviceClaimed: resolveValue,
              userIds: [
                ...(await instanceToSharedIds(environmentFound, context)),
                newDevice.producerId,
              ],
              allowedDeviceIds: [newDevice.id],
            })

            resolve(resolveValue)

            touch(Environment, environmentFound.id, newDevice.createdAt)
          },
          environmentToParent
        )(resolve, reject)
      }
    },
    createFloatValue: CreateGenericValue(
      User,
      Device,
      Environment,
      FloatValue,
      "FloatValue",
      [FloatValue, StringValue, BooleanValue, PlotValue, CategoryPlotValue],
      pubsub,
      (args, reject) => {
        if (
          isNotNullNorUndefined(args.min) &&
          isNotNullNorUndefined(args.max) &&
          args.min >= args.max
        ) {
          reject("The min value should be less than the max value")
          return false
        } else if (isOutOfBoundaries(args.min, args.max, args.value)) {
          reject("Value is out of boundaries (min and max)")
          return false
        } else if (args.cardSize === "LARGE") {
          reject("float cannot have cardSize set to LARGE")
          return false
        } else if (args.unitOfMeasurement === "") {
          reject(
            "unitOfMeasurement cannot be an empty string, pass null instead"
          )
          return false
        }
        return true
      }
    ),
    createStringValue: CreateGenericValue(
      User,
      Device,
      Environment,
      StringValue,
      "StringValue",
      [FloatValue, StringValue, BooleanValue, PlotValue, CategoryPlotValue],
      pubsub,
      (args, reject) => {
        if (isNotNullNorUndefined(args.maxChars) && args.maxChars <= 0) {
          reject("maxChars must be greater than 0")
          return false
        } else if (
          isNotNullNorUndefined(args.allowedValues) &&
          args.allowedValues.length < 2
        ) {
          reject("allowedValues must contain at least 2 options")
          return false
        } else if (
          isNotNullNorUndefined(args.maxChars) &&
          args.value.length > args.maxChars
        ) {
          reject("Value exceeds the maxChars")
          return false
        } else if (
          isNotNullNorUndefined(args.allowedValues) &&
          args.allowedValues.indexOf(args.value) === -1
        ) {
          reject("Value is not among the allowedValues")
          return false
        } else if (args.cardSize === "LARGE") {
          reject("stringValue cannot have cardSize set to LARGE")
          return false
        }
        return true
      }
    ),
    createBooleanValue: CreateGenericValue(
      User,
      Device,
      Environment,
      BooleanValue,
      "BooleanValue",
      [FloatValue, StringValue, BooleanValue, PlotValue, CategoryPlotValue],
      pubsub,
      (args, reject) => {
        if (args.cardSize !== "NORMAL") {
          reject("booleanValue can have cardSize set only to NORMAL")
          return false
        }

        return true
      }
    ),
    createPlotValue: CreateGenericValue(
      User,
      Device,
      Environment,
      PlotValue,
      "PlotValue",
      [FloatValue, StringValue, BooleanValue, PlotValue, CategoryPlotValue],
      pubsub,
      (args, reject) => {
        if (
          isNotNullNorUndefined(args.min) &&
          isNotNullNorUndefined(args.max) &&
          args.min >= args.max
        ) {
          reject("The min value should be less than the max value")
          return false
        } else if (args.unitOfMeasurement === "") {
          reject(
            "unitOfMeasurement cannot be an empty string, pass null instead"
          )
          return false
        }
        return true
      }
    ),
    createCategoryPlotValue: CreateGenericValue(
      User,
      Device,
      Environment,
      CategoryPlotValue,
      "CategoryPlotValue",
      [FloatValue, StringValue, BooleanValue, PlotValue, CategoryPlotValue],
      pubsub,
      (args, reject) => {
        if (
          isNotNullNorUndefined(args.allowedValues) &&
          args.allowedValues.length === 0
        ) {
          reject("allowedValues cannot be an empty array")
          return false
        }

        return true
      }
    ),
    createPlotNode(root, args, context) {
      return deviceInheritAuthorized(
        args.plotId,
        context.dataLoaders.plotValueLoaderById,
        context,
        2,
        async (resolve, reject, plotValueFound, deviceFound) => {
          if (
            isOutOfBoundaries(
              plotValueFound.min,
              plotValueFound.max,
              args.value
            )
          ) {
            reject("Value out of boundaries")
            return
          }

          if (deviceFound.storageUsed >= MAX_STORAGE) {
            reject("Storage space fully used")
            return
          }

          const plotNode = await PlotNode.create({
            ...args,
            timestamp: args.timestamp || new Date(),
            deviceId: plotValueFound.deviceId,
            userId: context.auth.userId,
          })

          plotNode.setPlot(plotValueFound)
          plotValueFound.addPlotNode(plotNode)

          const resolveObj = {
            ...plotNode.dataValues,
            user: { id: plotNode.userId },
            device: { id: plotNode.deviceId },
            plot: { id: plotNode.plotId },
          }

          resolve(resolveObj)
          deviceFound.increment({ storageUsed: 1 })

          if (deviceFound.environmentId)
            touch(Environment, deviceFound.environmentId, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(PlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("plotNodeCreated", {
            plotNodeCreated: resolveObj,
            userIds: deviceFound.environmentId
              ? [
                  deviceFound.producerId,
                  ...(await instanceToSharedIds(
                    await context.dataLoaders.environmentLoaderById.load(
                      deviceFound.environmentId
                    ),
                    context
                  )),
                ]
              : [deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    createCategoryPlotNode(root, args, context) {
      return deviceInheritAuthorized(
        args.plotId,
        context.dataLoaders.categoryPlotValueLoaderById,
        context,
        2,
        async (resolve, reject, plotValueFound, deviceFound) => {
          if (
            isNotNullNorUndefined(plotValueFound.allowedValues) &&
            plotValueFound.allowedValues.indexOf(args.value)
          ) {
            reject("Value not in allowedValues")
            return
          }

          if (deviceFound.storageUsed >= MAX_STORAGE) {
            reject("Storage space fully used")
            return
          }

          const plotNode = await CategoryPlotNode.create({
            ...args,
            timestamp: args.timestamp || new Date(),
            deviceId: plotValueFound.deviceId,
            userId: context.auth.userId,
          })

          plotNode.setPlot(plotValueFound)
          plotValueFound.addCategoryPlotNode(plotNode)

          const resolveObj = {
            ...plotNode.dataValues,
            user: { id: plotNode.userId },
            device: { id: plotNode.deviceId },
            plot: { id: plotNode.plotId },
          }

          resolve(resolveObj)

          deviceFound.increment({ storageUsed: 1 })
          if (deviceFound.environmentId)
            touch(Environment, deviceFound.environmentId, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(CategoryPlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("categoryPlotNodeCreated", {
            categoryPlotNodeCreated: resolveObj,
            userIds: deviceFound.environmentId
              ? [
                  deviceFound.producerId,
                  ...(await instanceToSharedIds(
                    await context.dataLoaders.environmentLoaderById.load(
                      deviceFound.environmentId
                    ),
                    context
                  )),
                ]
              : [deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    user(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        if (args.name === null || args.name === "") {
          reject("name cannot be null or empty")
          return
        }

        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else {
          const newUser = await userFound.update(args)
          resolve(newUser.dataValues)

          pubsub.publish("userUpdated", {
            userUpdated: newUser.dataValues,
            userId: context.auth.userId,
          })

          if (
            args.hasOwnProperty("name") ||
            args.hasOwnProperty("profileIconColor")
          ) {
            const ownerChanges = await PendingOwnerChange.findAll({
              where: {
                [Op.or]: [
                  { senderId: userFound.id },
                  { receiverId: userFound.id },
                ],
              },
            })

            ownerChanges.map(ownerChange => {
              pubsub.publish("pendingOwnerChangeUpdated", {
                pendingOwnerChangeUpdated: ownerChange,
                userIds: [ownerChange.senderId, ownerChange.receiverId],
              })
            })

            const environmentShares = await PendingEnvironmentShare.findAll({
              where: {
                [Op.or]: [
                  { senderId: userFound.id },
                  { receiverId: userFound.id },
                ],
              },
            })

            environmentShares.map(environmentShare => {
              pubsub.publish("pendingEnvironmentShareUpdated", {
                pendingEnvironmentShareUpdated: environmentShare,
                userIds: [
                  environmentShare.senderId,
                  environmentShare.receiverId,
                ],
              })
            })
          }
        }
      })
    },
    changeEmail(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const sameEmailUserFound = await User.find({
            where: { email: args.newEmail },
          })
          if (sameEmailUserFound) {
            reject("A user with this email already exists")
            return
          }

          try {
            sendVerificationEmail(args.newEmail, context.auth.userId)
            resolve(true)
          } catch (e) {
            console.log(e)
            if (e.errors[0].validatorKey === "isEmail") {
              reject("Invalid email")
            } else {
              /* istanbul ignore next */
              throw e
            }
          }
        },
        ["CHANGE_EMAIL"]
      )
    },
    settings(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else if (
          args.language === null ||
          args.lengthAndMass === null ||
          args.temperature === null ||
          args.dateFormat === null ||
          args.timeFormat === null
        ) {
          reject("You passed null for a parameter that doesn't accept null")
        } else {
          const updateQuery = {}
          const fields = [
            "language",
            "lengthAndMass",
            "temperature",
            "dateFormat",
            "timeFormat",
            "passwordChangeEmail",
            "pendingOwnerChangeReceivedEmail",
            "pendingEnvironmentShareReceivedEmail",
            "pendingOwnerChangeAcceptedEmail",
            "pendingEnvironmentShareAcceptedEmail",
            "permanentTokenCreatedEmail",
          ]

          fields.forEach(field => {
            if (isNotNullNorUndefined(args[field])) {
              updateQuery[`settings_${field}`] = args[field]
            }
          })

          const newUser = await userFound.update(updateQuery)

          resolve({
            language: newUser.settings_language,
            lengthAndMass: newUser.settings_lengthAndMass,
            temperature: newUser.settings_temperature,
            dateFormat: newUser.settings_dateFormat,
            timeFormat: newUser.settings_timeFormat,
            passwordChangeEmail: newUser.settings_passwordChangeEmail,
            pendingOwnerChangeReceivedEmail:
              newUser.settings_pendingOwnerChangeReceivedEmail,
            pendingEnvironmentShareReceivedEmail:
              newUser.settings_pendingEnvironmentShareReceivedEmail,
            pendingOwnerChangeAcceptedEmail:
              newUser.settings_pendingOwnerChangeAcceptedEmail,
            pendingEnvironmentShareAcceptedEmail:
              newUser.settings_pendingEnvironmentShareAcceptedEmail,
            permanentTokenCreatedEmail:
              newUser.settings_permanentTokenCreatedEmail,
          })

          pubsub.publish("userUpdated", {
            userUpdated: newUser.dataValues,
            userId: context.auth.userId,
          })
        }
      })
    },
    updatePaymentInfo(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else if (userFound.stripeCustomerId) {
          // replaces customer payment method
          await stripe.customers.createSource(userFound.stripeCustomerId, {
            source: args.stripeToken,
          })

          resolve(true)
        } else {
          // create a new customer and attaches
          const customer = await stripe.customers.create({
            email: userFound.email,
            source: args.stripeToken,
          })

          await userFound.update({
            stripeCustomerId: customer.id,
          })

          resolve(true)
        }
      })
    },
    environment(root, args, context) {
      return authorized(
        args.id,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        2,
        async (resolve, reject, environmentFound, _, userFound) => {
          if (args.name === "" || args.name === null) {
            reject("name cannot be null or an empty string")
            return
          } else if (userFound.quietMode && isNotNullNorUndefined(args.muted)) {
            reject(
              "Cannot change muted at environment level when quietMode is enabled at user level"
            )
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          }

          const newEnvironment = await environmentFound.update(args)

          resolve(newEnvironment.dataValues)
          pubsub.publish("environmentUpdated", {
            environmentUpdated: newEnvironment.dataValues,
            userIds: await instanceToSharedIds(environmentFound, context),
          })

          const pendingEnvironmentSharesFound = await PendingEnvironmentShare.findAll(
            { where: { environmentId: newEnvironment.id } }
          )

          pendingEnvironmentSharesFound.map(environmentShare => {
            pubsub.publish("pendingEnvironmentShareUpdated", {
              pendingEnvironmentShareUpdated: environmentShare,
              userIds: [environmentShare.senderId, environmentShare.receiverId],
            })
          })

          const ownerChanges = await PendingOwnerChange.findAll({
            where: {
              environmentId: newEnvironment.id,
            },
          })

          ownerChanges.map(ownerChange => {
            pubsub.publish("pendingOwnerChangeUpdated", {
              pendingOwnerChangeUpdated: ownerChange,
              userIds: [ownerChange.senderId, ownerChange.receiverId],
            })
          })
        },
        environmentToParent
      )
    },
    device(root, args, context) {
      return deviceAuthorized(
        args.id,
        context,
        1,
        async (resolve, reject, deviceFound, userFound) => {
          const environmentFound = deviceFound.environmentId
            ? await context.dataLoaders.environmentLoaderById.load(
                deviceFound.environmentId
              )
            : null
          const myRole =
            environmentFound && userFound
              ? await instanceToRole(environmentFound, userFound, context)
              : null
          // users with read only access can only star a device
          if (
            myRole == "SPECTATOR" &&
            (Object.keys(args).length > 2 ||
              !isNotNullNorUndefined(args.starred))
          ) {
            reject("You are not allowed to mutate fields other than starred")
            return
          }

          // runs sanity checks on the args
          if (
            isNotNullNorUndefined(args.batteryStatus) &&
            isOutOfBoundaries(0, 100, args.batteryStatus)
          ) {
            reject("batteryStatus is out of boundaries [0,100]")
            return
          } else if (
            isNotNullNorUndefined(args.signalStatus) &&
            isOutOfBoundaries(0, 100, args.signalStatus)
          ) {
            reject("signalStatus is out of boundaries [0,100]")
            return
          } else if (args.name === null || args.name === "") {
            reject("name cannot be null or an empty string")
            return
          } else if (args.muted === null) {
            reject("muted cannot be null")
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          } else if (
            ((environmentFound && environmentFound.muted) ||
              userFound.quietMode) &&
            isNotNullNorUndefined(args.muted)
          ) {
            reject(
              "Cannot change muted at device level when it is enabled at environment level or quietMode is enabled at user level"
            )
            return
          } else if (
            myRole === null &&
            (isNotNullNorUndefined(args.muted) ||
              isNotNullNorUndefined(args.starred))
          ) {
            reject("You are not authorized to change muted or starred value")
            return
          }

          if (
            (deviceFound.batteryStatus >= 10 ||
              deviceFound.batteryStatus === null) &&
            args.batteryStatus !== null &&
            args.batteryStatus < 10
          ) {
            resolvers.createNotification(
              {},
              {
                deviceId: deviceFound.id,
                content: "My battery is running low",
                date: new Date().toISOString(),
              },
              context
            )
          }

          // forbid starring more than MAX_STARRED devices
          if (
            deviceFound.starred.indexOf(context.auth.userId) === -1 &&
            args.starred === true
          ) {
            const starredCount = await Device.count({
              where: {
                environmentId: deviceFound.environmentId,
                starred: { [Op.contains]: [context.auth.userId] },
              },
            })

            const MAX_STARRED = 5
            if (starredCount >= MAX_STARRED) {
              reject(`Cannot have more than ${MAX_STARRED} starred devices`)
              return
            }
          }

          const updateQuery = args

          if (updateQuery.starred === true) {
            updateQuery.starred =
              deviceFound.starred.indexOf(context.auth.userId) === -1
                ? [...deviceFound.starred, context.auth.userId]
                : deviceFound.starred
          } else if (updateQuery.starred === false) {
            updateQuery.starred = deviceFound.starred.filter(
              id => id !== context.auth.userId
            )
          }

          const newDevice = await deviceFound.update(updateQuery)
          resolve(newDevice.dataValues)

          if (environmentFound)
            touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: environmentFound
              ? [
                  newDevice.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [newDevice.producerID],
            allowedDeviceIds: [newDevice.id],
          })
        }
      )
    },
    moveDevice(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        4,
        async (
          resolve,
          reject,
          deviceFound,
          [_, environmentFound],
          userFound
        ) => {
          if (args.newEnvironmentId === deviceFound.environmentId) {
            reject("The device already belongs to this environment")
            return
          }

          const targetEnvironment = await context.dataLoaders.environmentLoaderById.load(
            args.newEnvironmentId
          )

          const isOwnerOfTargetEnvironment =
            (await instanceToRole(targetEnvironment, userFound, context)) ===
            "OWNER"

          if (!isOwnerOfTargetEnvironment) {
            reject("You can only move devices to environments you own")
            return
          }

          const newDevice = await deviceFound.update({
            environmentId: args.newEnvironmentId,
          })

          const modelsToUpdate = [
            BooleanValue,
            FloatValue,
            StringValue,
            PlotValue,
            CategoryPlotValue,
            Notification,
          ]

          const updatePromises = modelsToUpdate.map(
            async Model =>
              await Model.update(
                { environmentId: newDevice.environmentId },
                { where: { deviceId: newDevice.id } }
              )
          )

          await Promise.all(updatePromises)

          resolve(newDevice.dataValues)

          touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceMoved", {
            deviceMoved: newDevice.dataValues,
            userIds: [
              ...(await instanceToSharedIds(environmentFound, context)),
              ...(await instanceToSharedIds(targetEnvironment, context)),
            ],
          })
        },
        deviceToParent
      )
    },
    resetOnlineState(root, args, context) {
      let id
      if (!args.deviceId && context.auth.tokenType !== "DEVICE_ACCESS") {
        reject("You need to pass a deviceId")
      } else if (!args.deviceId) {
        id = context.auth.deviceId
      } else {
        id = args.deviceId
      }

      return deviceAuthorized(
        id,
        context,
        2,
        async (resolve, reject, deviceFound) => {
          const newDevice = await deviceFound.update({ online: null })
          resolve(newDevice.dataValues)

          const environmentFound =
            deviceFound.environmentId &&
            (await context.dataLoaders.environmentLoaderById.load(
              deviceFound.environmentId
            ))

          if (environmentFound)
            touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: environmentFound
              ? [
                  newDevice.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [newDevice.producerID],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    value(root, args, context) {
      return authorizedValue(
        args.id,
        context,
        {
          FloatValue,
          StringValue,
          BooleanValue,
          PlotValue,
          CategoryPlotValue,
        },
        async (resolve, reject, valueFound, deviceFound, environmentFound) => {
          const valueType = valueFound._modelOptions.name.singular
          if (args.name === null || args.name === "") {
            reject("name cannot be null or an empty string")
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          } else if (args.unitOfMeasurement === "") {
            reject(
              "unitOfMeasurement cannot be an empty string, pass null instead"
            )
            return
          } else if (valueType === "floatValue" && args.cardSize === "LARGE") {
            reject("FloatValue cannot have cardSize set to LARGE")
            return false
          }

          const newValue = await valueFound.update(args)
          resolve(newValue)

          if (environmentFound) {
            Environment.update(
              { updatedAt: newValue.updatedAt },
              { where: { id: environmentFound.id } }
            )
          }
          Device.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: valueFound.deviceId } }
          )

          pubsub.publish("valueUpdated", {
            valueUpdated: newValue,
            userIds: environmentFound
              ? [
                  deviceFound.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    floatValue: genericValueMutation(
      "floatValueLoaderById",
      "FloatValue",
      pubsub,
      User,
      Device,
      Environment,
      (args, valueFound, reject) => {
        const expectedNewValue = { ...valueFound.dataValues, ...args }

        if (
          isNotNullNorUndefined(expectedNewValue.min) &&
          isNotNullNorUndefined(expectedNewValue.max) &&
          expectedNewValue.min >= expectedNewValue.max
        ) {
          reject("The min value should be less than the max value")
          return false
        } else if (
          isOutOfBoundaries(
            expectedNewValue.min,
            expectedNewValue.max,
            expectedNewValue.value
          )
        ) {
          reject("value is out of boundaries")
          return false
        } else if (expectedNewValue.cardSize === "LARGE") {
          reject("FloatValue cannot have cardSize set to LARGE")
          return false
        } else if (args.unitOfMeasurement === "") {
          reject(
            "unitOfMeasurement cannot be an empty string, pass null instead"
          )
          return false
        }
        return true
      }
    ),
    stringValue: genericValueMutation(
      "stringValueLoaderById",
      "StringValue",
      pubsub,
      User,
      Device,
      Environment,
      (args, valueFound, reject) => {
        // Current or new value should respect maxChars and allowedValue
        const expectedNewValue = { ...valueFound.dataValues, ...args }

        if (isNotNullNorUndefined(args.maxChars) && args.maxChars <= 0) {
          reject("maxChars must be greater than 0")
          return false
        } else if (
          isNotNullNorUndefined(args.value) &&
          isNotNullNorUndefined(expectedNewValue.maxChars) &&
          args.value.length > expectedNewValue.maxChars
        ) {
          reject("The value provided exceeds the maxChars")
          return false
        } else if (args.cardSize === "LARGE") {
          reject("stringValue cannot have cardSize set to LARGE")
          return false
        } else if (
          isNotNullNorUndefined(args.value) &&
          isNotNullNorUndefined(expectedNewValue.allowedValues) &&
          expectedNewValue.allowedValues.indexOf(args.value) === -1
        ) {
          reject("The value is not among the allowedValues")
          return false
        } else if (
          isNotNullNorUndefined(args.allowedValues) &&
          args.allowedValues.length < 2
        ) {
          reject("allowedValues must contain at least 2 options")
          return false
        } else if (
          !isNotNullNorUndefined(args.value) &&
          isNotNullNorUndefined(args.allowedValues) &&
          args.allowedValues.indexOf(valueFound.value) === -1
        ) {
          reject("Current value is not among the allowedValues")
          return false
        } else if (
          !isNotNullNorUndefined(args.value) &&
          isNotNullNorUndefined(args.maxChars) &&
          valueFound.value.length > args.maxChars
        ) {
          reject("Current value exceeds maxChars")
          return false
        } else if (
          isNotNullNorUndefined(expectedNewValue.maxChars) &&
          isNotNullNorUndefined(expectedNewValue.allowedValues)
        ) {
          reject(
            "Cannot have maxChars and allowedValues set at the same time, use only one"
          )
          return false
        } else if (
          isNotNullNorUndefined(expectedNewValue.maxChars) &&
          expectedNewValue.permission === "READ_ONLY"
        ) {
          reject("Cannot set maxChars for a Read-Only value")
          return false
        }
        return true
      }
    ),
    booleanValue: genericValueMutation(
      "booleanValueLoaderById",
      "BooleanValue",
      pubsub,
      User,
      Device,
      Environment,
      (args, valueFound, reject) => {
        if (args.cardSize !== "NORMAL") {
          reject("booleanValue can have cardSize set only to NORMAL")
          return false
        }

        return true
      }
    ),
    plotValue: genericValueMutation(
      "plotValueLoaderById",
      "PlotValue",
      pubsub,
      User,
      Device,
      Environment,
      (args, valueFound, reject) => {
        if (args.unitOfMeasurement === "") {
          reject(
            "unitOfMeasurement cannot be an empty string, pass null instead"
          )
          return false
        }

        return true
      }
    ),
    categoryPlotValue: genericValueMutation(
      "categoryPlotValueLoaderById",
      "CategoryPlotValue",
      pubsub,
      User,
      Device,
      Environment
    ),
    atomicUpdateFloat: (root, args, context) =>
      deviceInheritAuthorized(
        args.id,
        context.dataLoaders.floatValueLoaderById,
        context,
        2,
        async (resolve, reject, valueFound, deviceFound) => {
          if (
            isOutOfBoundaries(
              valueFound.min,
              valueFound.max,
              valueFound.value + args.incrementBy
            )
          ) {
            reject("new value is out of boundaries")
            return
          }

          const newValue = await valueFound.update({
            value: valueFound.value + args.incrementBy,
          })
          const resolveObj = {
            ...newValue.dataValues,
            owner: { id: newValue.dataValues.userId },
            device: { id: newValue.dataValues.deviceId },
          }
          resolve(resolveObj)

          const environmentFound =
            deviceFound.environmentId &&
            (await context.dataLoaders.environmentLoaderById.load(
              deviceFound.environmentId
            ))

          if (environmentFound) {
            Environment.update(
              { updatedAt: newValue.updatedAt },
              { where: { id: environmentFound.id } }
            )
          }
          Device.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: newValue.deviceId } }
          )

          pubsub.publish("valueUpdated", {
            valueUpdated: { ...resolveObj, __resolveType: "FloatValue" },
            userIds: environmentFound
              ? [
                  deviceFound.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      ),
    plotNode(root, args, context) {
      return deviceInheritAuthorized(
        args.id,
        context.dataLoaders.plotNodeLoaderById,
        context,
        2,
        async (resolve, reject, plotNodeFound, deviceFound) => {
          const plotValueFound = await context.dataLoaders.plotValueLoaderById.load(
            plotNodeFound.plotId
          )
          if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          } else if (
            isOutOfBoundaries(
              plotValueFound.min,
              plotValueFound.max,
              args.value
            )
          ) {
            reject("value out of boundaries")
            return
          }

          const newNode = await plotNodeFound.update(args)

          const resolveObj = {
            ...newNode.dataValues,
            user: { id: newNode.dataValues.userId },
            device: { id: newNode.dataValues.deviceId },
            plot: { id: newNode.dataValues.plotId },
          }
          resolve(resolveObj)

          const environmentFound = deviceFound.environmentId
            ? await context.dataLoaders.environmentLoaderById.load(
                deviceFound.environmentId
              )
            : null

          if (environmentFound)
            touch(Environment, environmentFound.id, newNode.updatedAt)
          touch(Device, plotValueFound.deviceId, newNode.updatedAt)
          touch(PlotValue, plotValueFound.id, newNode.updatedAt)

          pubsub.publish("plotNodeUpdated", {
            plotNodeUpdated: resolveObj,
            userIds: environmentFound
              ? [
                  deviceFound.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    categoryPlotNode(root, args, context) {
      return deviceInheritAuthorized(
        args.id,
        context.dataLoaders.categoryPlotNodeLoaderById,
        context,
        2,
        async (resolve, reject, plotNodeFound, deviceFound) => {
          const plotValueFound = await context.dataLoaders.categoryPlotValueLoaderById.load(
            plotNodeFound.plotId
          )
          if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          } else if (
            isNotNullNorUndefined(plotValueFound.allowedValues) &&
            args.value !== undefined &&
            plotValueFound.allowedValues.indexOf(args.value)
          ) {
            reject("value not in allowedValues")
            return
          }
          const newNode = await plotNodeFound.update(args)

          const resolveObj = {
            ...newNode.dataValues,
            user: { id: newNode.dataValues.userId },
            device: { id: newNode.dataValues.deviceId },
            plot: { id: newNode.dataValues.plotId },
          }
          resolve(resolveObj)

          const environmentFound = deviceFound.environmentId
            ? await context.dataLoaders.environmentLoaderById.load(
                deviceFound.environmentId
              )
            : null

          if (environmentFound)
            touch(Environment, environmentFound.id, newNode.updatedAt)
          touch(Device, deviceFound.id, newNode.updatedAt)
          touch(CategoryPlotValue, plotNodeFound.plotId, newNode.updatedAt)

          pubsub.publish("categoryPlotNodeUpdated", {
            categoryPlotNodeUpdated: resolveObj,
            userIds: environmentFound
              ? [
                  deviceFound.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    createNotification(root, args, context) {
      return deviceAuthorized(
        args.deviceId,
        context,
        2,
        async (resolve, reject, deviceFound) => {
          if (!deviceFound.environmentId) {
            reject("Cannot create notification on unclaimed device")
            return
          } else if (args.content === "" || args.content === null) {
            reject("content cannot be null or an empty string")
            return
          } else if (deviceFound.storageUsed >= MAX_STORAGE) {
            reject("Storage space fully used")
            return
          }

          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            deviceFound.environmentId
          )

          const deviceSharedIds = [
            deviceFound.producerId,
            ...(await instanceToSharedIds(environmentFound, context)),
          ]

          const newNotification = await Notification.create({
            ...args,
            notRead: deviceSharedIds,
            date: args.date || new Date(),
          })

          resolve(newNotification)

          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )

          // remove old notifications when total is over MAX_NOTIFICATIONS
          const notificationCount = await Notification.count({
            where: { deviceId: args.deviceId },
          })
          const MAX_NOTIFICATIONS = 100
          if (notificationCount > MAX_NOTIFICATIONS) {
            const excessNotifications = await Notification.findAll({
              where: { deviceId: args.deviceId },
              offset: MAX_NOTIFICATIONS,
              order: [["createdAt", "DESC"]],
              fields: ["id"],
            })

            await Notification.destroy({
              where: {
                id: {
                  [Op.in]: excessNotifications.map(
                    notification => notification.id
                  ),
                },
              },
            })

            for (let notification of excessNotifications) {
              pubsub.publish("notificationDeleted", {
                notificationDeleted: notification.id,
                userIds: deviceSharedIds,
                allowedDeviceIds: [deviceFound.id],
                source: notification,
              })
            }
          }

          deviceFound.increment({ storageUsed: 1 })
          touch(Environment, environmentFound.id, newNotification.updatedAt)
          touch(Device, newNotification.deviceId, newNotification.updatedAt)

          pubsub.publish("notificationCreated", {
            notificationCreated: newNotification,
            userIds: deviceSharedIds,
            allowedDeviceIds: [deviceFound.id],
          })

          // the notificationCount props are updated so send the device and environment subscriptions
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: newNotification.deviceId,
            },
            userIds: deviceSharedIds,
            allowedDeviceIds: [deviceFound.id],
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: deviceSharedIds,
          })

          if (
            !userFound.quietMode &&
            !environmentFound.muted &&
            !deviceFound.muted
          ) {
            sendPushNotification(
              deviceSharedIds,
              {
                content: newNotification.content,
                date: newNotification.date,
                device: deviceFound,
                type: "DEVICE_NOTIFICATION",
              },
              WebPushNotification
            )
          }
        }
      )
    },
    notification(root, args, context) {
      return deviceInheritAuthorized(
        args.id,
        context.dataLoaders.notificationLoaderById,
        context,
        1,
        async (resolve, reject, notificationFound, deviceFound, userFound) => {
          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            deviceFound.environmentId
          )
          const role =
            environmentFound &&
            (await instanceToRole(environmentFound, userFound, context))
          if (args.content === "" || args.content === null) {
            reject("content cannot be null or an empty string")
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          } else if (
            role === "SPECTATOR" &&
            (Object.keys(args).length > 2 || !isNotNullNorUndefined(args.read))
          ) {
            reject("You are not allowed to mutate fields other than read")
            return
          } else if (role === null && isNotNullNorUndefined(args.read)) {
            reject("Producer cannot change read status")
            return
          }
          const updateQuery = args

          if (updateQuery.read === true) {
            updateQuery.notRead = notificationFound.notRead.filter(
              id => id !== context.auth.userId
            )
          } else if (updateQuery.read === false) {
            updateQuery.notRead =
              notificationFound.notRead.indexOf(context.auth.userId) === -1
                ? [...notificationFound.notRead, context.auth.userId]
                : notificationFound.notRead
          }

          const newNotification = await notificationFound.update(updateQuery)

          resolve(newNotification)

          touch(Environment, environmentFound.id, newNotification.updatedAt)
          touch(Device, newNotification.deviceId, newNotification.updatedAt)

          const deviceSharedIds = [
            deviceFound.producerId,
            ...(await instanceToSharedIds(environmentFound, context)),
          ]
          pubsub.publish("notificationUpdated", {
            notificationUpdated: newNotification,
            userIds: deviceSharedIds,
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    deleteNotification(root, args, context) {
      return deviceInheritAuthorized(
        args.id,
        context.dataLoaders.notificationLoaderById,
        context,
        2,
        async (resolve, reject, notificationFound, deviceFound) => {
          await notificationFound.destroy()

          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            deviceFound.environmentId
          )
          resolve(args.id)

          const environmentSharedUsers = await instanceToSharedIds(
            environmentFound,
            context
          )
          const deviceSharedIds = [
            deviceFound.producerId,
            ...environmentSharedUsers,
          ]
          pubsub.publish("notificationDeleted", {
            notificationDeleted: args.id,
            userIds: deviceSharedIds,
            allowedDeviceIds: [deviceFound.id],
            source: notificationFound,
          })

          // the notificationCount props are updated so send the
          // device and environment subscriptions and change updatedAt
          deviceFound.increment({ storageUsed: -1 })
          touch(Environment, environmentFound.id)
          touch(Device, deviceFound.id)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: deviceFound.id,
            },
            userIds: deviceSharedIds,
            allowedDeviceIds: [deviceFound.id],
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          for (let userId of environmentSharedUsers) {
            pubsub.publish("userUpdated", {
              userUpdated: { id: userId },
              userId: userId,
            })
          }
        }
      )
    },
    deleteValue: (root, args, context) =>
      authorizedValue(
        args.id,
        context,
        {
          FloatValue,
          StringValue,
          BooleanValue,
          PlotValue,
          CategoryPlotValue,
        },
        async (resolve, reject, valueFound, deviceFound, environmentFound) => {
          const authorizedUsersIds = environmentFound
            ? [
                deviceFound.producerId,
                ...(await instanceToSharedIds(environmentFound, context)),
              ]
            : [deviceFound.producerId]

          // TODO: if value is plot remove nodes
          await valueFound.destroy()

          pubsub.publish("valueDeleted", {
            valueDeleted: args.id,
            userIds: authorizedUsersIds,
            source: valueFound,
            allowedDeviceIds: [deviceFound.id],
          })
          resolve(args.id)

          deviceFound.increment({ storageUsed: -1 })
          if (environmentFound) touch(Environment, environmentFound.id)
          touch(Device, valueFound.deviceId)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: valueFound.deviceId,
            },
            userIds: authorizedUsersIds,
            allowedDeviceIds: [deviceFound.id],
          })
          if (environmentFound) {
            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound.dataValues,
              userIds: authorizedUsersIds,
            })
          }
        }
      ),
    unclaimDevice: (root, args, context) =>
      authorized(
        args.id,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        3,
        async (resolve, reject, deviceFound, [environmentFound], userFound) => {
          const { environmentId } = deviceFound

          await deviceFound.update({
            muted: null,
            environmentId: null,
            index: null,
            name: null,
          })

          resolve({
            id: deviceFound.id,
            qrCode: new QRCode({ content: deviceFound.id }).svg(),
          })

          const authorizedUsersIds = await instanceToSharedIds(
            environmentFound,
            context
          )

          const notificationsFound = await Notification.findAll({
            where: {
              deviceId: args.id,
            },
          })

          await Promise.all(
            notificationsFound.map(async notification => {
              await notification.destroy()
              pubsub.publish("notificationDeleted", {
                notificationDeleted: notification.id,
                userIds: authorizedUsersIds,
                allowedDeviceIds: [deviceFound.id],
                source: notification,
              })
            })
          )

          pubsub.publish("deviceUnclaimed", {
            deviceUnclaimed: args.id,
            userIds: [...authorizedUsersIds, deviceFound.producerId],
            allowedDeviceIds: [deviceFound.id],
            source: { environmentId },
          })

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: authorizedUsersIds,
          })
        },
        deviceToParent
      ),
    deleteDevice: (root, args, context) => async (resolve, reject) => {
      const deviceFound = await context.dataLoaders.deviceLoaderById.load(
        args.id
      )
      if (!deviceFound) {
        reject("The requested resource does not exist")
        return
      } else if (context.auth.userId !== deviceFound.producerId) {
        reject("Only the producer can delete a device")
        return
      }

      const environmentFound = deviceFound.environmentId
        ? await context.dataLoaders.environmentLoaderById.load(
            deviceFound.environmentId
          )
        : null
      const authorizedUsersIds = [
        ...(environmentFound
          ? await instanceToSharedIds(environmentFound, context)
          : []),
        deviceFound.producerId,
      ]

      const deleteChild = async ([Model, subscription]) => {
        const childrenFound = await Model.findAll({
          where: {
            deviceId: args.id,
          },
        })

        await Promise.all(
          childrenFound.map(async child => {
            await child.destroy()
            pubsub.publish(subscription, {
              [subscription]: child.id,
              userIds: authorizedUsersIds,
              source: child,
              allowedDeviceIds: [deviceFound.id],
            })
          })
        )
      }

      await Promise.all(
        [
          [FloatValue, "valueDeleted"],
          [StringValue, "valueDeleted"],
          [BooleanValue, "valueDeleted"],
          [PlotValue, "valueDeleted"],
          [CategoryPlotValue, "valueDeleted"],
          [PlotNode, "plotNodeDeleted"],
          [CategoryPlotNode, "categoryPlotNodeDeleted"],
          [Notification, "notificationDeleted"],
        ].map(deleteChild)
      )

      await deviceFound.destroy()

      pubsub.publish("deviceDeleted", {
        deviceDeleted: args.id,
        userIds: authorizedUsersIds,
        allowedDeviceIds: [deviceFound.id],
        source: deviceFound,
      })

      resolve(args.id)

      if (environmentFound) touch(Environment, environmentFound.id)
      pubsub.publish("environmentUpdated", {
        environmentUpdated: environmentFound.dataValues,
        userIds: authorizedUsersIds,
      })
    },
    deleteEnvironment: (root, args, context) =>
      authorized(
        args.id,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        3,
        async (
          resolve,
          reject,
          environmentFound,
          environmentAndParents,
          userFound
        ) => {
          const authorizedUsersIds = await instanceToSharedIds(
            environmentFound,
            context
          )

          const devices = await Device.findAll({
            where: { environmentId: environmentFound.id },
          })

          // TODO: send deleted notifications for children
          const unclaimDevicesPromises = devices.map(async device => {
            await device.update({
              muted: null,
              environmentId: null,
              index: null,
              name: null,
            })

            pubsub.publish("deviceUnclaimed", {
              deviceUnclaimed: device.id,
              userIds: [...authorizedUsersIds, device.producerId],
              allowedDeviceIds: [device.id],
            })
          })

          async function destroyPendingEnvironmentShare() {
            const pendingEnvironmentSharesFound = await PendingEnvironmentShare.findAll(
              {
                where: { environmentId: environmentFound.id },
              }
            )

            await Promise.all(
              pendingEnvironmentSharesFound.map(
                async pendingEnvironmentShare => {
                  pubsub.publish("pendingEnvironmentShareRevoked", {
                    pendingEnvironmentShareRevoked: pendingEnvironmentShare.id,
                    userIds: [pendingEnvironmentShare.receiverId],
                  })
                  await pendingEnvironmentShare.destroy()
                }
              )
            )
          }
          async function destroyPendingOwnerChange() {
            const ownerChangesFound = await PendingOwnerChange.findAll({
              where: { environmentId: environmentFound.id },
            })

            await Promise.all(
              ownerChangesFound.map(async ownerChange => {
                pubsub.publish("pendingOwnerChangeRevoked", {
                  pendingOwnerChangeRevoked: ownerChange.id,
                  userId: ownerChange.receiverId,
                })
                await ownerChange.destroy()
              })
            )
          }

          async function deleteShareJoinTables() {
            const joinTables = [
              EnvironmentAdmin,
              EnvironmentEditor,
              EnvironmentSpectator,
            ]

            await Promise.all(
              joinTables.map(model =>
                model.destroy({ where: { environmentId: args.id } })
              )
            )
          }

          await Promise.all([
            ...unclaimDevicesPromises,
            destroyPendingEnvironmentShare(),
            destroyPendingOwnerChange(),
            deleteShareJoinTables(),
          ])

          await environmentFound.destroy()

          pubsub.publish("environmentDeleted", {
            environmentDeleted: args.id,
            userIds: authorizedUsersIds,
          })
          resolve(args.id)
        },
        environmentToParent
      ),
    deletePlotNode(root, args, context) {
      return deviceInheritAuthorized(
        args.id,
        context.dataLoaders.plotNodeLoaderById,
        context,
        2,
        async (resolve, reject, plotNodeFound, deviceFound) => {
          await plotNodeFound.destroy()

          resolve(args.id)

          const environmentFound =
            deviceFound.environmentId &&
            (await context.dataLoaders.environmentLoaderById.load(
              deviceFound.environmentId
            ))
          deviceFound.increment({ storageUsed: -1 })
          if (environmentFound) touch(Environment, environmentFound.id)
          touch(Device, plotNodeFound.deviceId)
          touch(PlotValue, plotNodeFound.plotId)

          const authorizedUsersIds = environmentFound
            ? [
                deviceFound.producerId,
                ...(await instanceToSharedIds(environmentFound, context)),
              ]
            : [deviceFound.producerId]
          pubsub.publish("valueUpdated", {
            valueUpdated: {
              id: plotNodeFound.plotId,
            },
            userIds: authorizedUsersIds,
            allowedDeviceIds: [deviceFound.id],
          })
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: plotNodeFound.deviceId,
            },
            userIds: authorizedUsersIds,
            allowedDeviceIds: [deviceFound.id],
          })
          if (environmentFound) {
            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound.dataValues,
              userIds: authorizedUsersIds,
            })
          }
          pubsub.publish("plotNodeDeleted", {
            plotNodeDeleted: args.id,
            userIds: authorizedUsersIds,
            source: plotNodeFound,
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    deleteCategoryPlotNode(root, args, context) {
      return deviceInheritAuthorized(
        args.id,
        context.dataLoaders.categoryPlotNodeLoaderById,
        context,
        2,
        async (resolve, reject, plotNodeFound, deviceFound) => {
          await plotNodeFound.destroy()

          resolve(args.id)

          const environmentFound =
            deviceFound.environmentId &&
            (await context.dataLoaders.environmentLoaderById.load(
              deviceFound.environmentId
            ))
          deviceFound.increment({ storageUsed: -1 })
          touch(Environment, environmentFound.id)
          touch(Device, plotNodeFound.deviceId)
          touch(CategoryPlotValue, plotNodeFound.plotId)

          const authorizedUsersIds = environmentFound
            ? [
                deviceFound.producerId,
                ...(await instanceToSharedIds(environmentFound, context)),
              ]
            : [deviceFound.producerId]
          pubsub.publish("valueUpdated", {
            valueUpdated: {
              id: plotNodeFound.plotId,
            },
            userIds: authorizedUsersIds,
            allowedDeviceIds: [deviceFound.id],
          })
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: plotNodeFound.deviceId,
            },
            userIds: authorizedUsersIds,
            allowedDeviceIds: [deviceFound.producerId],
          })
          if (environmentFound) {
            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound.dataValues,
              userIds: authorizedUsersIds,
            })
          }
          pubsub.publish("categoryPlotNodeDeleted", {
            categoryPlotNodeDeleted: args.id,
            userIds: authorizedUsersIds,
            source: plotNodeFound,
            allowedDeviceIds: [deviceFound.id],
          })
        }
      )
    },
    deleteUser: (root, args, context) =>
      authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )

          const environmentsFound = await Environment.findAll({
            where: { ownerId: userFound.id },
          })

          async function deleteEnvironment(environmentFound) {
            const authorizedUsersIds = await instanceToSharedIds(
              environmentFound,
              context
            )
            const devices = await Device.findAll({
              where: { environmentId: environmentFound.id },
            })

            const unclaimDevicesPromises = devices.map(async device => {
              await device.update({
                muted: null,
                environmentId: null,
                index: null,
                name: null,
              })

              pubsub.publish("deviceUnclaimed", {
                deviceUnclaimed: device.id,
                userIds: [...authorizedUsersIds, device.producerId],
                allowedDeviceIds: [device.id],
              })
            })

            async function destroyPendingEnvironmentShare() {
              const pendingEnvironmentSharesFound = await PendingEnvironmentShare.findAll(
                {
                  where: { environmentId: environmentFound.id },
                }
              )

              await Promise.all(
                pendingEnvironmentSharesFound.map(
                  async pendingEnvironmentShare => {
                    pubsub.publish("pendingEnvironmentShareRevoked", {
                      pendingEnvironmentShareRevoked:
                        pendingEnvironmentShare.id,
                      userIds: [pendingEnvironmentShare.receiverId],
                    })
                    await pendingEnvironmentShare.destroy()
                  }
                )
              )
            }
            async function destroyPendingOwnerChange() {
              const ownerChangesFound = await PendingOwnerChange.findAll({
                where: { environmentId: environmentFound.id },
              })

              await Promise.all(
                ownerChangesFound.map(async ownerChange => {
                  pubsub.publish("pendingOwnerChangeRevoked", {
                    pendingOwnerChangeRevoked: ownerChange.id,
                    userId: ownerChange.receiverId,
                  })
                  await ownerChange.destroy()
                })
              )
            }

            async function deleteShareJoinTables() {
              const joinTables = [
                EnvironmentAdmin,
                EnvironmentEditor,
                EnvironmentSpectator,
              ]

              await Promise.all(
                joinTables.map(model =>
                  model.destroy({
                    where: { environmentId: environmentFound.id },
                  })
                )
              )
            }

            await Promise.all([
              ...unclaimDevicesPromises,
              destroyPendingEnvironmentShare(),
              destroyPendingOwnerChange(),
              deleteShareJoinTables(),
            ])

            await environmentFound.destroy()

            pubsub.publish("environmentDeleted", {
              environmentDeleted: environmentFound.id,
              userIds: authorizedUsersIds,
            })
          }

          const deleteEnvironmentsPromises = environmentsFound.map(
            deleteEnvironment
          )

          const destroyTokenPromise = PermanentToken.destroy({
            where: { userId: userFound.id },
          })

          const destroyWebPushPromise = WebPushNotification.destroy({
            where: { userId: userFound.id },
          })

          const joinTables = [
            EnvironmentAdmin,
            EnvironmentEditor,
            EnvironmentSpectator,
          ]

          const removeJoinTablePromises = joinTables.map(model =>
            model.destroy({ where: { userId: userFound.id } })
          )

          async function destroyPendingEnvironmentShare() {
            const pendingEnvironmentSharesFound = await PendingEnvironmentShare.findAll(
              {
                where: {
                  $or: [
                    { receiverId: userFound.id },
                    { senderId: userFound.id },
                  ],
                },
              }
            )

            await Promise.all(
              pendingEnvironmentSharesFound.map(
                async pendingEnvironmentShare => {
                  pubsub.publish("pendingEnvironmentShareRevoked", {
                    pendingEnvironmentShareRevoked: pendingEnvironmentShare.id,
                    userIds: [pendingEnvironmentShare.receiverId],
                  })
                  await pendingEnvironmentShare.destroy()
                }
              )
            )
          }
          async function destroyPendingOwnerChange() {
            const ownerChangesFound = await PendingOwnerChange.findAll({
              where: {
                $or: [{ receiverId: userFound.id }, { senderId: userFound.id }],
              },
            })

            await Promise.all(
              ownerChangesFound.map(async ownerChange => {
                pubsub.publish("pendingOwnerChangeRevoked", {
                  pendingOwnerChangeRevoked: ownerChange.id,
                  userId: ownerChange.receiverId,
                })
                await ownerChange.destroy()
              })
            )
          }
          async function destroyWebauthnKeys() {
            const webauthnKeys = await WebauthnKey.findAll({
              where: {
                userId: context.auth.userId,
              },
            })

            await Promise.all(
              webauthnKeys.map(async key => {
                // pubsub.publish("pendingOwnerChangeRevoked", {
                //   pendingOwnerChangeRevoked: ownerChange.id,
                //   userId: ownerChange.receiverId,
                // })
                await key.destroy()
              })
            )
          }

          await Promise.all([
            ...deleteEnvironmentsPromises,
            destroyTokenPromise,
            destroyWebPushPromise,
            ...removeJoinTablePromises,
            destroyPendingEnvironmentShare(),
            destroyPendingOwnerChange(),
            destroyWebauthnKeys(),
          ])
          const email = userFound.email
          await userFound.destroy()

          sendAccountDeletedEmail(email)

          pubsub.publish("userDeleted", {
            userDeleted: context.auth.userId,
            userId: context.auth.userId,
          })
          resolve(context.auth.userId)
        },
        ["DELETE_USER"]
      ),
  }

  return resolvers
}

export default MutationResolver
