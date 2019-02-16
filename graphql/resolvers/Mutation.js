import bcrypt from "bcryptjs"
import OTP from "otp.js"
import {
  authenticated,
  generateAuthenticationToken,
  generatePermanentAuthenticationToken,
  CreateGenericValue,
  genericValueMutation,
  create2FSecret,
  check2FCode,
  getPropsIfDefined,
  sendVerificationEmail,
  sendPasswordRecoveryEmail,
  sendPasswordUpdatedEmail,
  sendTokenCreatedEmail,
  authorized,
  deviceToParent,
  authorizedValue,
  instanceToSharedIds,
  inheritAuthorized,
  valueToParent,
  randomEnvironmentPicture,
  randomUserIconColor,
  instanceToRole,
  authorizationLevel,
  GenerateUserBillingBatcher,
  environmentToParent,
  sendEnvironmentSharedEmail,
  runInParallel,
  findValue,
  sendPushNotification,
  sendOwnerChangeEmail,
  sendAccountDeletedEmail,
  generateDeviceAuthenticationToken,
} from "./utilities"
const { Fido2Lib } = require("fido2-lib-clone")
import Stripe from "stripe"
import moment from "moment"
import jwt from "jwt-simple"
import zxcvbn from "zxcvbn"
import { isNullOrUndefined } from "util"
import { Op } from "sequelize"
import QRCode from "qrcode-svg"

const f2l = new Fido2Lib()

require("dotenv").config()
/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

const SALT_ROUNDS = 10
const MUTATION_COST = 2

const stripe = Stripe("sk_test_pku6xMd2Tjlv5EU4GkZHw7aS")

const isNotNullNorUndefined = value => value !== undefined && value !== null
const isOutOfBoundaries = (min, max, value) => {
  if (isNotNullNorUndefined(min) && value < min) return true
  if (isNotNullNorUndefined(max) && value > max) return true
  return false
}

const touch = async (Model, id, updatedAt = new Date()) =>
  await Model.update({ updatedAt }, { where: { id } }) // FIXME: updated at is always set to current date by sequelize

function ab2str(buf) {
  return String.fromCharCode.apply(null, new Uint8Array(buf))
}
function str2ab(str) {
  return Uint8Array.from(str, c => c.charCodeAt(0))
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
    MapValue,
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
    UnclaimedDevice,
    WebauthnKey,
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
        } else if (!userFound.twoFactorSecret) {
          // setting context so that the resolvers for user know that the user is authenticated
          context.auth = {
            userId: userFound.id,
            accessLevel: "OWNER",
            tokenType: "TEMPORARY",
          }
          context.billingUpdater = GenerateUserBillingBatcher(
            context.dataLoaders,
            context.auth
          )

          resolve({
            token: generateAuthenticationToken(
              userFound.dataValues.id,
              JWT_SECRET
            ),
            user: userFound,
          })
        } else if (check2FCode(args.twoFactorCode, userFound.twoFactorSecret)) {
          // setting context so that the resolvers for user know that the user is authenticated
          context.auth = {
            userId: userFound.id,
            accessLevel: "OWNER",
            tokenType: "TEMPORARY",
          }
          context.billingUpdater = GenerateUserBillingBatcher(
            context.dataLoaders,
            context.auth
          )

          resolve({
            token: generateAuthenticationToken(
              userFound.dataValues.id,
              JWT_SECRET
            ),
            user: userFound,
          })
        } else {
          reject("Wrong or missing 2-Factor Authentication Code")
        }
      }
    },
    logInWithWebauthn(root, args, context) {
      return async (resolve, reject) => {
        const clientAssertionResponse = JSON.parse(args.challengeResponse)
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

        const { challenge: decodedChallenge, userId } = jwt.decode(
          args.jwtChallenge,
          process.env.JWT_SECRET
        )

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

          // setting context so that the resolvers for user know that the user is authenticated
          context.auth = {
            userId,
            accessLevel: "OWNER",
            tokenType: "TEMPORARY",
          }
          context.billingUpdater = GenerateUserBillingBatcher(
            context.dataLoaders,
            context.auth
          )

          resolve({
            token: generateAuthenticationToken(userId, JWT_SECRET),
            user: { id: userId },
          })
        } catch (e) {
          console.log(e)
          reject(e.message)
        }
      }
    },
    createToken(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )
        if (!bcrypt.compareSync(args.password, userFound.dataValues.password)) {
          reject("Wrong password")
        } else {
          resolve(
            jwt.encode(
              {
                userId: context.auth.userId,
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
      })
    },
    sendPasswordRecoveryEmail(root, args, context) {
      return async (resolve, reject) => {
        const userFound = await User.find({
          where: { email: args.email },
        })
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else {
          sendPasswordRecoveryEmail(userFound.email, userFound.id)

          resolve(true)
        }
      }
    },
    enableWebauthn(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        var clientAttestationResponse = JSON.parse(args.challengeResponse)
        clientAttestationResponse.rawId = new Int8Array(
          clientAttestationResponse.rawId
        ).buffer
        clientAttestationResponse.response.attestationObject = new Int8Array(
          clientAttestationResponse.response.attestationObject
        ).buffer
        clientAttestationResponse.response.clientDataJSON = new Int8Array(
          clientAttestationResponse.response.clientDataJSON
        ).buffer

        try {
          var decoded = jwt.decode(args.jwtChallenge, process.env.JWT_SECRET)
            .challenge

          var attestationExpectations = {
            challenge: str2ab(decoded),
            origin: "https://aurora.igloo.ooo",
            factor: "either",
          }

          var regResult = await f2l.attestationResult(
            clientAttestationResponse,
            attestationExpectations
          )

          const publicKey = regResult.authnrData.get("credentialPublicKeyPem")
          const credId = ab2str(regResult.authnrData.get("credId"))
          const counter = regResult.authnrData.get("counter")

          await WebauthnKey.create({
            userId: context.auth.userId,
            publicKey,
            credId,
            counter,
          })

          resolve(true)
        } catch (e) {
          reject(e.message)
        }
      })
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

            sendTokenCreatedEmail(userFound.email)
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
              devMode: false,
              monthUsage: 0,
              paymentPlan: "FREE",
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

            // setting context so that the resolvers for user know that the user is authenticated
            context.auth = {
              userId: newUser.id,
              accessLevel: "OWNER",
              tokenType: "TEMPORARY",
            }
            context.billingUpdater = GenerateUserBillingBatcher(
              context.dataLoaders,
              context.auth
            )

            resolve({
              token: generateAuthenticationToken(
                newUser.dataValues.id,
                JWT_SECRET
              ),
              changePasswordToken: jwt.encode(
                {
                  userId: newUser.id,
                  tokenType: "CHANGE_PASSWORD",
                  exp: moment()
                    .utc()
                    .add({ minutes: 15 })
                    .unix(),
                },
                JWT_SECRET,
                "HS512"
              ),
              user: newUser,
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
    /*
    UpgradeTo2FactorAuthentication(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
          const userFound = await Ucontext.dataLoaders.userLoaderById.load(context.auth.userId)
          // istanbul ignore if - should ever happen 
          if (!userFound) {
            reject("User doesn't exist. Use `signUp` to create one")
          } else if (!userFound.twoFactorSecret) {
            const { secret, qrCode } = create2FSecret(userFound.email)
            await userFound.update({ twoFactorSecret: secret })
            resolve({ secret, qrCode })
          } else {
            const qrCode = OTP.googleAuthenticator.qrCode(
              userFound.email,
              "igloo",
              userFound.twoFactorSecret
            )

            resolve({
              secret: userFound.twoFactorSecret,
              qrCode,
            })
          }
        })
      
  }, */
    // changes the password and returns an access token
    changePassword(root, args, context) {
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
            if (zxcvbn(args.newPassword, zxcvbnDictionary).score < 2) {
              reject(
                "Password too weak, avoid easily guessable password or short ones"
              )
              return
            }

            const encryptedPass = bcrypt.hashSync(args.newPassword, SALT_ROUNDS)

            const newUser = await userFound.update({
              password: encryptedPass,
            })
            resolve({
              id: newUser.dataValues.id,
              token: generateAuthenticationToken(
                newUser.dataValues.id,
                JWT_SECRET
              ),
            })

            sendPasswordUpdatedEmail(userFound.email)
          }
        },
        ["CHANGE_PASSWORD", "PASSWORD_RECOVERY"]
      )
    },
    resendVerificationEmail(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else if (userFound.emailIsVerified) {
          reject("This user has already verified their email")
        } else {
          resolve(true)
          sendVerificationEmail(userFound.email, userFound.id)
        }
      })
    },
    shareEnvironment: (root, args, context) =>
      authorized(
        args.environmentId,
        context,
        context.dataLoaders.environmentLoaderById,
        User,
        3,
        async (resolve, reject, environmentFound, _, senderFound) => {
          const receiverFound = await User.find({
            where: { email: args.email },
          })

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
            context.billingUpdater.update(MUTATION_COST)

            touch(Environment, args.environmentId, newPendingShare.updatedAt)

            pubsub.publish("pendingEnvironmentShareReceived", {
              pendingEnvironmentShareReceived: newPendingShare,
              userId: receiverFound.id,
            })
            sendEnvironmentSharedEmail(
              receiverFound.email,
              senderFound.name,
              environmentFound.name
            )
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
          context.billingUpdater.update(MUTATION_COST)

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
          // add new role
          const parsedRole = `${pendingEnvironmentFound.role[0] +
            pendingEnvironmentFound.role.slice(1).toLowerCase()}s`
          const environmentFound = await context.dataLoaders.environmentLoaderById.load(
            pendingEnvironmentFound.environmentId
          )
          const userFound = await context.dataLoaders.userLoaderById.load(
            context.auth.userId
          )

          // await userFound[`add${Environment[parsedRole]}`](environmentFound)
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

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("pendingEnvironmentShareAccepted", {
            pendingEnvironmentShareAccepted: payload,
            userId: userFound.id,
          })

          const senderFound = await context.dataLoaders.userLoaderById.load(
            pendingEnvironmentFound.senderId
          )
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

          context.billingUpdater.update(MUTATION_COST)

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
          const receiverFound = await User.find({
            where: { email: args.email },
          })

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
            context.billingUpdater.update(MUTATION_COST)

            touch(Environment, args.environmentId, newOwnerChange.updatedAt)

            pubsub.publish("pendingOwnerChangeReceived", {
              pendingOwnerChangeReceived: newOwnerChange,
              userId: receiverFound.id,
            })
            sendOwnerChangeEmail(
              receiverFound.email,
              senderFound.name,
              environmentFound.name
            )
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

          context.billingUpdater.update(MUTATION_COST)

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
          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("pendingOwnerChangeAccepted", {
            pendingOwnerChangeAccepted: payload,
            userId: userFound.id,
          })

          const senderFound = await context.dataLoaders.userLoaderById.load(
            pendingOwnerChangeFound.senderId
          )
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

          context.billingUpdater.update(MUTATION_COST)

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

            context.billingUpdater.update(MUTATION_COST)
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

          context.billingUpdater.update(MUTATION_COST)
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
          const userFound = await User.find({
            where: { email: args.email },
          })
          if (!userFound) {
            reject("This user doesn't exist, check that the email is correct")
            return
          }

          const role = await instanceToRole(
            environmentFound,
            userFound,
            context
          )

          if (!userFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (!role) {
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
            context.billingUpdater.update(MUTATION_COST)

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

        context.billingUpdater.update(MUTATION_COST)
      })
    },
    createDevice(root, args, context) {
      return async (resolve, reject) => {
        return authorized(
          args.environmentId,
          context,
          context.dataLoaders.environmentLoaderById,
          User,
          2,
          async (resolve, reject, environmentFound, _, userFound) => {
            // checks that batteryStatus and signalStatus are within boundaries [0,100]
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
            } else if (args.name === "") {
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

            const newDevice = await Device.create({
              ...args,
              muted: !!args.muted,
              environmentId: args.environmentId,
              index,
            })

            const resolveValue = {
              ...newDevice.dataValues,
              environment: newDevice.environmentId
                ? {
                    id: newDevice.environmentId,
                  }
                : null,
            }

            pubsub.publish("deviceCreated", {
              deviceCreated: resolveValue,
              userIds: await instanceToSharedIds(environmentFound, context),
            })

            resolve(resolveValue)

            touch(Environment, environmentFound.id, newDevice.createdAt)
            context.billingUpdater.update(MUTATION_COST)
          },
          environmentToParent
        )(resolve, reject)
      }
    },
    createUnclaimedDevice(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

        if (userFound.paymentPlan !== "BUSINESS") {
          reject("Only business users can create unclaimed devices")
          return
        }

        const newDevice = await UnclaimedDevice.create(args)

        const id = newDevice.id

        resolve({
          id,
          jwtToken: generateDeviceAuthenticationToken(newDevice.id, JWT_SECRET),
          qrCode: new QRCode({ content: id }).svg(),
        })

        context.billingUpdater.update(MUTATION_COST)
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
            // checks that batteryStatus and signalStatus are within boundaries [0,100]
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
            } else if (args.name === "") {
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

            const unclaimedDevice = await UnclaimedDevice.find({
              where: { id: args.unclaimedDeviceId },
            })

            if (!unclaimedDevice) {
              reject("The requested resource does not exist")
              return
            }

            const newDevice = await Device.create({
              ...args,
              muted: !!args.muted,
              environmentId: args.environmentId,
              index,
              firmware: unclaimedDevice.firmware,
              deviceType: unclaimedDevice.deviceType,
              id: unclaimedDevice.id,
            })

            const resolveValue = {
              ...newDevice.dataValues,
              environment: newDevice.environmentId
                ? {
                    id: newDevice.environmentId,
                  }
                : null,
            }

            pubsub.publish("deviceCreated", {
              deviceCreated: resolveValue,
              userIds: await instanceToSharedIds(environmentFound, context),
            })

            // TODO: add company account in the userIds
            pubsub.publish("deviceClaimed", {
              deviceClaimed: resolveValue,
              userIds: await instanceToSharedIds(environmentFound, context),
              allowedDeviceIds: [newDevice.id],
            })

            resolve(resolveValue)

            await unclaimedDevice.destroy()

            touch(Environment, environmentFound.id, newDevice.createdAt)
            context.billingUpdater.update(MUTATION_COST)
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
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        CategoryPlotValue,
      ],
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
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        CategoryPlotValue,
      ],
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
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        CategoryPlotValue,
      ],
      pubsub,
      (args, reject) => {
        if (args.cardSize !== "NORMAL") {
          reject("booleanValue can have cardSize set only to NORMAL")
          return false
        }

        return true
      }
    ),
    createMapValue: CreateGenericValue(
      User,
      Device,
      Environment,
      MapValue,
      "MapValue",
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        CategoryPlotValue,
      ],
      pubsub
    ),
    createPlotValue: CreateGenericValue(
      User,
      Device,
      Environment,
      PlotValue,
      "PlotValue",
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        CategoryPlotValue,
      ],
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
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        CategoryPlotValue,
      ],
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
      return authorized(
        args.plotId,
        context,
        context.dataLoaders.plotValueLoaderById,
        User,
        2,
        async (resolve, reject, plotValueFound, [_, environmentFound]) => {
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

          touch(Environment, environmentFound.id, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(PlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("plotNodeCreated", {
            plotNodeCreated: resolveObj,
            userIds: await instanceToSharedIds(environmentFound, context),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent
      )
    },
    createCategoryPlotNode(root, args, context) {
      return authorized(
        args.plotId,
        context,
        context.dataLoaders.categoryPlotValueLoaderById,
        User,
        2,
        async (resolve, reject, plotValueFound, [_, environmentFound]) => {
          if (
            isNotNullNorUndefined(plotValueFound.allowedValues) &&
            plotValueFound.allowedValues.indexOf(args.value)
          ) {
            reject("Value not in allowedValues")
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

          touch(Environment, environmentFound.id, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(CategoryPlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("categoryPlotNodeCreated", {
            categoryPlotNodeCreated: resolveObj,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent
      )
    },
    user(root, args, context) {
      return (resolve, reject) => {
        if (args.name === null || args.name === "") {
          reject("name cannot be null or empty")
          return
        }

        const mutationFields = Object.keys(args)
        let permissionRequired
        if (mutationFields.length === 1 && mutationFields[0] === "usageCap") {
          permissionRequired = ["TEMPORARY", "PERMANENT", "CHANGE_USAGE_CAP"]
        } else if (
          mutationFields.length === 1 &&
          mutationFields[0] === "paymentPlan"
        ) {
          permissionRequired = ["TEMPORARY", "PERMANENT", "SWITCH_TO_PAYING"]
        }

        authenticated(
          context,
          async (resolve, reject) => {
            const userFound = await context.dataLoaders.userLoaderById.load(
              context.auth.userId
            )

            if (!userFound) {
              reject("User doesn't exist. Use `signUp` to create one")
            } else {
              const newUser = await userFound.update(args)
              resolve(newUser.dataValues)

              // if the token used for the mutation is not a usageCap update or paymentPlan update bill it
              if (permissionRequired === undefined) {
                context.billingUpdater.update(MUTATION_COST)
              }
              pubsub.publish("userUpdated", {
                userUpdated: newUser.dataValues,
                userId: context.auth.userId,
              })

              if (
                args.hasOwnProperty("name") ||
                args.hasOwnProperty("profileIcon") ||
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

                const environmentShares = await PendingEnvironmentShare.findAll(
                  {
                    where: {
                      [Op.or]: [
                        { senderId: userFound.id },
                        { receiverId: userFound.id },
                      ],
                    },
                  }
                )

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
          },
          permissionRequired
        )(resolve, reject)
      }
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

            context.billingUpdater.update(MUTATION_COST)
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

          context.billingUpdater.update(MUTATION_COST)
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
          context.billingUpdater.update(MUTATION_COST)
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

          context.billingUpdater.update(MUTATION_COST)
        },
        environmentToParent
      )
    },
    device(root, args, context) {
      return authorized(
        args.id,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        1,
        async (
          resolve,
          reject,
          deviceFound,
          [_, environmentFound],
          userFound
        ) => {
          // users with read only access can only star a device
          if (
            (await instanceToRole(environmentFound, userFound, context)) ===
              "SPECTATOR" &&
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
            (environmentFound.muted || userFound.quietMode) &&
            isNotNullNorUndefined(args.muted)
          ) {
            reject(
              "Cannot change muted at device level when it is enabled at environment level or quietMode is enabled at user level"
            )
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

          touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent
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
            MapValue,
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
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent
      )
    },
    resetOnlineState(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        2,
        async (resolve, reject, deviceFound, [_, environmentFound]) => {
          const newDevice = await deviceFound.update({ online: null })
          resolve(newDevice.dataValues)

          touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent
      )
    },
    value(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
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
        }

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

        const valueType = valueFound._modelOptions.name.singular
        const expectedNewValue = {
          ...valueFound,
          ...args,
        }
        if (
          valueType === "floatValue" &&
          expectedNewValue.cardSize === "LARGE"
        ) {
          reject("FloatValue cannot have cardSize set to LARGE")
          return false
        }

        if (
          (await authorizationLevel(environmentFound, userFound, context)) > 0
        ) {
          const newValue = await valueFound.update(args)
          resolve(newValue)

          Environment.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: environmentFound.id } }
          )
          Device.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: valueFound.deviceId } }
          )

          pubsub.publish("valueUpdated", {
            valueUpdated: newValue,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          context.billingUpdater.update(MUTATION_COST)
        } else {
          reject("You are not authorized to perform this operation")
        }
      })
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
    mapValue: genericValueMutation(
      "mapValueLoaderById",
      "MapValue",
      pubsub,
      User,
      Device,
      Environment
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
      authorized(
        args.id,
        context,
        context.dataLoaders.floatValueLoaderById,
        User,
        2,
        async (resolve, reject, valueFound, [_, environmentFound]) => {
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

          Environment.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: environmentFound.id } }
          )
          Device.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: newValue.deviceId } }
          )

          pubsub.publish("valueUpdated", {
            valueUpdated: { ...resolveObj, __resolveType: "FloatValue" },
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent
      ),
    plotNode(root, args, context) {
      return inheritAuthorized(
        args.id,
        context.dataLoaders.plotNodeLoaderById,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        context.dataLoaders.plotValueLoaderById,
        2,
        async (
          resolve,
          reject,
          plotNodeFound,
          plotValueFound,
          [_, environmentFound]
        ) => {
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

          touch(Environment, environmentFound.id, newNode.updatedAt)
          touch(Device, plotValueFound.deviceId, newNode.updatedAt)
          touch(PlotValue, plotValueFound.id, newNode.updatedAt)

          pubsub.publish("plotNodeUpdated", {
            plotNodeUpdated: resolveObj,
            userIds: await instanceToSharedIds(environmentFound, context),
          })

          context.billingUpdater.update(MUTATION_COST)
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
        2,
        async (
          resolve,
          reject,
          plotNodeFound,
          plotValueFound,
          [_, environmentFound]
        ) => {
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

          touch(Environment, environmentFound.id, newNode.updatedAt)
          touch(Device, plotValueFound.deviceId, newNode.updatedAt)
          touch(CategoryPlotValue, plotValueFound.id, newNode.updatedAt)

          pubsub.publish("categoryPlotNodeUpdated", {
            categoryPlotNodeUpdated: resolveObj,
            userIds: await instanceToSharedIds(environmentFound, context),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent
      )
    },
    createNotification(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        2,
        async (
          resolve,
          reject,
          deviceFound,
          [_, environmentFound],
          userFound
        ) => {
          if (args.content === "" || args.content === null) {
            reject("content cannot be null or an empty string")
            return
          }
          const deviceSharedIds = await instanceToSharedIds(
            environmentFound,
            context
          )

          const newNotification = await Notification.create({
            ...args,
            environmentId: environmentFound.id,
            notRead: deviceSharedIds,
            userId: context.auth.userId,
            date: args.date || new Date(),
          })

          // TODO: is this stuff useful?
          deviceFound.addNotification(newNotification)
          newNotification.setDevice(deviceFound)

          environmentFound.addNotification(newNotification)
          newNotification.setEnvironment(environmentFound)

          resolve(newNotification)

          touch(Environment, environmentFound.id, newNotification.updatedAt)
          touch(Device, newNotification.deviceId, newNotification.updatedAt)

          pubsub.publish("notificationCreated", {
            notificationCreated: newNotification,
            userIds: deviceSharedIds,
          })

          // the notificationCount props are updated so send the device and environment subscriptions
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: newNotification.deviceId,
            },
            userIds: deviceSharedIds,
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: deviceSharedIds,
          })

          context.billingUpdater.update(MUTATION_COST)

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
        },
        deviceToParent
      )
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
        async (
          resolve,
          reject,
          notificationFound,
          deviceFound,
          [_, environmentFound],
          userFound
        ) => {
          if (args.content === "" || args.content === null) {
            reject("content cannot be null or an empty string")
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          } else if (
            (await instanceToRole(environmentFound, userFound, context)) ===
              "SPECTATOR" &&
            (Object.keys(args).length > 2 || !isNotNullNorUndefined(args.read))
          ) {
            reject("You are not allowed to mutate fields other than read")
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

          const deviceSharedIds = await instanceToSharedIds(
            environmentFound,
            context
          )
          pubsub.publish("notificationUpdated", {
            notificationUpdated: newNotification,
            userIds: deviceSharedIds,
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent
      )
    },
    deleteNotification(root, args, context) {
      return inheritAuthorized(
        args.id,
        context.dataLoaders.notificationLoaderById,
        User,
        notificationFound => notificationFound.deviceId,
        context,
        context.dataLoaders.deviceLoaderById,
        2,
        async (
          resolve,
          reject,
          notificationFound,
          deviceFound,
          [_, environmentFound]
        ) => {
          await notificationFound.destroy()

          resolve(args.id)

          const deviceSharedIds = await instanceToSharedIds(
            environmentFound,
            context
          )
          pubsub.publish("notificationDeleted", {
            notificationDeleted: args.id,
            userIds: deviceSharedIds,
          })

          // the notificationCount props are updated so send the
          // device and environment subscriptions and change updatedAt
          touch(Environment, environmentFound.id)
          touch(Device, deviceFound.id)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: deviceFound.id,
            },
            userIds: deviceSharedIds,
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: await instanceToSharedIds(environmentFound, context),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent
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
          MapValue,
          PlotValue,
          CategoryPlotValue,
        },
        User,
        3,
        async (resolve, reject, valueFound, [_, environmentFound]) => {
          const authorizedUsersIds = await instanceToSharedIds(
            environmentFound,
            context
          )

          // TODO: if value is plot remove nodes
          await valueFound.destroy()

          pubsub.publish("valueDeleted", {
            valueDeleted: args.id,
            userIds: authorizedUsersIds,
          })
          resolve(args.id)

          touch(Environment, environmentFound.id)
          touch(Device, valueFound.deviceId)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: valueFound.deviceId,
            },
            userIds: authorizedUsersIds,
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: authorizedUsersIds,
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        Device,
        Environment
      ),
    deleteDevice: (root, args, context) =>
      authorized(
        args.id,
        context,
        context.dataLoaders.deviceLoaderById,
        User,
        3,
        async (resolve, reject, deviceFound, [_, environmentFound]) => {
          const authorizedUsersIds = await instanceToSharedIds(
            environmentFound,
            context
          )

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
                  [subscription]: args.id,
                  userIds: authorizedUsersIds,
                })
              })
            )
          }

          await Promise.all(
            [
              [FloatValue, "valueDeleted"],
              [StringValue, "valueDeleted"],
              [BooleanValue, "valueDeleted"],
              [MapValue, "valueDeleted"],
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
          })

          resolve(args.id)

          touch(Environment, environmentFound.id)
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: authorizedUsersIds,
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent
      ),
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
          console.log(authorizedUsersIds)

          const devices = await Device.findAll({
            where: { environmentId: environmentFound.id },
          })

          // TODO: send deleted notifications for children
          const deleteDevicesPromises = devices.map(async device => {
            const deleteChild = async ([Model, subscription]) => {
              const childrenFound = await Model.findAll({
                where: {
                  deviceId: device.id,
                },
              })

              await Promise.all(
                childrenFound.map(async child => {
                  await child.destroy()
                  pubsub.publish(subscription, {
                    [subscription]: args.id,
                    userIds: authorizedUsersIds,
                  })
                })
              )
            }

            await Promise.all(
              [
                [PlotNode, "plotNodeDeleted"],
                [CategoryPlotNode, "categoryPlotNodeDeleted"],
              ].map(deleteChild)
            )

            await Promise.all(
              [
                [FloatValue, "valueDeleted"],
                [StringValue, "valueDeleted"],
                [BooleanValue, "valueDeleted"],
                [MapValue, "valueDeleted"],
                [PlotValue, "valueDeleted"],
                [CategoryPlotValue, "valueDeleted"],
                [Notification, "notificationDeleted"],
              ].map(deleteChild)
            )

            await device.destroy()
            pubsub.publish("deviceDeleted", {
              deviceDeleted: device.id,
              userIds: authorizedUsersIds,
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
            ...deleteDevicesPromises,
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

          context.billingUpdater.update(MUTATION_COST)
        },
        environmentToParent
      ),
    deletePlotNode(root, args, context) {
      return inheritAuthorized(
        args.id,
        context.dataLoaders.plotNodeLoaderById,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        context.dataLoaders.plotValueLoaderById,
        2,
        async (
          resolve,
          reject,
          plotNodeFound,
          plotValueFound,
          [_, environmentFound]
        ) => {
          await plotNodeFound.destroy()

          resolve(args.id)

          touch(Environment, environmentFound.id)
          touch(Device, plotNodeFound.deviceId)
          touch(PlotValue, plotNodeFound.plotId)

          const authorizedUsersIds = await instanceToSharedIds(
            environmentFound,
            context
          )
          pubsub.publish("valueUpdated", {
            valueUpdated: {
              id: plotNodeFound.plotId,
            },
            userIds: authorizedUsersIds,
          })
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: plotNodeFound.deviceId,
            },
            userIds: authorizedUsersIds,
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: authorizedUsersIds,
          })
          pubsub.publish("plotNodeDeleted", {
            plotNodeDeleted: args.id,
            userIds: authorizedUsersIds,
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent
      )
    },
    deleteCategoryPlotNode(root, args, context) {
      return inheritAuthorized(
        args.id,
        plotValueLoaderById.categoryPlotNodeLoaderById,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        context.dataLoaders.categoryPlotValueLoaderById,
        2,
        async (
          resolve,
          reject,
          plotNodeFound,
          plotValueFound,
          [_, environmentFound]
        ) => {
          await plotNodeFound.destroy()

          resolve(args.id)

          touch(Environment, environmentFound.id)
          touch(Device, plotNodeFound.deviceId)
          touch(CategoryPlotValue, plotNodeFound.plotId)

          const authorizedUsersIds = await instanceToSharedIds(
            environmentFound,
            context
          )
          pubsub.publish("valueUpdated", {
            valueUpdated: {
              id: plotNodeFound.plotId,
            },
            userIds: authorizedUsersIds,
          })
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: plotNodeFound.deviceId,
            },
            userIds: authorizedUsersIds,
          })
          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound.dataValues,
            userIds: authorizedUsersIds,
          })
          pubsub.publish("categoryPlotNodeDeleted", {
            categoryPlotNodeDeleted: args.id,
            userIds: authorizedUsersIds,
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent
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

            const deleteDevicesPromises = devices.map(async device => {
              const deleteChild = async ([Model, subscription]) => {
                const childrenFound = await Model.findAll({
                  where: {
                    deviceId: device.id,
                  },
                })

                await Promise.all(
                  childrenFound.map(async child => {
                    await child.destroy()
                    pubsub.publish(subscription, {
                      [subscription]: child.id,
                      userIds: authorizedUsersIds,
                    })
                  })
                )
              }

              await Promise.all(
                [
                  [PlotNode, "plotNodeDeleted"],
                  [CategoryPlotNode, "categoryPlotNodeDeleted"],
                ].map(deleteChild)
              )
              await Promise.all(
                [
                  [FloatValue, "valueDeleted"],
                  [StringValue, "valueDeleted"],
                  [BooleanValue, "valueDeleted"],
                  [MapValue, "valueDeleted"],
                  [PlotValue, "valueDeleted"],
                  [CategoryPlotValue, "valueDeleted"],
                  [Notification, "notificationDeleted"],
                ].map(deleteChild)
              )

              await device.destroy()
              pubsub.publish("deviceDeleted", {
                deviceDeleted: device.id,
                userIds: authorizedUsersIds,
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
              ...deleteDevicesPromises,
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
