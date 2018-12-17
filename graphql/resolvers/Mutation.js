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
  randomEnvironmentAvatar,
  randomUserIconColor,
  instanceToRole,
  authorizationLevel,
  GenerateUserBillingBatcher,
  environmentToParent,
  sendEnvironmentSharedEmail,
  runInParallel,
} from "./utilities"
import webpush from "web-push"
import Stripe from "stripe"
import moment from "moment"
import jwt from "jwt-simple"
import { Op } from "sequelize"
import zxcvbn from "zxcvbn"

require("dotenv").config()
/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

webpush.setVapidDetails(
  "http://igloo.witlab.io/",
  process.env.PUBLIC_VAPID_KEY,
  process.env.PRIVATE_VAPID_KEY
)
const SALT_ROUNDS = 10
const MUTATION_COST = 2

const stripe = Stripe("sk_test_pku6xMd2Tjlv5EU4GkZHw7aS")

const isNotNullNorUndefined = value => value !== undefined && value !== null
const isOutOfBoundaries = (boundaries, value) =>
  value < boundaries[0] || value > boundaries[1]

const touch = async (Model, id, updatedAt = new Date()) =>
  await Model.update({ updatedAt }, { where: { id } }) // FIXME: updated at is always set to current date by sequelize

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
    StringPlotValue,
    StringPlotNode,
    Notification,
    PendingEnvironmentShare,
    PendingOwnerChange,
  },
  WebPushSubscription,
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
            User,
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
            User,
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
    createToken(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })
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
    createPermanentAccessToken(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          console.log("test")
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

            const userFound = await User.find({
              where: { id: context.auth.userId },
            })

            sendTokenCreatedEmail(userFound.email)
          }
        },
        ["GENERATE_PERMANENT_TOKEN"]
      )
    },
    deletePermanentAccesToken(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        const databaseToken = await PermanentToken.find({
          where: { id: args.id },
        })
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
      })
    }, // if not it creates one and returnes an access token // checks if a user with that email already exists
    signUp(root, args, context) {
      return async (resolve, reject) => {
        // check password strength
        const zxcvbnDictionary = [
          args.email,
          args.email.split("@")[0],
          args.name,
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

        if (!args.name) {
          reject("name required")
          return
        }

        const userFound = await User.find({ where: { email: args.email } })
        if (userFound) {
          reject("A user with this email already exists")
        } else {
          const encryptedPass = bcrypt.hashSync(args.password, SALT_ROUNDS)
          try {
            const newUser = await User.create({
              email: args.email,
              password: encryptedPass,
              quietMode: false,
              devMode: false,
              monthUsage: 0,
              paymentPlan: "FREE",
              emailIsVerified: false,
              name: args.name,
              profileIconColor: randomUserIconColor(),
              settings_language: "en-GB",
              settings_timeZone: "+00:00_Greenwich", // TODO: Daylight Saving Time
              settings_lengthAndMass: "SI",
              settings_temperature: "CELSIUS",
              settings_dateFormat: "DMY",
              settings_timeFormat: "H24",
            })

            const newEnvironment = await Environment.create({
              name: "Home",
              ownerId: newUser.id,
              avatar: randomEnvironmentAvatar(),
              muted: false,
              index: 0,
            })

            // await newUser.addOwnEnvironment(newEnvironment)
            // await newEnvironment.setOwner(newUser)

            // setting context so that the resolvers for user know that the user is authenticated
            context.auth = {
              userId: newUser.id,
              accessLevel: "OWNER",
              tokenType: "TEMPORARY",
            }
            context.billingUpdater = GenerateUserBillingBatcher(
              User,
              context.auth
            )

            resolve({
              token: generateAuthenticationToken(
                newUser.dataValues.id,
                JWT_SECRET
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
    } /* 
    UpgradeTo2FactorAuthentication(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          // istanbul ignore if - should ever happen 
          if (!userFound) {
            reject("User doesn't exist. Use `` to create one")
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
      
  }, */,
    // changes the password and returns an access token
    changePassword(root, args, context) {
      return authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          if (!userFound) {
            reject("User doesn't exist. Use `` to create one")
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
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })
        if (!userFound) {
          reject("User doesn't exist. Use `` to create one")
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
        Environment,
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
            const role = await instanceToRole(environmentFound, receiverFound)
            if (role !== null) {
              reject("The user already has a role on this environment")
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
              reject(`There is already a environmentShare pending`)
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
              reject(`There is already an ownerChange pending`)
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

            pubsub.publish("environmentSharedWithYou", {
              environmentSharedWithYou: newPendingShare,
              userId: receiverFound.id,
            })
            sendEnvironmentSharedEmail(
              receiverFound.email,
              senderFound.name,
              environmentFound.name
            )

            const usersWithAccessIds = (await instanceToSharedIds(
              environmentFound
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
        PendingEnvironmentShare,
        User,
        pendingEnvironmentShare => pendingEnvironmentShare.environmentId,
        context,
        Environment,
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

          pubsub.publish("environmentShareUpdated", {
            environmentShareUpdated: newPendingEnvironmentShare,
            userId: newPendingEnvironmentShare.receiverId,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound
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
          const environmentFound = await Environment.find({
            where: { id: pendingEnvironmentFound.environmentId },
          })
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          await userFound[`add${Environment[parsedRole]}`](environmentFound)

          resolve({ id: environmentFound.id })

          touch(Environment, environmentFound.id)

          await pendingEnvironmentFound.destroy()
          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("environmentShareAccepted", {
            environmentShareAccepted: { id: environmentFound.id },
            userId: userFound.id,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound
          )).filter(id => id !== context.auth.userId)

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: usersWithAccessIds,
          })
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
          const environmentFound = await Environment.find({
            where: { id: pendingEnvironmentFound.environmentId },
          })

          await pendingEnvironmentFound.destroy()

          resolve(pendingEnvironmentFoundId)

          touch(Environment, environmentFound.id)

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("environmentShareDeclined", {
            environmentShareDeclined: pendingEnvironmentFoundId,
            userId: context.auth.userId,
          })

          const usersWithAccessIds = await instanceToSharedIds(environmentFound)

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
          const environmentFound = await Environment.find({
            where: { id: pendingEnvironmentFound.environmentId },
          })
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          if ((await authorizationLevel(environmentFound, userFound)) < 3) {
            reject("You are not authorized to perform this operation")
          } else {
            const revokedId = pendingEnvironmentFound.id
            const receiverId = pendingEnvironmentFound.receiverId
            await pendingEnvironmentFound.destroy()

            resolve(revokedId)

            touch(Environment, environmentFound.id)

            const usersWithAccessIds = await instanceToSharedIds(
              environmentFound
            )

            pubsub.publish("environmentShareRevoked", {
              environmentShareRevoked: revokedId,
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
        Environment,
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

              pubsub.publish("environmentShareRevoked", {
                environmentShareRevoked: otherPendingShare.id,
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

            pubsub.publish("ownerChangeBegan", {
              ownerChangeBegan: newOwnerChange,
              userId: receiverFound.id,
            })
            sendEnvironmentSharedEmail(
              receiverFound.email,
              senderFound.name,
              environmentFound.name
            )

            const usersWithAccessIds = (await instanceToSharedIds(
              environmentFound
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
        PendingOwnerChange,
        User,
        pendingEnvironmentShare => pendingEnvironmentShare.environmentId,
        context,
        Environment,
        3,
        async (resolve, reject, pendingOwnerChangeFound, environmentFound) => {
          const targetUserId = pendingOwnerChangeFound.receiverId
          await pendingOwnerChangeFound.destroy()

          resolve(args.pendingOwnerChangeId)

          touch(Environment, environmentFound.id)

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("ownerChangeRevoked", {
            ownerChangeRevoked: args.pendingOwnerChangeId,
            userId: targetUserId,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound
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
          const environmentFound = await Environment.find({
            where: { id: pendingOwnerChangeFound.environmentId },
          })
          const userFound = await User.find({
            where: { id: pendingOwnerChangeFound.receiverId },
          })

          // remove old roles
          await runInParallel(
            () => userFound[`remove${Environment.Admins}`](environmentFound),
            () => userFound[`remove${Environment.Editors}`](environmentFound),
            () => userFound[`remove${Environment.Spectators}`](environmentFound)
          )

          await environmentFound.setOwner(userFound)
          await userFound.addOwnEnvironment(environmentFound)

          const oldOwnerFound = await User.find({
            where: { id: pendingOwnerChangeFound.senderId },
          })
          await oldOwnerFound.removeOwnEnvironment(environmentFound)
          await oldOwnerFound[`add${Environment.Admins}`](environmentFound)

          resolve(environmentFound.id)

          touch(Environment, environmentFound.id)

          await pendingOwnerChangeFound.destroy()
          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("environmentShareAccepted", {
            environmentShareAccepted: environmentFound.id,
            userId: userFound.id,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            environmentFound
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

          const environmentFound = await Environment.find({
            where: { id: pendingOwnerChangeFound.environmentId },
          })
          touch(Environment, args.environmentId)

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("ownerChangeDeclined", {
            ownerChangeRevoked: args.pendingOwnerChangeId,
            userId: targetUserId,
          })

          // sending environmentUpdated subscription also to the target user
          // for ease of handling on the client side
          const usersWithAccessIds = await instanceToSharedIds(environmentFound)
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
        Environment,
        User,
        3,
        async (resolve, reject, environmentFound, _, userFound) => {
          const targetUserFound = await User.find({
            where: { email: args.email },
          })
          const currentRole = await instanceToRole(
            environmentFound,
            targetUserFound
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
                targetUserFound[`remove${Environment.Admins}`](
                  environmentFound
                ),
              () =>
                targetUserFound[`remove${Environment.Editors}`](
                  environmentFound
                ),
              () =>
                targetUserFound[`remove${Environment.Spectators}`](
                  environmentFound
                )
            )

            // add new role
            const parsedRole = `${args.newRole[0] +
              args.newRole.slice(1).toLowerCase()}s`

            await targetUserFound[`add${Environment[parsedRole]}`](
              environmentFound
            )

            resolve(environmentFound)

            touch(Environment, args.environmentId)

            pubsub.publish("environmentUpdated", {
              environmentUpdated: environmentFound,
              userIds: await instanceToSharedIds(environmentFound),
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
        Environment,
        User,
        1,
        async (resolve, reject, environmentFound, _, userFound) => {
          if ((await instanceToRole(environmentFound, userFound)) === "OWNER") {
            reject("You cannot leave a environment that you own")
            return
          }

          await runInParallel(
            () => userFound[`remove${Environment.Admins}`](environmentFound),
            () => userFound[`remove${Environment.Editors}`](environmentFound),
            () => userFound[`remove${Environment.Spectators}`](environmentFound)
          )
          resolve(environmentFound.id)

          touch(Environment, args.environmentId)

          pubsub.publish("environmentStoppedSharingWithYou", {
            environmentUpdated: environmentFound.id,
            userId: userFound.id,
          })

          pubsub.publish("environmentUpdated", {
            environmentUpdated: environmentFound,
            userIds: await instanceToSharedIds(environmentFound),
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
        Environment,
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

          const role = await instanceToRole(environmentFound, userFound)

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
              () => userFound[`remove${Environment.Admins}`](environmentFound),
              () => userFound[`remove${Environment.Editors}`](environmentFound),
              () =>
                userFound[`remove${Environment.Spectators}`](environmentFound)
            )

            resolve(environmentFound)
            context.billingUpdater.update(MUTATION_COST)

            touch(Environment, args.environmentId)

            pubsub.publish("environmentStoppedSharingWithYou", {
              environmentStoppedSharingWithYou: args.environmentId,
              userId: userFound.id,
            })

            const usersWithAccessIds = await instanceToSharedIds(
              environmentFound
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

        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

        const newEnvironment = await Environment.create({
          ...args,
          ownerId: userFound.id,
          avatar: args.avatar || randomEnvironmentAvatar(),
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
          Environment,
          User,
          2,
          async (resolve, reject, environmentFound, _, userFound) => {
            // checks that batteryStatus and signalStatus are within boundaries [0,100]
            if (
              isNotNullNorUndefined(args.batteryStatus) &&
              isOutOfBoundaries([0, 100], args.batteryStatus)
            ) {
              reject("batteryStatus is out of boundaries [0,100]")
              return
            } else if (
              isNotNullNorUndefined(args.signalStatus) &&
              isOutOfBoundaries([0, 100], args.signalStatus)
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
            console.log(newDevice.id)

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
              userIds: await instanceToSharedIds(environmentFound),
            })

            resolve(resolveValue)

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
        StringPlotValue,
      ],
      pubsub,
      (args, reject) => {
        if (
          isNotNullNorUndefined(args.boundaries) &&
          (args.boundaries.length !== 2 ||
            args.boundaries[0] >= args.boundaries[1])
        ) {
          reject("Boundaries should be a [min, max] array")
          return false
        } else if (
          isNotNullNorUndefined(args.boundaries) &&
          isOutOfBoundaries(args.boundaries, args.value)
        ) {
          reject("Value is out of boundaries")
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
        StringPlotValue,
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
        StringPlotValue,
      ],
      pubsub
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
        StringPlotValue,
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
        StringPlotValue,
      ],
      pubsub
    ),
    createStringPlotValue: CreateGenericValue(
      User,
      Device,
      Environment,
      StringPlotValue,
      "StringPlotValue",
      [
        FloatValue,
        StringValue,
        BooleanValue,
        MapValue,
        PlotValue,
        StringPlotValue,
      ],
      pubsub
    ),
    createPlotNode(root, args, context) {
      return authorized(
        args.plotId,
        context,
        PlotValue,
        User,
        2,
        async (resolve, reject, plotValueFound, [_, environmentFound]) => {
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
            userIds: await instanceToSharedIds(environmentFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Environment)
      )
    },
    createStringPlotNode(root, args, context) {
      return authorized(
        args.plotId,
        context,
        StringPlotValue,
        User,
        2,
        async (resolve, reject, plotValueFound, [_, environmentFound]) => {
          const plotNode = await StringPlotNode.create({
            ...args,
            timestamp: args.timestamp || new Date(),
            deviceId: plotValueFound.deviceId,
            userId: context.auth.userId,
          })

          plotNode.setPlot(plotValueFound)
          plotValueFound.addStringPlotNode(plotNode)

          const resolveObj = {
            ...plotNode.dataValues,
            user: { id: plotNode.userId },
            device: { id: plotNode.deviceId },
            plot: { id: plotNode.plotId },
          }

          resolve(resolveObj)

          touch(Environment, environmentFound.id, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(StringPlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("stringPlotNodeCreated", {
            stringPlotNodeCreated: resolveObj,
            userIds: await instanceToSharedIds(environmentFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Environment)
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
            const userFound = await User.find({
              where: { id: context.auth.userId },
            })

            if (!userFound) {
              reject("User doesn't exist. Use `` to create one")
            } else {
              const newUser = await userFound.update(args)
              resolve(newUser.dataValues)

              pubsub.publish("userUpdated", {
                userUpdated: newUser.dataValues,
                userId: context.auth.userId,
              })

              // if the token used for the mutation is not a usageCap update or paymentPlan update bill it
              if (permissionRequired === undefined) {
                context.billingUpdater.update(MUTATION_COST)
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
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          const sameEmailUserFound = await User.find({
            where: { email: args.newEmail },
          })
          if (sameEmailUserFound) {
            reject("A user with this email already exists")
            return
          }

          try {
            const newUser = await userFound.update({
              email: args.newEmail,
              emailIsVerified: false,
            })
            resolve(true)

            pubsub.publish("userUpdated", {
              userUpdated: newUser.dataValues,
              userId: context.auth.userId,
            })

            sendVerificationEmail(args.newEmail, newUser.id)

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
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })
        if (!userFound) {
          reject("User doesn't exist. Use `` to create one")
        } else if (
          args.timeZone === null ||
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
            "timeZone",
            "language",
            "lengthAndMass",
            "temperature",
            "dateFormat",
            "timeFormat",
          ]
          fields.forEach(field => {
            if (isNotNullNorUndefined(args[field])) {
              updateQuery[`settings_${field}`] = args[field]
            }
          })

          const newUser = await userFound.update(updateQuery)

          resolve({
            timeZone: newUser.settings_timeZone,
            language: newUser.settings_language,
            lengthAndMass: newUser.settings_lengthAndMass,
            temperature: newUser.settings_temperature,
            dateFormat: newUser.settings_dateFormat,
            timeFormat: newUser.settings_timeFormat,
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
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })
        if (!userFound) {
          reject("User doesn't exist. Use `` to create one")
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
        Environment,
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
            userIds: await instanceToSharedIds(environmentFound),
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
        Device,
        User,
        2,
        async (
          resolve,
          reject,
          deviceFound,
          [_, environmentFound],
          userFound
        ) => {
          // runs sanity checks on the args
          if (
            isNotNullNorUndefined(args.batteryStatus) &&
            isOutOfBoundaries([0, 100], args.batteryStatus)
          ) {
            reject("batteryStatus is out of boundaries [0,100]")
            return
          } else if (
            isNotNullNorUndefined(args.signalStatus) &&
            isOutOfBoundaries([0, 100], args.signalStatus)
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

          const newDevice = await deviceFound.update(args)
          resolve(newDevice.dataValues)

          touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: await instanceToSharedIds(environmentFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Environment)
      )
    },
    moveDevice(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        Device,
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

          const targetEnvironment = await Environment.find({
            where: { id: args.newEnvironmentId },
          })

          const isOwnerOfTargetEnvironment =
            (await instanceToRole(targetEnvironment, userFound)) === "OWNER"

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
            StringPlotValue,
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
              ...(await instanceToSharedIds(environmentFound)),
              ...(await instanceToSharedIds(targetEnvironment)),
            ],
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Environment)
      )
    },
    resetOnlineState(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        Device,
        User,
        2,
        async (resolve, reject, deviceFound, [_, environmentFound]) => {
          const newDevice = await deviceFound.update({ online: null })
          resolve(newDevice.dataValues)

          touch(Environment, environmentFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: await instanceToSharedIds(environmentFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Environment)
      )
    },
    floatValue: genericValueMutation(
      FloatValue,
      "FloatValue",
      pubsub,
      User,
      Device,
      Environment,
      (args, valueFound, reject) => {
        const expectedNewValue = { ...valueFound.dataValues, ...args }

        if (
          isNotNullNorUndefined(args.boundaries) &&
          args.boundaries.length !== 2
        ) {
          reject(
            "Boundaries should be an array containing min and max ([min, max])"
          )
          return false
        } else if (
          isNotNullNorUndefined(args.boundaries) &&
          args.boundaries[0] >= args.boundaries[1]
        ) {
          reject(
            "The min value should be less than the max value, boundaries should be an array [min, max]"
          )
        } else if (
          isNotNullNorUndefined(expectedNewValue.value) &&
          isNotNullNorUndefined(expectedNewValue.boundaries) &&
          isOutOfBoundaries(expectedNewValue.boundaries, expectedNewValue.value)
        ) {
          reject("value is out of boundaries")
          return false
        } else if (
          expectedNewValue.boundaries === null &&
          (expectedNewValue.tileSize === "WIDE" ||
            expectedNewValue.tileSize === "TALL")
        ) {
          reject(
            "FloatValue with no boundaries cannot have tileSize set to WIDE or TALL"
          )
          return false
        }
        return true
      }
    ),
    stringValue: genericValueMutation(
      StringValue,
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
      BooleanValue,
      "BooleanValue",
      pubsub,
      User,
      Device,
      Environment
    ),
    mapValue: genericValueMutation(
      MapValue,
      "MapValue",
      pubsub,
      User,
      Device,
      Environment
    ),
    plotValue: genericValueMutation(
      PlotValue,
      "PlotValue",
      pubsub,
      User,
      Device,
      Environment
    ),
    stringPlotValue: genericValueMutation(
      StringPlotValue,
      "StringPlotValue",
      pubsub,
      User,
      Device,
      Environment
    ),
    incrementFloatValue: (root, args, context) =>
      authorized(
        args.id,
        context,
        FloatValue,
        User,
        2,
        async (resolve, reject, valueFound, [_, environmentFound]) => {
          if (
            isNotNullNorUndefined(valueFound.boundaries) &&
            isOutOfBoundaries(
              valueFound.boundaries,
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
            userIds: await instanceToSharedIds(environmentFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Environment)
      ),
    plotNode(root, args, context) {
      return inheritAuthorized(
        args.id,
        PlotNode,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        PlotValue,
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
            userIds: await instanceToSharedIds(environmentFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Environment)
      )
    },
    stringPlotNode(root, args, context) {
      return inheritAuthorized(
        args.id,
        StringPlotNode,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        StringPlotValue,
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
          touch(StringPlotValue, plotValueFound.id, newNode.updatedAt)

          pubsub.publish("stringPlotNodeUpdated", {
            stringPlotNodeUpdated: resolveObj,
            userIds: await instanceToSharedIds(environmentFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Environment)
      )
    },
    createNotification(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        Device,
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

          const newNotification = await Notification.create({
            ...args,
            environmentId: environmentFound.id,
            visualized: [],
            userId: context.auth.userId,
            date: args.date || new Date(),
          })

          // TODO: is this stuff useful?
          deviceFound.addNotification(newNotification)
          newNotification.setDevice(deviceFound)

          environmentFound.addNotification(newNotification)
          newNotification.setEnvironment(environmentFound)

          const {
            visualized,
            content,
            date,
            userId,
            deviceId,
            environmentId,
            id,
          } = newNotification.dataValues

          const resolveValue = {
            id,
            visualized,
            content,
            date,
            user: { id: userId },
            device: { id: deviceId },
            environment: { id: environmentId },
          }

          resolve(resolveValue)

          touch(Environment, environmentFound.id, newNotification.updatedAt)
          touch(Device, newNotification.deviceId, newNotification.updatedAt)

          const deviceSharedIds = await instanceToSharedIds(environmentFound)
          pubsub.publish("notificationCreated", {
            notificationCreated: resolveValue,
            userIds: deviceSharedIds,
          })

          // the notificationCount props are updated so send the device and environment subscriptions
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: deviceId,
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
            const notificationSubscriptions = await WebPushSubscription.findAll(
              {
                where: {
                  userId: {
                    [Op.in]: deviceSharedIds,
                  },
                },
              }
            )

            notificationSubscriptions.map(notificationSubscription =>
              webpush.sendNotification(
                {
                  endpoint: notificationSubscription.endpoint,
                  expirationTime: notificationSubscription.expirationTime,
                  keys: {
                    p256dh: notificationSubscription.p256dh,
                    auth: notificationSubscription.auth,
                  },
                },
                JSON.stringify({
                  content,
                  date,
                  device: deviceFound,
                })
              )
            )
          }
        },
        deviceToParent(Environment)
      )
    },
    notification(root, args, context) {
      return inheritAuthorized(
        args.id,
        Notification,
        User,
        notificationFound => notificationFound.deviceId,
        context,
        Device,
        2,
        async (
          resolve,
          reject,
          notificationFound,
          deviceFound,
          [_, environmentFound]
        ) => {
          if (args.content === "" || args.content === null) {
            reject("content cannot be null or an empty string")
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          }
          const updateQuery = args

          if (updateQuery.visualized === true) {
            updateQuery.visualized =
              notificationFound.visualized.indexOf(context.auth.userId) === -1
                ? [...notificationFound.visualized, context.auth.userId]
                : notificationFound.visualized
          } else if (updateQuery.visualized === false) {
            updateQuery.visualized = notificationFound.visualized.filter(
              id => id !== context.auth.userId
            )
          }

          const {
            date,
            updatedAt,
            visualized,
            content,
            id,
            userId,
            deviceId,
          } = (await notificationFound.update(updateQuery)).dataValues

          const resolveValue = {
            date,
            visualized,
            content,
            id,
            user: { id: userId },
            device: { id: deviceId },
          }

          resolve(resolveValue)

          touch(Environment, environmentFound.id, updatedAt)
          touch(Device, deviceId, updatedAt)

          const deviceSharedIds = await instanceToSharedIds(environmentFound)
          pubsub.publish("notificationUpdated", {
            notificationUpdated: resolveValue,
            userIds: deviceSharedIds,
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Environment)
      )
    },
    deleteNotification(root, args, context) {
      return inheritAuthorized(
        args.id,
        Notification,
        User,
        notificationFound => notificationFound.deviceId,
        context,
        Device,
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

          const deviceSharedIds = await instanceToSharedIds(environmentFound)
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
            userIds: await instanceToSharedIds(environmentFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Environment)
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
          StringPlotValue,
        },
        User,
        3,
        async (resolve, reject, valueFound, [_, environmentFound]) => {
          const authorizedUsersIds = await instanceToSharedIds(environmentFound)

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
        Device,
        User,
        3,
        async (resolve, reject, deviceFound, [_, environmentFound]) => {
          const authorizedUsersIds = await instanceToSharedIds(environmentFound)

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
              [StringPlotValue, "valueDeleted"],
              [PlotNode, "plotNodeDeleted"],
              [StringPlotNode, "stringPlotNodeDeleted"],
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
        deviceToParent(Environment)
      ),
    deleteEnvironment: (root, args, context) =>
      authorized(
        args.id,
        context,
        Environment,
        User,
        3,
        async (
          resolve,
          reject,
          environmentFound,
          environmentAndParents,
          userFound
        ) => {
          const authorizedUsersIds = await instanceToSharedIds(environmentFound)
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
                [FloatValue, "valueDeleted"],
                [StringValue, "valueDeleted"],
                [BooleanValue, "valueDeleted"],
                [MapValue, "valueDeleted"],
                [PlotValue, "valueDeleted"],
                [StringPlotValue, "valueDeleted"],
                [PlotNode, "plotNodeDeleted"],
                [StringPlotNode, "stringPlotNodeDeleted"],
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
            const environmentSharesFound = await PendingEnvironmentShare.findAll(
              {
                where: { environmentId: environmentFound.id },
              }
            )

            await Promise.all(
              environmentSharesFound.map(async environmentShare => {
                pubsub.publish("environmentShareRevoked", {
                  environmentShareRevoked: environmentShare.id,
                  userIds: [environmentShare.receiverId],
                })
                await environmentShare.destroy()
              })
            )
          }
          async function destroyPendingOwnerChange() {
            const ownerChangesFound = await PendingOwnerChange.findAll({
              where: { environmentId: environmentFound.id },
            })

            await Promise.all(
              ownerChangesFound.map(async ownerChange => {
                pubsub.publish("ownerChangeRevoked", {
                  ownerChangeRevoked: ownerChange.id,
                  userId: ownerChange.receiverId,
                })
                await ownerChange.destroy()
              })
            )
          }

          await Promise.all([
            ...deleteDevicesPromises,
            destroyPendingEnvironmentShare(),
            destroyPendingOwnerChange(),
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
        PlotNode,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        PlotValue,
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

          const authorizedUsersIds = await instanceToSharedIds(environmentFound)
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
        valueToParent(Environment)
      )
    },
    deleteStringPlotNode(root, args, context) {
      return inheritAuthorized(
        args.id,
        StringPlotNode,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        StringPlotValue,
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
          touch(StringPlotValue, plotNodeFound.plotId)

          const authorizedUsersIds = await instanceToSharedIds(environmentFound)
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
          pubsub.publish("stringPlotNodeDeleted", {
            stringPlotNodeDeleted: args.id,
            userIds: authorizedUsersIds,
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Environment)
      )
    },
    deleteUser: (root, args, context) =>
      authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          const environmentsFound = await Environment.findAll({
            where: { ownerId: userFound.id },
          })

          async function deleteEnvironment(environmentFound) {
            const authorizedUsersIds = await instanceToSharedIds(
              environmentFound
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
                  [StringPlotNode, "stringPlotNodeDeleted"],
                ].map(deleteChild)
              )
              await Promise.all(
                [
                  [FloatValue, "valueDeleted"],
                  [StringValue, "valueDeleted"],
                  [BooleanValue, "valueDeleted"],
                  [MapValue, "valueDeleted"],
                  [PlotValue, "valueDeleted"],
                  [StringPlotValue, "valueDeleted"],
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
              const environmentSharesFound = await PendingEnvironmentShare.findAll(
                {
                  where: { environmentId: environmentFound.id },
                }
              )

              await Promise.all(
                environmentSharesFound.map(async environmentShare => {
                  pubsub.publish("environmentShareRevoked", {
                    environmentShareRevoked: environmentShare.id,
                    userIds: [environmentShare.receiverId],
                  })
                  await environmentShare.destroy()
                })
              )
            }
            async function destroyPendingOwnerChange() {
              const ownerChangesFound = await PendingOwnerChange.findAll({
                where: { environmentId: environmentFound.id },
              })

              await Promise.all(
                ownerChangesFound.map(async ownerChange => {
                  pubsub.publish("ownerChangeRevoked", {
                    ownerChangeRevoked: ownerChange.id,
                    userId: ownerChange.receiverId,
                  })
                  await ownerChange.destroy()
                })
              )
            }

            await Promise.all([
              ...deleteDevicesPromises,
              destroyPendingEnvironmentShare(),
              destroyPendingOwnerChange(),
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

          await Promise.all([
            ...deleteEnvironmentsPromises,
            destroyTokenPromise,
          ])
          await userFound.destroy()

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
