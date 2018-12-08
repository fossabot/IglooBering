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
  randomBoardAvatar,
  randomUserIconColor,
  instanceToRole,
  authorizationLevel,
  GenerateUserBillingBatcher,
  boardToParent,
  sendBoardSharedEmail,
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
    Board,
    FloatValue,
    StringValue,
    BooleanValue,
    MapValue,
    PlotValue,
    PlotNode,
    StringPlotValue,
    StringPlotNode,
    Notification,
    PendingBoardShare,
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

            const newBoard = await Board.create({
              name: "Home",
              avatar: randomBoardAvatar(),
              muted: false,
              index: 0,
            })

            await newUser.addOwnBoard(newBoard)
            await newBoard.setOwner(newUser)

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
        ["TEMPORARY", "PERMANENT", "PASSWORD_RECOVERY"]
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
    shareBoard: (root, args, context) =>
      authorized(
        args.boardId,
        context,
        Board,
        User,
        3,
        async (resolve, reject, boardFound, _, senderFound) => {
          const receiverFound = await User.find({
            where: { email: args.email },
          })

          if (!receiverFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (receiverFound.id === context.auth.userId) {
            reject("You can't share a resource with yourself")
          } else {
            const role = await instanceToRole(boardFound, receiverFound)
            if (role !== null) {
              reject("The user already has a role on this board")
              return
            }

            // if receiver has already a pending share throw error
            const otherPendingShare = await PendingBoardShare.find({
              where: {
                receiverId: receiverFound.id,
                boardId: args.boardId,
              },
            })
            if (otherPendingShare) {
              reject(`There is already a boardShare pending`)
              return
            }

            let newPendingShare = await PendingBoardShare.create({
              senderId: senderFound.id,
              receiverId: receiverFound.id,
              boardId: boardFound.id,
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
              board: {
                id: newPendingShare.boardId,
              },
              role: newPendingShare.role,
            })
            context.billingUpdater.update(MUTATION_COST)

            touch(Board, args.boardId, newPendingShare.updatedAt)

            pubsub.publish("boardSharedWithYou", {
              boardSharedWithYou: newPendingShare,
              userId: receiverFound.id,
            })
            sendBoardSharedEmail(
              receiverFound.email,
              senderFound.name,
              boardFound.name
            )

            const usersWithAccessIds = (await instanceToSharedIds(
              boardFound
            )).filter(id => id !== receiverFound.id)

            pubsub.publish("boardUpdated", {
              boardUpdated: boardFound,
              userIds: usersWithAccessIds,
            })
          }
        },
        boardToParent
      ),
    pendingBoardShare: (root, args, context) =>
      inheritAuthorized(
        args.id,
        PendingBoardShare,
        User,
        pendingBoardShare => pendingBoardShare.boardId,
        context,
        Board,
        3,
        async (resolve, reject, pendingBoardShareFound, boardFound) => {
          const newPendingBoardShare = await pendingBoardShareFound.update(args)

          resolve(newPendingBoardShare)
          context.billingUpdater.update(MUTATION_COST)

          touch(Board, boardFound.id, newPendingBoardShare.updatedAt)

          pubsub.publish("boardSharedWithYou", {
            boardSharedWithYou: newPendingBoardShare,
            userId: newPendingBoardShare.receiverId,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            boardFound
          )).filter(id => id !== newPendingBoardShare.receiverId)

          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
            userIds: usersWithAccessIds,
          })
        },
        boardToParent
      ),
    acceptPendingBoardShare: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: args.pendingBoardShareId },
        })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingBoardFound.receiverId) {
          reject("You are not the receiver of this board share")
        } else {
          // add new role
          const parsedRole = `${pendingBoardFound.role[0] +
            pendingBoardFound.role.slice(1).toLowerCase()}s`
          const boardFound = await Board.find({
            where: { id: pendingBoardFound.boardId },
          })
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          await userFound[`add${Board[parsedRole]}`](boardFound)

          resolve(boardFound.id)

          touch(Board, boardFound.id)

          await pendingBoardFound.destroy()
          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("boardShareAccepted", {
            boardShareAccepted: boardFound.id,
            userId: userFound.id,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            boardFound
          )).filter(id => id !== context.auth.userId)

          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
            userIds: usersWithAccessIds,
          })
        }
      }),
    declinePendingBoardShare: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: args.pendingBoardShareId },
        })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingBoardFound.receiverId) {
          reject("You are not the receiver of this board share")
        } else {
          const pendingBoardFoundId = pendingBoardFound.id
          const boardFound = await Board.find({
            where: { id: pendingBoardFound.boardId },
          })

          await pendingBoardFound.destroy()

          resolve(pendingBoardFoundId)

          touch(Board, boardFound.id)

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("boardShareDeclined", {
            boardShareDeclined: pendingBoardFoundId,
            userId: context.auth.userId,
          })

          const usersWithAccessIds = await instanceToSharedIds(boardFound)

          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
            userIds: usersWithAccessIds,
          })
        }
      }),
    revokePendingBoardShare: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: args.pendingBoardShareId },
        })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else {
          const boardFound = await Board.find({
            where: { id: pendingBoardFound.boardId },
          })
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          if ((await authorizationLevel(boardFound, userFound)) < 3) {
            reject("You are not authorized to perform this operation")
          } else {
            const revokedId = pendingBoardFound.id
            const receiverId = pendingBoardFound.receiverId
            await pendingBoardFound.destroy()

            resolve(revokedId)

            touch(Board, boardFound.id)

            const usersWithAccessIds = await instanceToSharedIds(boardFound)

            pubsub.publish("boardShareRevoked", {
              boardShareRevoked: revokedId,
              userId: receiverId,
            })
            pubsub.publish("boardUpdated", {
              boardUpdated: boardFound,
              userIds: usersWithAccessIds,
            })
          }
        }
      }),
    changeOwner: (root, args, context) =>
      authorized(
        args.boardId,
        context,
        Board,
        User,
        4,
        async (resolve, reject, boardFound, _, formerOwnerFound) => {
          const newOwnerFound = await User.find({
            where: { email: args.email },
          })

          if (!newOwnerFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (newOwnerFound.id === context.auth.userId) {
            reject("You already are the owner of this board")
          } else {
            // if the board already has a pending owner change remove it
            const otherOwnerChange = await PendingOwnerChange.find({
              where: {
                boardId: args.boardId,
              },
            })
            if (otherOwnerChange) {
              await otherOwnerChange.destroy()
            }

            let newOwnerChange = await PendingOwnerChange.create({
              formerOwnerId: formerOwnerFound.id,
              newOwnerId: newOwnerFound.id,
              boardId: boardFound.id,
            })

            resolve({
              id: newOwnerChange.id,
              formerOwner: {
                id: newOwnerChange.formerOwnerId,
              },
              newOwner: {
                id: newOwnerChange.newOwnerId,
              },
              board: {
                id: newOwnerChange.boardId,
              },
            })
            context.billingUpdater.update(MUTATION_COST)

            touch(Board, args.boardId, newOwnerChange.updatedAt)

            pubsub.publish("ownerChangeBegan", {
              ownerChangeBegan: newOwnerChange,
              userId: newOwnerFound.id,
            })
            sendBoardSharedEmail(
              newOwnerFound.email,
              formerOwnerFound.name,
              boardFound.name
            )

            const usersWithAccessIds = (await instanceToSharedIds(
              boardFound
            )).filter(id => id !== newOwnerFound.id)

            pubsub.publish("boardUpdated", {
              boardUpdated: boardFound,
              userIds: usersWithAccessIds,
            })
          }
        },
        boardToParent
      ),
    revokePendingOwnerChange: (root, args, context) =>
      inheritAuthorized(
        args.pendingOwnerChangeId,
        PendingOwnerChange,
        User,
        pendingBoardShare => pendingBoardShare.boardId,
        context,
        Board,
        3,
        async (resolve, reject, pendingOwnerChangeFound, boardFound) => {
          const targetUserId = pendingOwnerChangeFound.newOwnerId
          await pendingOwnerChangeFound.destroy()

          resolve(args.pendingOwnerChangeId)

          touch(Board, boardFound.id)

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("ownerChangeRevoked", {
            ownerChangeRevoked: args.pendingOwnerChangeId,
            userId: targetUserId,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            boardFound
          )).filter(id => id !== targetUserId)

          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
            userIds: usersWithAccessIds,
          })
        },
        boardToParent
      ),
    acceptPendingOwnerChange: (root, args, context) =>
      authenticated(context, async (resolve, reject) => {
        const pendingOwnerChangeFound = await PendingOwnerChange.find({
          where: { id: args.pendingOwnerChangeId },
        })

        if (!pendingOwnerChangeFound) {
          reject("The requested resource does not exist")
        } else if (context.auth.userId !== pendingOwnerChangeFound.newOwnerId) {
          reject("You are not the receiver of this owner change")
        } else {
          const boardFound = await Board.find({
            where: { id: pendingOwnerChangeFound.boardId },
          })
          const userFound = await User.find({
            where: { id: pendingOwnerChangeFound.newOwnerId },
          })

          // remove old roles
          await Promise.all([
            userFound[`remove${Board.Admins}`](boardFound),
            userFound[`remove${Board.Editors}`](boardFound),
            userFound[`remove${Board.Spectators}`](boardFound),
          ])

          await boardFound.setOwner(userFound)
          await userFound.addOwnBoard(boardFound)

          const oldOwnerFound = await User.find({
            where: { id: pendingOwnerChangeFound.formerOwnerId },
          })
          await oldOwnerFound.removeOwnBoard(boardFound)
          await oldOwnerFound[`add${Board.Admins}`](boardFound)

          resolve(boardFound.id)

          touch(Board, boardFound.id)

          await pendingOwnerChangeFound.destroy()
          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("boardShareAccepted", {
            boardShareAccepted: boardFound.id,
            userId: userFound.id,
          })

          const usersWithAccessIds = (await instanceToSharedIds(
            boardFound
          )).filter(id => id !== context.auth.userId)

          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
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
        } else if (context.auth.userId !== pendingOwnerChangeFound.newOwnerId) {
          reject("You are not the receiver of this owner change")
        } else {
          const targetUserId = pendingOwnerChangeFound.newOwnerId
          await pendingOwnerChangeFound.destroy()

          resolve(args.pendingOwnerChangeId)

          const boardFound = await Board.find({
            where: { id: pendingOwnerChangeFound.boardId },
          })
          touch(Board, args.boardId)

          context.billingUpdater.update(MUTATION_COST)

          pubsub.publish("ownerChangeDeclined", {
            ownerChangeRevoked: args.pendingOwnerChangeId,
            userId: targetUserId,
          })

          // sending boardUpdated subscription also to the target user
          // for ease of handling on the client side
          const usersWithAccessIds = await instanceToSharedIds(boardFound)
          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
            userIds: usersWithAccessIds,
          })
        }
      }),
    changeRole(root, args, context) {
      return authorized(
        args.boardId,
        context,
        Board,
        User,
        3,
        async (resolve, reject, boardFound, _, userFound) => {
          const targetUserFound = await User.find({
            where: { email: args.email },
          })
          const currentRole = await instanceToRole(boardFound, targetUserFound)

          if (!targetUserFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (currentRole === "OWNER") {
            reject("You cannot change the role of the owner")
          } else if (!currentRole) {
            reject(
              "This user doesn't have a role on this board you should use the `shareBoard` mutation"
            )
          } else {
            // remove old role
            await Promise.all([
              targetUserFound[`remove${Board.Admins}`](boardFound),
              targetUserFound[`remove${Board.Editors}`](boardFound),
              targetUserFound[`remove${Board.Spectators}`](boardFound),
            ])

            // add new role
            const parsedRole = `${args.newRole[0] +
              args.newRole.slice(1).toLowerCase()}s`

            await targetUserFound[`add${Board[parsedRole]}`](boardFound)

            resolve(boardFound)

            touch(Board, args.boardId)

            pubsub.publish("boardUpdated", {
              boardUpdated: boardFound,
              userIds: await instanceToSharedIds(boardFound),
            })

            context.billingUpdater.update(MUTATION_COST)
          }
        },
        boardToParent
      )
    },
    leaveBoard(root, args, context) {
      return authorized(
        args.boardId,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound, _, userFound) => {
          if ((await instanceToRole(boardFound, userFound)) === "OWNER") {
            reject("You cannot leave a board that you own")
            return
          }

          await Promise.all([
            userFound[`remove${Board.Admins}`](boardFound),
            userFound[`remove${Board.Editors}`](boardFound),
            userFound[`remove${Board.Spectators}`](boardFound),
          ])
          resolve(boardFound.id)

          touch(Board, args.boardId)

          pubsub.publish("boardStoppedSharingWithYou", {
            boardUpdated: boardFound.id,
            userId: userFound.id,
          })

          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        boardToParent
      )
    },
    stopSharingBoard: (root, args, context) =>
      authorized(
        args.boardId,
        context,
        Board,
        User,
        3,
        async (resolve, reject, boardFound) => {
          const userFound = await User.find({
            where: { email: args.email },
          })
          if (!userFound) {
            reject("This user doesn't exist, check that the email is correct")
            return
          }

          const role = await instanceToRole(boardFound, userFound)

          if (!userFound) {
            reject("This account doesn't exist, check the email passed")
          } else if (!role) {
            reject("This resource isn't shared with that user")
          } else if (userFound.id === context.auth.userId) {
            reject(
              "You cannot stopSharing the board with youself, use the `leaveBoard` mutation instead"
            )
          } else if (role === "OWNER") {
            reject("You cannot stop sharing a resource with its owner")
          } else {
            await Promise.all([
              userFound[`remove${Board.Admins}`](boardFound),
              userFound[`remove${Board.Editors}`](boardFound),
              userFound[`remove${Board.Spectators}`](boardFound),
            ])

            resolve(boardFound)
            context.billingUpdater.update(MUTATION_COST)

            touch(Board, args.boardId)

            pubsub.publish("boardStoppedSharingWithYou", {
              boardStoppedSharingWithYou: args.boardId,
              userId: userFound.id,
            })

            const usersWithAccessIds = await instanceToSharedIds(boardFound)

            pubsub.publish("boardUpdated", {
              boardUpdated: boardFound,
              userIds: usersWithAccessIds,
            })
          }
        },
        boardToParent
      ),
    createBoard(root, args, context) {
      return authenticated(context, async (resolve, reject) => {
        if (args.name === "" || args.name === null) {
          reject("name cannot be null or an empty string")
          return
        }

        const newBoard = await Board.create({
          ...args,
          avatar: args.avatar || randomBoardAvatar(),
          // if muted is not passed then set it to false
          muted: !!args.muted,
          index:
            args.index !== null && args.index !== undefined
              ? args.index
              : (await Board.max("index", {
                  where: { ownerId: context.auth.userId },
                })) + 1 || 0, // or 0 replaces NaN when there are no other devices
        })

        const userFound = await User.find({
          where: { id: context.auth.userId },
        })
        await userFound.addOwnBoard(newBoard)
        await newBoard.setOwner(userFound)

        const resolveValue = {
          ...newBoard.dataValues,
          owner: { id: newBoard.ownerId },
          devices: [],
        }

        pubsub.publish("boardCreated", {
          boardCreated: resolveValue,
          userId: context.auth.userId,
        })

        resolve(resolveValue)

        context.billingUpdater.update(MUTATION_COST)
      })
    },
    createDevice(root, args, context) {
      return async (resolve, reject) => {
        return authorized(
          args.boardId,
          context,
          Board,
          User,
          2,
          async (resolve, reject, boardFound) => {
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
                    where: { boardId: args.boardId },
                  })) + 1 || 0 // or 0 replaces NaN when there are no other devices

            const newDevice = await Device.create({
              ...args,
              muted: !!args.muted,
              boardId: args.boardId,
              index,
            })

            const resolveValue = {
              ...newDevice.dataValues,
              board: newDevice.boardId
                ? {
                    id: newDevice.boardId,
                  }
                : null,
            }

            pubsub.publish("deviceCreated", {
              deviceCreated: resolveValue,
              userIds: await instanceToSharedIds(boardFound),
            })

            resolve(resolveValue)

            touch(Board, boardFound.id, newDevice.createdAt)
            context.billingUpdater.update(MUTATION_COST)
          },
          boardToParent
        )(resolve, reject)
      }
    },
    createFloatValue: CreateGenericValue(
      User,
      Device,
      Board,
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
      Board,
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
      Board,
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
      Board,
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
      Board,
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
      Board,
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
        async (resolve, reject, plotValueFound, [_, boardFound]) => {
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

          touch(Board, boardFound.id, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(PlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("plotNodeCreated", {
            plotNodeCreated: resolveObj,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
      )
    },
    createStringPlotNode(root, args, context) {
      return authorized(
        args.plotId,
        context,
        StringPlotValue,
        User,
        2,
        async (resolve, reject, plotValueFound, [_, boardFound]) => {
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

          touch(Board, boardFound.id, plotNode.createdAt)
          touch(Device, plotValueFound.deviceId, plotNode.createdAt)
          touch(StringPlotValue, plotValueFound.id, plotNode.createdAt)

          pubsub.publish("stringPlotNodeCreated", {
            stringPlotNodeCreated: resolveObj,
            userIds: await instanceToSharedIds(boardFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
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

          userFound.update(updateQuery)

          resolve({
            timeZone: userFound.settings_timeZone,
            language: userFound.settings_language,
            lengthAndMass: userFound.settings_lengthAndMass,
            temperature: userFound.settings_temperature,
            dateFormat: userFound.settings_dateFormat,
            timeFormat: userFound.settings_timeFormat,
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
    board(root, args, context) {
      return authorized(
        args.id,
        context,
        Board,
        User,
        2,
        async (resolve, reject, boardFound, _, userFound) => {
          if (args.name === "" || args.name === null) {
            reject("name cannot be null or an empty string")
            return
          } else if (userFound.quietMode && isNotNullNorUndefined(args.muted)) {
            reject(
              "Cannot change muted at board level when quietMode is enabled at user level"
            )
            return
          } else if (Object.keys(args).length === 1) {
            reject("You cannot make a mutation with only the id field")
            return
          }

          const newBoard = await boardFound.update(args)

          resolve(newBoard.dataValues)
          pubsub.publish("boardUpdated", {
            boardUpdated: newBoard.dataValues,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        boardToParent
      )
    },
    device(root, args, context) {
      return authorized(
        args.id,
        context,
        Device,
        User,
        2,
        async (resolve, reject, deviceFound, [_, boardFound], userFound) => {
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
            (boardFound.muted || userFound.quietMode) &&
            isNotNullNorUndefined(args.muted)
          ) {
            reject(
              "Cannot change muted at device level when it is enabled at board level or quietMode is enabled at user level"
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

          touch(Board, boardFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: await instanceToSharedIds(boardFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Board)
      )
    },
    moveDevice(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        Device,
        User,
        4,
        async (resolve, reject, deviceFound, [_, boardFound], userFound) => {
          if (args.newBoardId === deviceFound.boardId) {
            reject("The device already belongs to this board")
            return
          }

          const targetBoard = await Board.find({
            where: { id: args.newBoardId },
          })

          const isOwnerOfTargetBoard =
            (await instanceToRole(targetBoard, userFound)) === "OWNER"

          if (!isOwnerOfTargetBoard) {
            reject("You can only move devices to boards you own")
            return
          }

          const newDevice = await deviceFound.update({
            boardId: args.newBoardId,
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
                { boardId: newDevice.boardId },
                { where: { deviceId: newDevice.id } }
              )
          )

          await Promise.all(updatePromises)

          resolve(newDevice.dataValues)

          touch(Board, boardFound.id, newDevice.updatedAt)

          pubsub.publish("deviceMoved", {
            deviceMoved: newDevice.dataValues,
            userIds: [
              ...(await instanceToSharedIds(boardFound)),
              ...(await instanceToSharedIds(targetBoard)),
            ],
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Board)
      )
    },
    resetOnlineState(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        Device,
        User,
        2,
        async (resolve, reject, deviceFound, [_, boardFound]) => {
          const newDevice = await deviceFound.update({ online: null })
          resolve(newDevice.dataValues)

          touch(Board, boardFound.id, newDevice.updatedAt)

          pubsub.publish("deviceUpdated", {
            deviceUpdated: newDevice.dataValues,
            userIds: await instanceToSharedIds(boardFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Board)
      )
    },
    floatValue: genericValueMutation(
      FloatValue,
      "FloatValue",
      pubsub,
      User,
      Device,
      Board,
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
      Board,
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
      Board
    ),
    mapValue: genericValueMutation(
      MapValue,
      "MapValue",
      pubsub,
      User,
      Device,
      Board
    ),
    plotValue: genericValueMutation(
      PlotValue,
      "PlotValue",
      pubsub,
      User,
      Device,
      Board
    ),
    stringPlotValue: genericValueMutation(
      StringPlotValue,
      "StringPlotValue",
      pubsub,
      User,
      Device,
      Board
    ),
    incrementFloatValue: (root, args, context) =>
      authorized(
        args.id,
        context,
        FloatValue,
        User,
        2,
        async (resolve, reject, valueFound, [_, boardFound]) => {
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

          Board.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: boardFound.id } }
          )
          Device.update(
            { updatedAt: newValue.updatedAt },
            { where: { id: newValue.deviceId } }
          )

          pubsub.publish("valueUpdated", {
            valueUpdated: { ...resolveObj, __resolveType: "FloatValue" },
            userIds: await instanceToSharedIds(boardFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
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
          [_, boardFound]
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

          touch(Board, boardFound.id, newNode.updatedAt)
          touch(Device, plotValueFound.deviceId, newNode.updatedAt)
          touch(PlotValue, plotValueFound.id, newNode.updatedAt)

          pubsub.publish("plotNodeUpdated", {
            plotNodeUpdated: resolveObj,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
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
          [_, boardFound]
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

          touch(Board, boardFound.id, newNode.updatedAt)
          touch(Device, plotValueFound.deviceId, newNode.updatedAt)
          touch(StringPlotValue, plotValueFound.id, newNode.updatedAt)

          pubsub.publish("stringPlotNodeUpdated", {
            stringPlotNodeUpdated: resolveObj,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
      )
    },
    createNotification(root, args, context) {
      return authorized(
        args.deviceId,
        context,
        Device,
        User,
        2,
        async (resolve, reject, deviceFound, [_, boardFound], userFound) => {
          if (args.content === "" || args.content === null) {
            reject("content cannot be null or an empty string")
            return
          }

          const newNotification = await Notification.create({
            ...args,
            boardId: boardFound.id,
            visualized: [],
            userId: context.auth.userId,
            date: args.date || new Date(),
          })

          // TODO: is this stuff useful?
          deviceFound.addNotification(newNotification)
          newNotification.setDevice(deviceFound)

          boardFound.addNotification(newNotification)
          newNotification.setBoard(boardFound)

          const {
            visualized,
            content,
            date,
            userId,
            deviceId,
            boardId,
            id,
          } = newNotification.dataValues

          const resolveValue = {
            id,
            visualized,
            content,
            date,
            user: { id: userId },
            device: { id: deviceId },
            board: { id: boardId },
          }

          resolve(resolveValue)

          touch(Board, boardFound.id, newNotification.updatedAt)
          touch(Device, newNotification.deviceId, newNotification.updatedAt)

          const deviceSharedIds = await instanceToSharedIds(boardFound)
          pubsub.publish("notificationCreated", {
            notificationCreated: resolveValue,
            userIds: deviceSharedIds,
          })

          // the notificationCount props are updated so send the device and board subscriptions
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: deviceId,
            },
            userIds: deviceSharedIds,
          })
          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound.dataValues,
            userIds: deviceSharedIds,
          })

          context.billingUpdater.update(MUTATION_COST)

          if (!userFound.quietMode && !boardFound.muted && !deviceFound.muted) {
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
        deviceToParent(Board)
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
          [_, boardFound]
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

          touch(Board, boardFound.id, updatedAt)
          touch(Device, deviceId, updatedAt)

          const deviceSharedIds = await instanceToSharedIds(boardFound)
          pubsub.publish("notificationUpdated", {
            notificationUpdated: resolveValue,
            userIds: deviceSharedIds,
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Board)
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
          [_, boardFound]
        ) => {
          await notificationFound.destroy()

          resolve(args.id)

          const deviceSharedIds = await instanceToSharedIds(boardFound)
          pubsub.publish("notificationDeleted", {
            notificationDeleted: args.id,
            userIds: deviceSharedIds,
          })

          // the notificationCount props are updated so send the device and board subscriptions

          //implement touch
          pubsub.publish("deviceUpdated", {
            deviceUpdated: {
              id: deviceFound.id,
            },
            userIds: deviceSharedIds,
          })
          pubsub.publish("boardUpdated", {
            boardUpdated: boardFound.dataValues,
            userIds: await instanceToSharedIds(boardFound),
          })
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Board)
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
        async (resolve, reject, valueFound, [_, boardFound]) => {
          const authorizedUsersIds = await instanceToSharedIds(boardFound)

          // TODO: if value is plot remove nodes
          await valueFound.destroy()

          pubsub.publish("valueDeleted", {
            valueDeleted: args.id,
            userIds: authorizedUsersIds,
          })
          resolve(args.id)
          //implement touch
          context.billingUpdater.update(MUTATION_COST)
        },
        Device,
        Board
      ),
    deleteDevice: (root, args, context) =>
      authorized(
        args.id,
        context,
        Device,
        User,
        3,
        async (resolve, reject, deviceFound, [_, boardFound]) => {
          const authorizedUsersIds = await instanceToSharedIds(boardFound)

          // TODO: send deleted notifications also for children
          const deleteChild = Model =>
            Model.destroy({
              where: {
                deviceId: args.id,
              },
            })

          await Promise.all(
            [
              FloatValue,
              StringValue,
              BooleanValue,
              MapValue,
              PlotValue,
              StringPlotValue,
              PlotNode,
              StringPlotNode,
              Notification,
            ].map(deleteChild)
          )

          await deviceFound.destroy()

          pubsub.publish("deviceDeleted", {
            deviceDeleted: args.id,
            userIds: authorizedUsersIds,
          })

          resolve(args.id)
          //implement touch
          context.billingUpdater.update(MUTATION_COST)
        },
        deviceToParent(Board)
      ),
    deleteBoard: (root, args, context) =>
      authorized(
        args.id,
        context,
        Board,
        User,
        3,
        async (resolve, reject, boardFound, boardAndParents, userFound) => {
          const authorizedUsersIds = await instanceToSharedIds(boardFound)
          const devices = await Device.findAll({
            where: { boardId: boardFound.id },
          })

          // TODO: send deleted notifications for children
          const deleteDevicesPromises = devices.map(async device => {
            const deleteChild = Model =>
              Model.destroy({
                where: {
                  deviceId: device.id,
                },
              })

            await Promise.all(
              [
                FloatValue,
                StringValue,
                BooleanValue,
                MapValue,
                PlotValue,
                StringPlotValue,
                PlotNode,
                Notification,
              ].map(deleteChild)
            )

            await device.destroy()
          })
          await Promise.all(deleteDevicesPromises)

          await boardFound.destroy()

          pubsub.publish("boardDeleted", {
            boardDeleted: args.id,
            userIds: authorizedUsersIds,
          })
          resolve(args.id)

          //implement touch
          context.billingUpdater.update(MUTATION_COST)
        },
        boardToParent
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
          [_, boardFound]
        ) => {
          await plotNodeFound.destroy()

          resolve(args.id)
          //implement touch
          pubsub.publish("plotNodeDeleted", {
            plotNodeDeleted: args.id,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
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
          [_, boardFound]
        ) => {
          await plotNodeFound.destroy()

          resolve(args.id)
          //implement touch
          pubsub.publish("plotNodeDeleted", {
            plotNodeDeleted: args.id,
            userIds: await instanceToSharedIds(boardFound),
          })

          context.billingUpdater.update(MUTATION_COST)
        },
        valueToParent(Board)
      )
    },
    deleteUser: (root, args, context) =>
      authenticated(
        context,
        async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          // after enabling cascade delete in postgres destroying the boards should be enough to clear the user

          // await Board.destroy({
          //   where: {
          //     ownerId: context.auth.userId,
          //   },
          // })

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
