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
  logErrorsPromise,
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
  GenerateUserBillingBatcher,
  boardToParent,
} from "./utilities"
import webpush from "web-push"
import Stripe from "stripe"
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

const MutationResolver = (
  {
    User,
    PermanentToken,
    Device,
    Board,
    FloatValue,
    StringValue,
    BoolValue,
    MapValue,
    PlotValue,
    PlotNode,
    StringPlotValue,
    StringPlotNode,
    Notification,
  },
  WebPushSubscription,
  pubsub,
  JWT_SECRET
) => {
  const resolvers = {
    // checks if the user exists, if so
    // compares the given password with the hash
    // and returns an access token
    AuthenticateUser(root, args, context) {
      return logErrorsPromise(
        "AuthenticateUser",
        103,
        async (resolve, reject) => {
          const userFound = await User.find({
            where: { email: args.email },
          })
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
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
              id: userFound.dataValues.id,
              token: generateAuthenticationToken(
                userFound.dataValues.id,
                JWT_SECRET
              ),
              user: userFound,
            })
          } else if (
            check2FCode(args.twoFactorCode, userFound.twoFactorSecret)
          ) {
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
              id: userFound.dataValues.id,
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
      )
    },
    SendPasswordRecoveryEmail(root, args, context) {
      return logErrorsPromise(
        "SendPasswordRecoveryEmail",
        901,
        async (resolve, reject) => {
          const userFound = await User.find({
            where: { email: args.email },
          })
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
          } else {
            sendPasswordRecoveryEmail(userFound.email, userFound.id)

            resolve(true)
          }
        }
      )
    },
    GeneratePermanentAccessToken(root, args, context) {
      return logErrorsPromise(
        "GeneratePermanentAccessToken",
        125,
        authenticated(context, async (resolve, reject) => {
          if (args.customName === "") {
            reject("Empty name is not allowed")
          } else {
            const databaseToken = await PermanentToken.create({
              customName: args.customName,
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
              customName: databaseToken.customName,
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
        })
      )
    },

    DeletePermanentAccesToken(root, args, context) {
      return logErrorsPromise(
        "DeletePermanentAccesToken",
        126,
        authenticated(context, async (resolve, reject) => {
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
      )
    },
    // checks if a user with that email already exists
    // if not it creates one and returnes an access token
    SignupUser(root, args, context) {
      return logErrorsPromise("SignupUser", 102, async (resolve, reject) => {
        // check password strength
        const zxcvbnDictionary = [
          args.email,
          args.email.split("@")[0],
          args.fullName,
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

        if (!args.fullName) {
          reject("fullName required")
          return
        }

        const userFound = await User.find({
          where: { email: args.email },
        })
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
              fullName: args.fullName,
              profileIconColor: randomUserIconColor(),
              settings_language: "en-GB",
              settings_timeZone: "+00:00_Greenwich", // TODO: Daylight Saving Time
              settings_lengthAndMass: "SI",
              settings_temperature: "CELSIUS",
              settings_dateFormat: "DMY",
              settings_timeFormat: "H24",
            })

            const newBoard = await Board.create({
              customName: "Home",
              avatar: randomBoardAvatar(),
              quietMode: false,
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
              id: newUser.dataValues.id,
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
      })
    },
    UpgradeTo2FactorAuthentication(root, args, context) {
      return logErrorsPromise(
        "UpgradeTo2FactorAuthentication",
        118,
        authenticated(context, async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          /* istanbul ignore if - should ever happen */
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
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
      )
    },
    // changes the password and returns an access token
    ChangePassword(root, args, context) {
      return logErrorsPromise(
        "ChangePassword",
        101,
        authenticated(
          context,
          async (resolve, reject) => {
            const userFound = await User.find({
              where: { id: context.auth.userId },
            })
            if (!userFound) {
              reject("User doesn't exist. Use `SignupUser` to create one")
            } else {
              const encryptedPass = bcrypt.hashSync(
                args.newPassword,
                SALT_ROUNDS
              )

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
      )
    },
    ResendVerificationEmail(root, args, context) {
      return logErrorsPromise(
        "ResendVerificationEmail",
        900,
        authenticated(context, async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
          } else if (userFound.emailIsVerified) {
            reject("This user has already verified their email")
          } else {
            resolve(true)
            sendVerificationEmail(userFound.email, userFound.id)
          }
        })
      )
    },
    shareBoard: (root, args, context) =>
      logErrorsPromise(
        "genericShare",
        921,
        authorized(
          args.boardId,
          context,
          Board,
          User,
          3,
          async (resolve, reject, found) => {
            const userFound = await User.find({
              where: { email: args.email },
            })

            if (!userFound) {
              reject("This account doesn't exist, check the email passed")
            } else if (userFound.id === context.auth.userId) {
              reject("You can't share a resource with yourself")
            } else {
              // remove old role
              await Promise.all([
                userFound[`remove${Board.Admins}`](found),
                userFound[`remove${Board.Editors}`](found),
                userFound[`remove${Board.Spectators}`](found),
              ])

              // add new role
              const parsedRole = `${args.role[0] +
                args.role.slice(1).toLowerCase()}s`
              await userFound[`add${Board[parsedRole]}`](found)

              resolve(found)
              context.billingUpdater.update(MUTATION_COST)

              pubsub.publish("boardShared", {
                boardShared: found,
                userId: userFound.id,
              })
            }
          },
          boardToParent
        )
      ),
    stopSharingBoard: (root, args, context) =>
      logErrorsPromise(
        "genericShare",
        921,
        authorized(
          args.boardId,
          context,
          Board,
          User,
          3,
          async (resolve, reject, found, [_, boardFound]) => {
            const userFound = await User.find({
              where: { email: args.email },
            })
            // instanceToRole now accepts an instance not an array
            const role = await instanceToRole(boardFound, userFound)

            if (!userFound) {
              reject("This account doesn't exist, check the email passed")
            } else if (!role) {
              reject("This resource isn't shared with that user")
            } else if (role === "OWNER") {
              reject("You cannot stop sharing a resource with its owner")
            } else {
              // remove old role
              await Promise.all([
                userFound[`remove${Board.Admins}`](found),
                userFound[`remove${Board.Editors}`](found),
                userFound[`remove${Board.Spectators}`](found),
              ])

              resolve(found)
              context.billingUpdater.update(MUTATION_COST)

              pubsub.publish("boardStoppedSharing", {
                boardStoppedSharing: args[idField],
                userId: userFound.id,
              })
            }
          },
          boardToParent
        )
      ),
    CreateBoard(root, args, context) {
      return logErrorsPromise(
        "CreateBoard",
        910,
        authenticated(context, async (resolve, reject) => {
          if (args.customName === "" || args.customName === null) {
            reject("customName cannot be null or an empty string")
            return
          }

          const newBoard = await Board.create({
            ...args,
            avatar: args.avatar || randomBoardAvatar(),
            // if quietMode are not passed then set them to false
            quietMode: !!args.quietMode,
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
      )
    },
    CreateDevice(root, args, context) {
      return logErrorsPromise("CreateDevice", 104, async (resolve, reject) => {
        let boardId

        // if boardId is not specified and there is only one board choose that board
        if (args.boardId) {
          boardId = args.boardId
        } else {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          // TODO: is it possible to just use a count or at least fetch only the ids?
          const boards = await userFound.getOwnBoards()

          if (boards.length === 1) {
            boardId = boards[0].id
          } else if (board.length === 0) {
            reject("To create a device you need to have at least 1 board")
            return
          } else {
            reject(
              "You need to specify the boardId when the user has more than one board"
            )
            return
          }
        }

        return authorized(
          boardId,
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
            } else if (args.customName === "") {
              reject("Custom name cannot be an empty string")
              return
            } else if (args.quietMode === null) {
              reject("quietMode cannot be null")
              return
            }

            const index =
              args.index !== null && args.index !== undefined
                ? args.index
                : (await Device.max("index", {
                    where: { boardId },
                  })) + 1 || 0 // or 0 replaces NaN when there are no other devices

            const newDevice = await Device.create({
              ...args,
              quietMode: !!args.quietMode,
              boardId,
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

            context.billingUpdater.update(MUTATION_COST)
          },
          boardToParent
        )(resolve, reject)
      })
    },
    CreateFloatValue: CreateGenericValue(
      User,
      Device,
      Board,
      FloatValue,
      "FloatValue",
      [
        FloatValue,
        StringValue,
        BoolValue,
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
    CreateStringValue: CreateGenericValue(
      User,
      Device,
      Board,
      StringValue,
      "StringValue",
      [
        FloatValue,
        StringValue,
        BoolValue,
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
    CreateBooleanValue: CreateGenericValue(
      User,
      Device,
      Board,
      BoolValue,
      "BoolValue",
      [
        FloatValue,
        StringValue,
        BoolValue,
        MapValue,
        PlotValue,
        StringPlotValue,
      ],
      pubsub
    ),
    CreateMapValue: CreateGenericValue(
      User,
      Device,
      Board,
      MapValue,
      "MapValue",
      [
        FloatValue,
        StringValue,
        BoolValue,
        MapValue,
        PlotValue,
        StringPlotValue,
      ],
      pubsub
    ),
    CreatePlotValue: CreateGenericValue(
      User,
      Device,
      Board,
      PlotValue,
      "PlotValue",
      [
        FloatValue,
        StringValue,
        BoolValue,
        MapValue,
        PlotValue,
        StringPlotValue,
      ],
      pubsub
    ),
    CreateStringPlotValue: CreateGenericValue(
      User,
      Device,
      Board,
      StringPlotValue,
      "StringPlotValue",
      [
        FloatValue,
        StringValue,
        BoolValue,
        MapValue,
        PlotValue,
        StringPlotValue,
      ],
      pubsub
    ),
    CreatePlotNode(root, args, context) {
      return logErrorsPromise(
        "CreatePlotNode mutation",
        139,
        authorized(
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
              user: {
                id: plotNode.userId,
              },
              device: {
                id: plotNode.deviceId,
              },
              plot: {
                id: plotNode.plotId,
              },
            }

            resolve(resolveObj)
            pubsub.publish("plotNodeCreated", {
              plotNodeCreated: resolveObj,
              userIds: await instanceToSharedIds(boardFound),
            })

            context.billingUpdater.update(MUTATION_COST)
          },
          valueToParent(Board)
        )
      )
    },
    CreateStringPlotNode(root, args, context) {
      return logErrorsPromise(
        "CreateStringPlotNode mutation",
        139,
        authorized(
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
              user: {
                id: plotNode.userId,
              },
              device: {
                id: plotNode.deviceId,
              },
              plot: {
                id: plotNode.plotId,
              },
            }

            resolve(resolveObj)
            pubsub.publish("stringPlotNodeCreated", {
              stringPlotNodeCreated: resolveObj,
              userIds: await instanceToSharedIds(boardFound),
            })
            context.billingUpdater.update(MUTATION_COST)
          },
          valueToParent(Board)
        )
      )
    },
    user(root, args, context) {
      return logErrorsPromise("user mutation", 115, (resolve, reject) => {
        if (args.fullName === null || args.fullName === "") {
          reject("fullName cannot be null or empty")
          return
        } else if (args.email === null) {
          reject("Email cannot be set to null")
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
              reject("User doesn't exist. Use `SignupUser` to create one")
            } else {
              if (args.email) {
                const sameEmailUserFound = await User.find({
                  where: { email: args.email },
                })
                if (sameEmailUserFound) {
                  reject("A user with this email already exists")
                  return
                }
              }

              try {
                const updateObj = args.email
                  ? { ...args, emailIsVerified: false }
                  : args
                const newUser = await userFound.update(updateObj)
                resolve(newUser.dataValues)

                pubsub.publish("userUpdated", {
                  userUpdated: newUser.dataValues,
                  userId: context.auth.userId,
                })

                if (args.email) {
                  sendVerificationEmail(args.email, newUser.id)
                }

                // if the token used for the mutation is not a usageCap update or paymentPlan update bill it
                if (permissionRequired === undefined) {
                  context.billingUpdater.update(MUTATION_COST)
                }
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
          },
          permissionRequired
        )(resolve, reject)
      })
    },
    settings(root, args, context) {
      return logErrorsPromise(
        "settings Mutation",
        1834,
        authenticated(context, async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
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
      )
    },
    updatePaymentInfo(root, args, context) {
      return logErrorsPromise(
        "updatePaymentInfo",
        500,
        authenticated(context, async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
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
      )
    },
    board(root, args, context) {
      return logErrorsPromise(
        "board mutation",
        911,
        authorized(
          args.id,
          context,
          Board,
          User,
          2,
          async (resolve, reject, boardFound, _, userFound) => {
            if (args.customName === "" || args.customName === null) {
              reject("customName cannot be null or an empty string")
              return
            } else if (
              userFound.quietMode &&
              isNotNullNorUndefined(args.quietMode)
            ) {
              reject(
                "Cannot change quietMode at board level when it is enabled at user level"
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
      )
    },
    device(root, args, context) {
      const authorizationNeeded = isNotNullNorUndefined(args.boardId) ? 4 : 2
      return logErrorsPromise(
        "device mutation",
        116,
        authorized(
          args.id,
          context,
          Device,
          User,
          authorizationNeeded,
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
            } else if (args.customName === null || args.customName === "") {
              reject("customName cannot be null or an empty string")
              return
            } else if (args.quietMode === null) {
              reject("quietMode cannot be null")
              return
            } else if (Object.keys(args).length === 1) {
              reject("You cannot make a mutation with only the id field")
              return
            } else if (args.boardId === null) {
              reject("boardId cannot be set to null")
              return
            } else if (
              (boardFound.quietMode || userFound.quietMode) &&
              isNotNullNorUndefined(args.quietMode)
            ) {
              reject(
                "Cannot change quietMode at device level when it is enabled at board or user level"
              )
              return
            } else if (args.boardId) {
              // devices can be moved only to boards owned by the user

              // uses authorized passing mock reject and resolve, so that if the authorization
              // throws error the resolve value of the promise is false and true otherwise
              const isOwnerOfTargetBoard = await new Promise(resolve =>
                authorized(
                  args.boardId,
                  context,
                  Board,
                  User,
                  4,
                  () => {
                    resolve(true)
                  },
                  boardToParent
                )(() => {}, () => resolve(false))
              )

              if (!isOwnerOfTargetBoard) {
                reject("You can only move devices to boards you own")
                return
              }
            }

            if (
              (deviceFound.batteryStatus >= 10 ||
                deviceFound.batteryStatus === null) &&
              args.batteryStatus !== null &&
              args.batteryStatus < 10
            ) {
              resolvers.CreateNotification(
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
            pubsub.publish("deviceUpdated", {
              deviceUpdated: newDevice.dataValues,
              userIds: await instanceToSharedIds(boardFound),
            })
            context.billingUpdater.update(MUTATION_COST)
          },
          deviceToParent(Board)
        )
      )
    },
    resetOnlineState(root, args, context) {
      return logErrorsPromise(
        "resetOnlineState mutation",
        4000,
        authorized(
          args.deviceId,
          context,
          Device,
          User,
          2,
          async (resolve, reject, deviceFound, [_, boardFound]) => {
            const newDevice = await deviceFound.update({
              online: null,
            })
            resolve(newDevice.dataValues)
            pubsub.publish("deviceUpdated", {
              deviceUpdated: newDevice.dataValues,
              userIds: await instanceToSharedIds(boardFound),
            })
            context.billingUpdater.update(MUTATION_COST)
          },
          deviceToParent(Board)
        )
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
      BoolValue,
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
    plotNode(root, args, context) {
      return logErrorsPromise(
        "CreatePlotNode mutation",
        139,
        inheritAuthorized(
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
              user: {
                id: newNode.dataValues.userId,
              },
              device: {
                id: newNode.dataValues.deviceId,
              },
              plot: {
                id: newNode.dataValues.plotId,
              },
            }
            resolve(resolveObj)
            pubsub.publish("plotNodeUpdated", {
              plotNodeUpdated: resolveObj,
              userIds: await instanceToSharedIds(boardFound),
            })

            context.billingUpdater.update(MUTATION_COST)
          },
          valueToParent(Board)
        )
      )
    },
    stringPlotNode(root, args, context) {
      return logErrorsPromise(
        "stringPlotNode mutation",
        139,
        inheritAuthorized(
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
              user: {
                id: newNode.dataValues.userId,
              },
              device: {
                id: newNode.dataValues.deviceId,
              },
              plot: {
                id: newNode.dataValues.plotId,
              },
            }
            resolve(resolveObj)
            pubsub.publish("stringPlotNodeUpdated", {
              stringPlotNodeUpdated: resolveObj,
              userIds: await instanceToSharedIds(boardFound),
            })

            context.billingUpdater.update(MUTATION_COST)
          },
          valueToParent(Board)
        )
      )
    },
    CreateNotification(root, args, context) {
      return logErrorsPromise(
        "create notification mutation",
        122,
        authorized(
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
              user: {
                id: userId,
              },
              device: {
                id: deviceId,
              },
              board: {
                id: boardId,
              },
            }

            resolve(resolveValue)

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

            if (!userFound.quietMode) {
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
      )
    },
    async notification(root, args, context) {
      const notificationFound = await Notification.find({
        where: { id: args.id },
      })

      if (!notificationFound) {
        // FIXME: reject not defined
        reject("The requested resource does not exist")
      } else {
        return logErrorsPromise(
          "notification mutation",
          123,
          authorized(
            notificationFound.deviceId,
            context,
            Device,
            User,
            2,
            async (resolve, reject, deviceFound, [_, boardFound]) => {
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
                  notificationFound.visualized.indexOf(context.auth.userId) ===
                  -1
                    ? [...notificationFound.visualized, context.auth.userId]
                    : notificationFound.visualized
              } else if (updateQuery.visualized === false) {
                updateQuery.visualized = notificationFound.visualized.filter(
                  id => id !== context.auth.userId
                )
              }

              const {
                date,
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

              const deviceSharedIds = await instanceToSharedIds(boardFound)
              pubsub.publish("notificationUpdated", {
                notificationUpdated: resolveValue,
                userIds: deviceSharedIds,
              })

              context.billingUpdater.update(MUTATION_COST)
            },
            deviceToParent(Board)
          )
        )
      }
    },
    async deleteNotification(root, args, context) {
      const notificationFound = await Notification.find({
        where: { id: args.id },
      })

      if (!notificationFound) {
        throw new Error("The requested resource does not exist")
      } else {
        return logErrorsPromise(
          "notification mutation",
          1001,
          authorized(
            notificationFound.deviceId,
            context,
            Device,
            User,
            2,
            async (resolve, reject, deviceFound, [_, boardFound]) => {
              await notificationFound.destroy()

              resolve(args.id)

              const deviceSharedIds = await instanceToSharedIds(boardFound)
              pubsub.publish("notificationDeleted", {
                notificationDeleted: args.id,
                userIds: deviceSharedIds,
              })

              // the notificationCount props are updated so send the device and board subscriptions
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
        )
      }
    },
    deleteValue: (root, args, context) =>
      logErrorsPromise(
        "delete value",
        124,
        authorizedValue(
          args.id,
          context,
          {
            FloatValue,
            StringValue,
            BoolValue,
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
            context.billingUpdater.update(MUTATION_COST)
          },
          Device,
          Board
        )
      ),
    deleteDevice: (root, args, context) =>
      logErrorsPromise(
        "delete device mutation",
        126,
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
                BoolValue,
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
            context.billingUpdater.update(MUTATION_COST)
          },
          deviceToParent(Board)
        )
      ),
    deleteBoard: (root, args, context) =>
      logErrorsPromise(
        "delete board mutation",
        913,
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
                  BoolValue,
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

            context.billingUpdater.update(MUTATION_COST)
          },
          boardToParent
        )
      ),
    deletePlotNode(root, args, context) {
      return logErrorsPromise(
        "deletePlotNode mutation",
        139,
        inheritAuthorized(
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
            pubsub.publish("plotNodeDeleted", {
              plotNodeDeleted: args.id,
              userIds: await instanceToSharedIds(boardFound),
            })

            context.billingUpdater.update(MUTATION_COST)
          },
          valueToParent(Board)
        )
      )
    },
    deleteStringPlotNode(root, args, context) {
      return logErrorsPromise(
        "CreatePlotNode mutation",
        139,
        inheritAuthorized(
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
            pubsub.publish("plotNodeDeleted", {
              plotNodeDeleted: args.id,
              userIds: await instanceToSharedIds(boardFound),
            })

            context.billingUpdater.update(MUTATION_COST)
          },
          valueToParent(Board)
        )
      )
    },
    deleteUser: (root, args, context) =>
      logErrorsPromise(
        "delete device mutation",
        126,
        authenticated(context, async (resolve, reject) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          if (!userFound) {
            reject("The requested resource does not exist")
          } else if (!bcrypt.compareSync(args.password, userFound.password)) {
            reject("Wrong password")
          } else if (
            !userFound.twoFactorSecret ||
            check2FCode(args.twoFactorCode, userFound.twoFactorSecret)
          ) {
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
          } else {
            reject("Wrong two factor code")
          }
        })
      ),
  }

  return resolvers
}

export default MutationResolver
