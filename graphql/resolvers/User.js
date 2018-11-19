import {
  authenticated,
  logErrorsPromise,
  findAllValues,
  getAll,
} from "./utilities"
import { Op } from "sequelize"

const QUERY_COST = 1

const retrieveUserScalarProp = (User, prop, acceptedTokens) => (
  root,
  args,
  context
) =>
  logErrorsPromise(
    "retrieveScalarProp",
    106,
    authenticated(
      context,
      async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const userFound = await User.find({ where: { id: root.id } })
          if (!userFound) {
            reject("User doesn't exist. Use `SignupUser` to create one")
          } else {
            resolve(userFound[prop])
          }
        }
      },
      acceptedTokens
    )
  )

const scalarProps = (User, props) =>
  props.reduce((acc, prop) => {
    acc[prop] = retrieveUserScalarProp(User, prop)
    return acc
  }, {})

const retrievePublicUserScalarProp = (User, prop, acceptedTokens) => (
  root,
  args,
  context
) =>
  logErrorsPromise(
    "retrieveScalarProp",
    106,
    authenticated(
      context,
      async (resolve, reject) => {
        const userFound = await User.find({ where: { id: root.id } })
        if (!userFound) {
          reject("User doesn't exist. Use `SignupUser` to create one")
        } else {
          resolve(userFound[prop])
        }
      },
      acceptedTokens
    )
  )

const UserResolver = ({
  User,
  PermanentToken,
  Device,
  Board,
  FloatValue,
  StringValue,
  BoolValue,
  PlotValue,
  StringPlotValue,
  MapValue,
  Notification,
}) => ({
  ...scalarProps(User, [
    "createdAt",
    "updatedAt",
    "quietMode",
    "devMode",
    "monthUsage",
    "emailIsVerified",
  ]),
  email: retrievePublicUserScalarProp(User, "email", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  fullName: retrievePublicUserScalarProp(User, "fullName", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  profileIcon: retrievePublicUserScalarProp(User, "profileIcon", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  profileIconColor: retrievePublicUserScalarProp(User, "profileIconColor", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  paymentPlan: retrieveUserScalarProp(User, "paymentPlan", [
    "TEMPORARY",
    "PERMANENT",
    "SWITCH_TO_PAYING",
  ]),
  usageCap: retrieveUserScalarProp(User, "usageCap", [
    "TEMPORARY",
    "PERMANENT",
    "CHANGE_USAGE_CAP",
  ]),
  settings(root, args, context) {
    return logErrorsPromise(
      "User devices resolver",
      107,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const userFound = await User.find({ where: { id: root.id } })

          resolve({
            timeZone: userFound.settings_timeZone,
            language: userFound.settings_language,
            lengthAndMass: userFound.settings_lengthAndMass,
            temperature: userFound.settings_temperature,
            dateFormat: userFound.settings_dateFormat,
            timeFormat: userFound.settings_timeFormat,
          })
          context.billingUpdater.update(QUERY_COST)
        }
      })
    )
  },
  devices(root, args, context) {
    return logErrorsPromise(
      "User devices resolver",
      107,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const devicesInheritedByBoards = await getAll(Board, User, root.id, [
            { model: Device },
          ])

          const devices = devicesInheritedByBoards.reduce(
            (acc, curr) => [...acc, ...curr.devices],
            []
          )

          resolve(devices)
          context.billingUpdater.update(QUERY_COST * devices.length)
        }
      })
    )
  },
  boardsCount(root, args, context) {
    return logErrorsPromise(
      "User boards resolver",
      904,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          // TODO: use count query instead
          const boards = await getAll(Board, User, root.id)

          resolve(boards.length)
          context.billingUpdater.update(QUERY_COST)
        }
      })
    )
  },
  boards(root, args, context) {
    return logErrorsPromise(
      "User boards resolver",
      904,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const boards = await getAll(Board, User, root.id)

          resolve(boards)
          context.billingUpdater.update(QUERY_COST * boards.length)
        }
      })
    )
  },
  notificationsCount(root, args, context) {
    return logErrorsPromise(
      "notificationsCount UserResolver",
      925,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const devicesInheritedByBoards = await getAll(Board, User, root.id, [
            { model: Device, include: [{ model: Notification }] },
          ])

          // flattens the notifications
          const allNotifications = devicesInheritedByBoards.reduce(
            (acc, board) => [
              ...acc,
              ...board.devices.reduce(
                (acc, device) => [...acc, ...device.notifications],
                []
              ),
            ],
            []
          )

          // count not visualized notifications
          const totalCount = allNotifications.filter(
            notification =>
              notification.visualized.indexOf(context.auth.userId) === -1
          ).length

          resolve(totalCount)
          context.billingUpdater.update(QUERY_COST)
        }
      })
    )
  },
  notifications(root, args, context) {
    return logErrorsPromise(
      "User devices resolver",
      119,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const devicesInheritedByBoards = await getAll(Board, User, root.id, [
            { model: Device, include: [{ model: Notification }] },
          ])

          // flattens the notifications
          const allNotifications = devicesInheritedByBoards.reduce(
            (acc, board) => [
              ...acc,
              ...board.devices.reduce(
                (acc, device) => [...acc, ...device.notifications],
                []
              ),
            ],
            []
          )

          resolve(allNotifications)
          context.billingUpdater.update(QUERY_COST * allNotifications.length)
        }
      })
    )
  },
  values(root, args, context) {
    return logErrorsPromise(
      "User values resolver",
      108,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          // TODO: tag the values with the right __resolveType
          const valueModels = [
            FloatValue,
            StringValue,
            BoolValue,
            PlotValue,
            StringPlotValue,
            MapValue,
          ]

          const valuesInheritedFromBoards = await getAll(Board, User, root.id, [
            {
              model: Device,
              include: valueModels.map(Model => ({ model: Model })),
            },
          ])
          const flattenedAllValues = valuesInheritedFromBoards.reduce(
            (acc, curr) => [
              ...acc,
              ...curr.devices.reduce(
                (acc, device) => [
                  ...acc,
                  ...device.floatValues,
                  ...device.stringValues,
                  ...device.boolValues,
                  ...device.plotValues,
                  ...device.stringPlotValues,
                  ...device.mapValues,
                ],
                []
              ),
            ],
            []
          )

          resolve(flattenedAllValues)
          context.billingUpdater.update(QUERY_COST * flattenedAllValues.length)
        }
      })
    )
  },
  permanentTokens(root, args, context) {
    return logErrorsPromise(
      "user permanentTokens",
      127,
      authenticated(context, async (resolve, reject) => {
        if (context.auth.userId !== root.id) {
          reject("You are not allowed to access details about this user")
        } else {
          const tokens = await PermanentToken.findAll({
            where: { userId: root.id },
          })

          resolve(tokens)
          context.billingUpdater.update(QUERY_COST * tokens.length)
        }
      })
    )
  },
})

export default UserResolver
