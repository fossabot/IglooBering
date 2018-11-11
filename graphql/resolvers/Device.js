import {
  authorized,
  logErrorsPromise,
  findAllValues,
  authorizedScalarPropsResolvers,
  rolesResolver,
  deviceToParents,
  instanceToRole,
} from './utilities'
import { Op } from 'sequelize'

const QUERY_COST = 1

const DeviceResolver = ({
  Device,
  User,
  Board,
  BoolValue,
  FloatValue,
  StringValue,
  PlotValue,
  StringPlotValue,
  MapValue,
  Notification,
  joinTables,
}) => ({
  ...authorizedScalarPropsResolvers(
    Device,
    User,
    [
      'createdAt',
      'updatedAt',
      'deviceType',
      'customName',
      'icon',
      'index',
      'online',
      'signalStatus',
      'batteryStatus',
      'batteryCharging',
      'firmware',
    ],
    deviceToParents(Board),
  ),
  values(root, args, context) {
    return logErrorsPromise(
      'Device values resolver',
      110,
      authorized(
        root.id,
        context,
        Device,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          const valuesFound = await findAllValues(
            {
              BoolValue,
              FloatValue,
              StringValue,
              PlotValue,
              StringPlotValue,
              MapValue,
            },
            {
              where: { deviceId: deviceFound.id },
            },
          )

          resolve(valuesFound)

          context.billingUpdater.update(QUERY_COST * valuesFound.length)
        },
        deviceToParents(Board),
      ),
    )
  },
  myRole(root, args, context) {
    return logErrorsPromise(
      'myRole BoardResolver',
      902,
      authorized(
        root.id,
        context,
        Device,
        User,
        1,
        async (
          resolve,
          reject,
          deviceFound,
          deviceAndParentFound,
          userFound,
        ) => {
          const myRole = await instanceToRole(deviceAndParentFound, userFound)

          resolve(myRole)
        },
        deviceToParents(Board),
      ),
    )
  },
  owner(root, args, context) {
    return logErrorsPromise(
      'user BoardResolver',
      902,
      authorized(
        root.id,
        context,
        Device,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          resolve({
            id: deviceFound.ownerId,
          })

          context.billingUpdater.update(QUERY_COST)
        },
        deviceToParents(Board),
      ),
    )
  },
  admins: rolesResolver('admin', Device, User, deviceToParents(Board)),
  editors: rolesResolver('editor', Device, User, deviceToParents(Board)),
  spectators: rolesResolver('spectator', Device, User, deviceToParents(Board)),
  board(root, args, context) {
    return logErrorsPromise(
      'Device board resolver',
      903,
      authorized(
        root.id,
        context,
        Device,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          // the Board resolver will take care of loading the other props,
          // it only needs to know the board id
          resolve(deviceFound.boardId ? { id: deviceFound.boardId } : null)

          if (deviceFound.boardId) context.billingUpdater.update(QUERY_COST)
        },
        deviceToParents(Board),
      ),
    )
  },
  notifications(root, args, context) {
    return logErrorsPromise(
      'User devices resolver',
      119,
      authorized(
        root.id,
        context,
        Device,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          const notifications = await deviceFound.getNotifications()

          resolve(notifications)
          context.billingUpdater.update(QUERY_COST * notifications.length)
        },
        deviceToParents(Board),
      ),
    )
  },
  notificationsCount(root, args, context) {
    return logErrorsPromise(
      'notificationsCount device resolver',
      916,
      authorized(
        root.id,
        context,
        Device,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          const count = await Notification.count({
            where: {
              deviceId: root.id,
              [Op.not]: {
                visualized: { [Op.contains]: [context.auth.userId] },
              },
            },
          })

          resolve(count)
        },
        deviceToParents(Board),
      ),
    )
  },
})

export default DeviceResolver
