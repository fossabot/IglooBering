import {
  authorized,
  logErrorsPromise,
  findAllValues,
  authorizedScalarPropsResolvers,
  deviceToParent,
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
      'quietMode',
      'firmware',
    ],
    deviceToParent(Board),
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
        deviceToParent(Board),
      ),
    )
  },
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
          resolve({ id: deviceFound.boardId })

          context.billingUpdater.update(QUERY_COST)
        },
        deviceToParent(Board),
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
        deviceToParent(Board),
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
        deviceToParent(Board),
      ),
    )
  },
})

export default DeviceResolver
