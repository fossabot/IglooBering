import {
  authorized,
  findAllValues,
  authorizedScalarPropsResolvers,
  deviceToParent,
} from "./utilities"
import { Op } from "sequelize"

const QUERY_COST = 1

const DeviceResolver = ({
  Device,
  User,
  Board,
  BooleanValue,
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
      "createdAt",
      "updatedAt",
      "deviceType",
      "name",
      "index",
      "online",
      "signalStatus",
      "batteryStatus",
      "batteryCharging",
      "firmware",
    ],
    deviceToParent(Board)
  ),
  values(root, args, context) {
    return authorized(
      root.id,
      context,
      Device,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const valuesFound = await findAllValues(
          {
            BooleanValue,
            FloatValue,
            StringValue,
            PlotValue,
            StringPlotValue,
            MapValue,
          },
          {
            where: { deviceId: deviceFound.id },
          }
        )

        resolve(valuesFound)

        context.billingUpdater.update(QUERY_COST * valuesFound.length)
      },
      deviceToParent(Board)
    )
  },
  muted(root, args, context) {
    return authorized(
      root.id,
      context,
      Device,
      User,
      1,
      async (resolve, reject, deviceFound, [_, boardFound], userFound) => {
        // the Board resolver will take care of loading the other props,
        // it only needs to know the board id
        resolve(deviceFound.muted || boardFound.muted || userFound.quietMode)

        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent(Board)
    )
  },
  board(root, args, context) {
    return authorized(
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
      deviceToParent(Board)
    )
  },
  notifications(root, args, context) {
    return authorized(
      root.id,
      context,
      Device,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const notifications = await deviceFound.getNotifications()

        // the database returns ISO-format dates, so sorting the strings without casting is fine
        const compareDates = (a, b) =>
          a.date > b.date ? -1 : a.date === b.date ? 0 : 1

        resolve(notifications.sort(compareDates))
        context.billingUpdater.update(QUERY_COST * notifications.length)
      },
      deviceToParent(Board)
    )
  },
  notificationCount(root, args, context) {
    return authorized(
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
      deviceToParent(Board)
    )
  },
})

export default DeviceResolver
