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
  Environment,
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
    deviceToParent(Environment)
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
      deviceToParent(Environment)
    )
  },
  muted(root, args, context) {
    return authorized(
      root.id,
      context,
      Device,
      User,
      1,
      async (
        resolve,
        reject,
        deviceFound,
        [_, environmentFound],
        userFound
      ) => {
        // the Environment resolver will take care of loading the other props,
        // it only needs to know the environment id
        resolve(
          deviceFound.muted || environmentFound.muted || userFound.quietMode
        )

        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent(Environment)
    )
  },
  environment(root, args, context) {
    return authorized(
      root.id,
      context,
      Device,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        // the Environment resolver will take care of loading the other props,
        // it only needs to know the environment id
        resolve({ id: deviceFound.environmentId })

        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent(Environment)
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
        const notifications = await Notification.findAll({
          where: { deviceId: deviceFound.id },
        })

        // the database returns ISO-format dates, so sorting the strings without casting is fine
        const compareDates = (a, b) =>
          a.date > b.date ? -1 : a.date === b.date ? 0 : 1

        resolve(notifications.sort(compareDates))
        context.billingUpdater.update(QUERY_COST * notifications.length)
      },
      deviceToParent(Environment)
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
      deviceToParent(Environment)
    )
  },
})

export default DeviceResolver
