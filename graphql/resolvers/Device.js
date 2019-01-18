import {
  authorized,
  findAllValues,
  authorizedScalarPropsResolvers,
  deviceToParent,
  instanceToRole,
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
  CategoryPlotValue,
  MapValue,
  Notification,
  joinTables,
}) => ({
  ...authorizedScalarPropsResolvers(
    "deviceLoaderById",
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
    deviceToParent
  ),
  values(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const valuesFound = await findAllValues(
          {
            BooleanValue,
            FloatValue,
            StringValue,
            PlotValue,
            CategoryPlotValue,
            MapValue,
          },
          {
            where: { deviceId: deviceFound.id },
            limit: args.eachTypeLimit,
            offset: args.eachTypeOffset,
            order: [["id", "DESC"]],
          }
        )

        resolve(valuesFound)

        context.billingUpdater.update(QUERY_COST * valuesFound.length)
      },
      deviceToParent
    )
  },
  muted(root, args, context) {
    return authorized(
      root.id,
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
        // the Environment resolver will take care of loading the other props,
        // it only needs to know the environment id
        resolve(
          deviceFound.muted || environmentFound.muted || userFound.quietMode
        )

        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
  environment(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        // the Environment resolver will take care of loading the other props,
        // it only needs to know the environment id
        resolve({ id: deviceFound.environmentId })

        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
  notifications(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const notifications = await Notification.findAll({
          where: { deviceId: deviceFound.id },
          limit: args.limit,
          offset: args.offset,
          order: [["id", "DESC"]],
        })

        // the database returns ISO-format dates, so sorting the strings without casting is fine
        const compareDates = (a, b) =>
          a.date > b.date ? -1 : a.date === b.date ? 0 : 1

        resolve(notifications.sort(compareDates))
        context.billingUpdater.update(QUERY_COST * notifications.length)
      },
      deviceToParent
    )
  },
  notificationCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const count = await Notification.count({
          where: {
            deviceId: root.id,
            notRead: { [Op.contains]: [context.auth.userId] },
          },
        })

        resolve(count)
      },
      deviceToParent
    )
  },
  myRole(root, args, context) {
    return authorized(
      root.id,
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
        const myRole = await instanceToRole(
          environmentFound,
          userFound,
          context
        )

        resolve(myRole)
      },
      deviceToParent
    )
  },
})

export default DeviceResolver
