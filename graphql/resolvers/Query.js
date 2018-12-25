import {
  authenticated,
  findValue,
  authorized,
  deviceToParent,
  notificationToParent,
  inheritAuthorized,
  environmentToParent,
  valueToParent,
} from "./utilities"
import bcrypt from "bcryptjs"

const QUERY_COST = 1

const QueryResolver = ({
  User,
  Device,
  Environment,
  FloatValue,
  StringValue,
  BooleanValue,
  PlotValue,
  CategoryPlotValue,
  MapValue,
  Notification,
  PlotNode,
  CategoryPlotNode,
}) => ({
  user(root, args, context) {
    return authenticated(
      context,
      resolve => {
        resolve({ id: context.auth.userId })
        context.billingUpdater.update(QUERY_COST)
      },
      ["TEMPORARY", "PERMANENT", "PASSWORD_RECOVERY"]
    )
  },
  device(root, args, context) {
    return authorized(
      args.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        resolve(deviceFound.dataValues)

        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent(Environment)
    )
  },
  environment(root, args, context) {
    return authorized(
      args.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        resolve(environmentFound.dataValues)
        context.billingUpdater.update(QUERY_COST)
      },
      environmentToParent
    )
  },
  value(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const userFound = await context.dataLoaders.userLoaderById.load(
        context.auth.userId
      )
      const valueFound = await findValue(
        {
          BooleanValue,
          FloatValue,
          StringValue,
          PlotValue,
          CategoryPlotValue,
          MapValue,
        },
        Device,
        Environment,
        { where: { id: args.id } },
        userFound
      ).catch(e => reject(e))

      resolve(valueFound)
      context.billingUpdater.update(QUERY_COST)
    })
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
      async (resolve, reject, notificationFound) => {
        resolve(notificationFound)
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent(Environment)
    )
  },
  plotNode(root, args, context) {
    return inheritAuthorized(
      args.id,
      context.dataLoaders.plotNodeLoaderById,
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      context.dataLoaders.plotValueLoaderById,
      1,
      async (resolve, reject, plotNodeFound) => {
        resolve(plotNodeFound)
      },
      valueToParent(Environment)
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
      1,
      async (resolve, reject, plotNodeFound) => {
        resolve(plotNodeFound)
      },
      valueToParent(Environment)
    )
  },
})

export default QueryResolver
