import {
  authenticated,
  logErrorsPromise,
  findValue,
  authorized,
  deviceToParent,
  notificationToParent,
  inheritAuthorized,
  boardToParent,
  valueToParent,
} from "./utilities"
import bcrypt from "bcryptjs"

const QUERY_COST = 1

const QueryResolver = ({
  User,
  Device,
  Board,
  FloatValue,
  StringValue,
  BoolValue,
  PlotValue,
  StringPlotValue,
  MapValue,
  Notification,
  PlotNode,
  StringPlotNode,
}) => ({
  user(root, args, context) {
    return new Promise(
      authenticated(
        context,
        resolve => {
          resolve({ id: context.auth.userId })
          context.billingUpdater.update(QUERY_COST)
        },
        ["TEMPORARY", "PERMANENT", "PASSWORD_RECOVERY"]
      )
    )
  },
  device(root, args, context) {
    return logErrorsPromise(
      "device query",
      105,
      authorized(
        args.id,
        context,
        Device,
        User,
        1,
        async (resolve, reject, deviceFound) => {
          resolve(deviceFound.dataValues)

          context.billingUpdater.update(QUERY_COST)
        },
        deviceToParent(Board)
      )
    )
  },
  board(root, args, context) {
    return logErrorsPromise(
      "board query",
      912,
      authorized(
        args.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          resolve(boardFound.dataValues)
          context.billingUpdater.update(QUERY_COST)
        },
        boardToParent
      )
    )
  },
  value(root, args, context) {
    return logErrorsPromise(
      "value query",
      114,
      authenticated(context, async (resolve, reject) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })
        const valueFound = await findValue(
          {
            BoolValue,
            FloatValue,
            StringValue,
            PlotValue,
            StringPlotValue,
            MapValue,
          },
          Device,
          Board,
          { where: { id: args.id } },
          userFound
        ).catch(e => reject(e))

        resolve(valueFound)
        context.billingUpdater.update(QUERY_COST)
      })
    )
  },
  notification(root, args, context) {
    return logErrorsPromise(
      "notificationQuery",
      300,
      inheritAuthorized(
        args.id,
        Notification,
        User,
        notificationFound => notificationFound.deviceId,
        context,
        Device,
        1,
        async (resolve, reject, notificationFound) => {
          resolve(notificationFound)
          context.billingUpdater.update(QUERY_COST)
        },
        deviceToParent(Board)
      )
    )
  },
  plotNode(root, args, context) {
    return logErrorsPromise(
      "plotNode query",
      1390,
      inheritAuthorized(
        args.id,
        PlotNode,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        PlotValue,
        1,
        async (resolve, reject, plotNodeFound) => {
          resolve(plotNodeFound)
        },
        valueToParent(Board)
      )
    )
  },
  stringPlotNode(root, args, context) {
    return logErrorsPromise(
      "stringPlotNode query",
      1390,
      inheritAuthorized(
        args.id,
        StringPlotNode,
        User,
        plotNodeFound => plotNodeFound.plotId,
        context,
        StringPlotValue,
        1,
        async (resolve, reject, plotNodeFound) => {
          resolve(plotNodeFound)
        },
        valueToParent(Board)
      )
    )
  },
})

export default QueryResolver
