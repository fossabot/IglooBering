import {
  subscriptionFilterOnlyMine,
  subscriptionFilterOwnedOrShared,
  socketToDeviceMap,
  authorized,
  deviceToParent,
  logErrorsPromise,
  instanceToSharedIds,
} from "./utilities"

const subscriptionResolver = (pubsub, { User, Device, Board }) => ({
  boardSharedWithYou: subscriptionFilterOnlyMine("boardSharedWithYou", pubsub),
  boardStoppedSharingWithYou: subscriptionFilterOnlyMine(
    "boardStoppedSharingWithYou",
    pubsub
  ),
  boardShareDeclined: subscriptionFilterOnlyMine("boardShareDeclined", pubsub),
  boardShareRevoked: subscriptionFilterOwnedOrShared(
    "boardShareRevoked",
    pubsub
  ),
  boardCreated: subscriptionFilterOnlyMine("boardCreated", pubsub),
  deviceCreated: subscriptionFilterOwnedOrShared("deviceCreated", pubsub),
  valueCreated: subscriptionFilterOwnedOrShared("valueCreated", pubsub),
  permanentTokenCreated: subscriptionFilterOnlyMine(
    "permanentTokenCreated",
    pubsub
  ),
  plotNodeCreated: subscriptionFilterOwnedOrShared("plotNodeCreated", pubsub),
  stringPlotNodeCreated: subscriptionFilterOwnedOrShared(
    "stringPlotNodeCreated",
    pubsub
  ),
  notificationCreated: subscriptionFilterOwnedOrShared(
    "notificationCreated",
    pubsub
  ),
  userUpdated: subscriptionFilterOnlyMine("userUpdated", pubsub),
  deviceUpdated: subscriptionFilterOwnedOrShared("deviceUpdated", pubsub),
  boardUpdated: subscriptionFilterOwnedOrShared("boardUpdated", pubsub),
  valueUpdated: subscriptionFilterOwnedOrShared("valueUpdated", pubsub),
  plotNodeUpdated: subscriptionFilterOwnedOrShared("plotNodeUpdated", pubsub),
  stringPlotNodeUpdated: subscriptionFilterOwnedOrShared(
    "stringPlotNodeUpdated",
    pubsub
  ),
  notificationUpdated: subscriptionFilterOwnedOrShared(
    "notificationUpdated",
    pubsub
  ),
  notificationDeleted: subscriptionFilterOwnedOrShared(
    "notificationDeleted",
    pubsub
  ),
  valueDeleted: subscriptionFilterOwnedOrShared("valueDeleted", pubsub),
  deviceDeleted: subscriptionFilterOwnedOrShared("deviceDeleted", pubsub),
  boardDeleted: subscriptionFilterOwnedOrShared("boardDeleted", pubsub),
  userDeleted: subscriptionFilterOnlyMine("userDeleted", pubsub),
  plotNodeDeleted: subscriptionFilterOwnedOrShared("plotNodeDeleted", pubsub),
  stringPlotNodeDeleted: subscriptionFilterOwnedOrShared(
    "stringPlotNodeDeleted",
    pubsub
  ),
  permanentTokenDeleted: subscriptionFilterOnlyMine(
    "permanentTokenDeleted",
    pubsub
  ),
  keepOnline: {
    subscribe: (root, args, context) =>
      logErrorsPromise(
        "keepOnlineSubscription",
        1000,
        authorized(
          args.deviceId,
          context,
          Device,
          User,
          2,
          async (resolve, reject, deviceFound, [_, boardFound]) => {
            const newDevice = await deviceFound.update({ online: true })
            const userIds = await instanceToSharedIds(boardFound)

            pubsub.publish("deviceUpdated", {
              deviceUpdated: newDevice.dataValues,
              userIds,
            })

            socketToDeviceMap[context.websocket] = {
              deviceId: args.deviceId,
              userIds,
            }

            resolve(pubsub.asyncIterator("bogusIterator")) // this iterator will never send any data
          },
          deviceToParent(Board)
        )
      ),
  },
})
export default subscriptionResolver
