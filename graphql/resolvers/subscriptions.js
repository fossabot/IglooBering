import {
  subscriptionFilterOnlyMine,
  subscriptionFilterOwnedOrShared,
  socketToDeviceMap,
  authorized,
  deviceToParent,
  logErrorsPromise,
  instanceToSharedIds,
} from "./utilities"

const subscriptionResolver = (pubsub, { User, Device, Environment }) => ({
  environmentSharedWithYou: subscriptionFilterOnlyMine(
    "environmentSharedWithYou",
    pubsub
  ),
  environmentShareUpdated: subscriptionFilterOnlyMine(
    "environmentShareUpdated",
    pubsub
  ),
  environmentStoppedSharingWithYou: subscriptionFilterOnlyMine(
    "environmentStoppedSharingWithYou",
    pubsub
  ),
  environmentShareDeclined: subscriptionFilterOnlyMine(
    "environmentShareDeclined",
    pubsub
  ),
  environmentShareRevoked: subscriptionFilterOwnedOrShared(
    "environmentShareRevoked",
    pubsub
  ),
  ownerChangeBegan: subscriptionFilterOnlyMine("ownerChangeBegan", pubsub),
  ownerChangeAccepted: subscriptionFilterOnlyMine(
    "ownerChangeAccepted",
    pubsub
  ),
  ownerChangeDeclined: subscriptionFilterOnlyMine(
    "ownerChangeDeclined",
    pubsub
  ),
  ownerChangeRevoked: subscriptionFilterOnlyMine("ownerChangeRevoked", pubsub),
  environmentCreated: subscriptionFilterOnlyMine("environmentCreated", pubsub),
  deviceCreated: subscriptionFilterOwnedOrShared("deviceCreated", pubsub),
  deviceMoved: subscriptionFilterOwnedOrShared("deviceMoved", pubsub),
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
  environmentUpdated: subscriptionFilterOwnedOrShared(
    "environmentUpdated",
    pubsub
  ),
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
  environmentDeleted: subscriptionFilterOwnedOrShared(
    "environmentDeleted",
    pubsub
  ),
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
          async (resolve, reject, deviceFound, [_, environmentFound]) => {
            const newDevice = await deviceFound.update({ online: true })
            const userIds = await instanceToSharedIds(environmentFound)

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
          deviceToParent(Environment)
        )
      ),
  },
})
export default subscriptionResolver
