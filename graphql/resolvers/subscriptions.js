import {
  subscriptionFilterOnlyMine,
  subscriptionFilterOwnedOrShared,
  socketToDeviceMap,
  authorized,
  deviceToParent,
  logErrorsPromise,
  instanceToSharedIds,
} from "./utilities"
import createDataLoaders from "../../dataloaders/index"

const subscriptionResolver = (pubsub, { User, Device, Environment }) => ({
  pendingEnvironmentShareReceived: subscriptionFilterOnlyMine(
    "pendingEnvironmentShareReceived",
    pubsub,
    createDataLoaders
  ),
  pendingEnvironmentShareUpdated: subscriptionFilterOwnedOrShared(
    "pendingEnvironmentShareUpdated",
    pubsub,
    createDataLoaders
  ),
  environmentStoppedSharingWithYou: subscriptionFilterOnlyMine(
    "environmentStoppedSharingWithYou",
    pubsub,
    createDataLoaders
  ),
  pendingEnvironmentShareAccepted: subscriptionFilterOnlyMine(
    "pendingEnvironmentShareAccepted",
    pubsub,
    createDataLoaders
  ),
  pendingEnvironmentShareDeclined: subscriptionFilterOnlyMine(
    "pendingEnvironmentShareDeclined",
    pubsub,
    createDataLoaders
  ),
  pendingEnvironmentShareRevoked: subscriptionFilterOwnedOrShared(
    "pendingEnvironmentShareRevoked",
    pubsub,
    createDataLoaders
  ),
  pendingOwnerChangeReceived: subscriptionFilterOnlyMine(
    "pendingOwnerChangeReceived",
    pubsub,
    createDataLoaders
  ),
  pendingOwnerChangeUpdated: subscriptionFilterOwnedOrShared(
    "pendingOwnerChangeUpdated",
    pubsub,
    createDataLoaders
  ),
  pendingOwnerChangeAccepted: subscriptionFilterOnlyMine(
    "pendingOwnerChangeAccepted",
    pubsub,
    createDataLoaders
  ),
  pendingOwnerChangeDeclined: subscriptionFilterOnlyMine(
    "pendingOwnerChangeDeclined",
    pubsub,
    createDataLoaders
  ),
  pendingOwnerChangeRevoked: subscriptionFilterOnlyMine(
    "pendingOwnerChangeRevoked",
    pubsub,
    createDataLoaders
  ),
  environmentCreated: subscriptionFilterOnlyMine(
    "environmentCreated",
    pubsub,
    createDataLoaders
  ),
  deviceCreated: subscriptionFilterOwnedOrShared(
    "deviceCreated",
    pubsub,
    createDataLoaders
  ),
  deviceMoved: subscriptionFilterOwnedOrShared(
    "deviceMoved",
    pubsub,
    createDataLoaders
  ),
  valueCreated: subscriptionFilterOwnedOrShared(
    "valueCreated",
    pubsub,
    createDataLoaders
  ),
  permanentTokenCreated: subscriptionFilterOnlyMine(
    "permanentTokenCreated",
    pubsub,
    createDataLoaders
  ),
  plotNodeCreated: subscriptionFilterOwnedOrShared(
    "plotNodeCreated",
    pubsub,
    createDataLoaders
  ),
  categoryPlotNodeCreated: subscriptionFilterOwnedOrShared(
    "categoryPlotNodeCreated",
    pubsub,
    createDataLoaders
  ),
  notificationCreated: subscriptionFilterOwnedOrShared(
    "notificationCreated",
    pubsub,
    createDataLoaders
  ),
  userUpdated: subscriptionFilterOnlyMine(
    "userUpdated",
    pubsub,
    createDataLoaders
  ),
  deviceUpdated: subscriptionFilterOwnedOrShared(
    "deviceUpdated",
    pubsub,
    createDataLoaders
  ),
  environmentUpdated: subscriptionFilterOwnedOrShared(
    "environmentUpdated",
    pubsub,
    createDataLoaders
  ),
  valueUpdated: subscriptionFilterOwnedOrShared(
    "valueUpdated",
    pubsub,
    createDataLoaders
  ),
  plotNodeUpdated: subscriptionFilterOwnedOrShared(
    "plotNodeUpdated",
    pubsub,
    createDataLoaders
  ),
  categoryPlotNodeUpdated: subscriptionFilterOwnedOrShared(
    "categoryPlotNodeUpdated",
    pubsub,
    createDataLoaders
  ),
  notificationUpdated: subscriptionFilterOwnedOrShared(
    "notificationUpdated",
    pubsub,
    createDataLoaders
  ),
  notificationDeleted: subscriptionFilterOwnedOrShared(
    "notificationDeleted",
    pubsub,
    createDataLoaders
  ),
  valueDeleted: subscriptionFilterOwnedOrShared(
    "valueDeleted",
    pubsub,
    createDataLoaders
  ),
  deviceDeleted: subscriptionFilterOwnedOrShared(
    "deviceDeleted",
    pubsub,
    createDataLoaders
  ),
  environmentDeleted: subscriptionFilterOwnedOrShared(
    "environmentDeleted",
    pubsub,
    createDataLoaders
  ),
  userDeleted: subscriptionFilterOnlyMine(
    "userDeleted",
    pubsub,
    createDataLoaders
  ),
  plotNodeDeleted: subscriptionFilterOwnedOrShared(
    "plotNodeDeleted",
    pubsub,
    createDataLoaders
  ),
  categoryPlotNodeDeleted: subscriptionFilterOwnedOrShared(
    "categoryPlotNodeDeleted",
    pubsub,
    createDataLoaders
  ),
  permanentTokenDeleted: subscriptionFilterOnlyMine(
    "permanentTokenDeleted",
    pubsub,
    createDataLoaders
  ),
  keepOnline: {
    subscribe: (root, args, context) =>
      logErrorsPromise(
        "keepOnlineSubscription",
        1000,
        authorized(
          args.deviceId,
          context,
          context.dataLoaders.deviceLoaderById,
          User,
          2,
          async (resolve, reject, deviceFound, [_, environmentFound]) => {
            const newDevice = await deviceFound.update({ online: true })
            const userIds = await instanceToSharedIds(environmentFound, context)

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
          deviceToParent
        )
      ),
  },
})
export default subscriptionResolver
