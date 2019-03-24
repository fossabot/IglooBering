import {
  socketToDeviceMap,
  authorized,
  deviceToParent,
  logErrorsPromise,
  instanceToSharedIds,
} from "./utilities"
import { withFilter } from "graphql-subscriptions"
import createDataLoaders from "../../dataloaders/index"

const subscriptionFilterOnlyMine = (
  subscriptionName,
  pubsub,
  createDataLoaders,
  customFilter = () => true
) => ({
  subscribe: (root, args, context, info) => {
    if (context.auth) {
      const myUserId = context.auth.userId
      return withFilter(
        () => pubsub.asyncIterator(subscriptionName),
        payload => {
          context.dataLoaders = createDataLoaders()

          return (
            payload.userId === myUserId && customFilter(args, context, payload)
          )
        }
      )(root, args, context, info)
    }
    throw new Error("No authorization token")
  },
})

const subscriptionFilterOwnedOrShared = (
  subscriptionName,
  pubsub,
  createDataLoaders,
  customFilter = () => true
) => ({
  subscribe: (root, args, context, info) => {
    if (context.auth) {
      if (context.auth.userId) {
        const myUserId = context.auth.userId
        return withFilter(
          () => pubsub.asyncIterator(subscriptionName),
          payload => {
            context.dataLoaders = createDataLoaders()

            return (
              payload.userIds.indexOf(myUserId) !== -1 &&
              customFilter(args, context, payload)
            )
          }
        )(root, args, context, info)
      } else if (context.auth.tokenType === "DEVICE_ACCESS") {
        const authDeviceId = context.auth.deviceId
        return withFilter(
          () => pubsub.asyncIterator(subscriptionName),
          payload => {
            context.dataLoaders = createDataLoaders()

            return (
              payload.allowedDeviceIds &&
              payload.allowedDeviceIds.indexOf(authDeviceId) !== -1 &&
              customFilter(args, context, payload)
            )
          }
        )(root, args, context, info)
      } else {
        throw new Error("No authorization token")
      }
    }
    throw new Error("No authorization token")
  },
})

const customFilterByField = (subscriptionName, field) => (
  { [field]: requested },
  context,
  { [subscriptionName]: { [field]: actual } }
) => (requested ? requested === actual : true)

const customFilterOnSource = field => (
  { [field]: requested },
  context,
  { source: { [field]: actual } }
) => (requested ? requested === actual : true)

const subscriptionResolver = (pubsub, { User, Device, Environment }) => ({
  userUpdated: {
    subscribe: (root, args, context, info) => {
      if (context.auth) {
        const userUpdatedId = args.id || context.auth.userId
        return withFilter(
          () => pubsub.asyncIterator("userUpdated"),
          payload => {
            context.dataLoaders = createDataLoaders()

            return payload.userId === userUpdatedId
          }
        )(root, args, context, info)
      }
      throw new Error("No authorization token")
    },
  },
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
    createDataLoaders,
    customFilterByField("deviceCreated", "environmentId")
  ),
  deviceClaimed: subscriptionFilterOwnedOrShared(
    "deviceClaimed",
    pubsub,
    createDataLoaders,
    customFilterByField("deviceClaimed", "environmentId")
  ),
  deviceMoved: subscriptionFilterOwnedOrShared(
    "deviceMoved",
    pubsub,
    createDataLoaders,
    customFilterByField("deviceMoved", "environmentId")
  ),
  valueCreated: subscriptionFilterOwnedOrShared(
    "valueCreated",
    pubsub,
    createDataLoaders,
    customFilterByField("valueCreated", "deviceId")
  ),
  permanentTokenCreated: subscriptionFilterOnlyMine(
    "permanentTokenCreated",
    pubsub,
    createDataLoaders
  ),
  plotNodeCreated: subscriptionFilterOwnedOrShared(
    "plotNodeCreated",
    pubsub,
    createDataLoaders,
    customFilterByField("plotNodeCreated", "plotId")
  ),
  categoryPlotNodeCreated: subscriptionFilterOwnedOrShared(
    "categoryPlotNodeCreated",
    pubsub,
    createDataLoaders,
    customFilterByField("categoryPlotNodeCreated", "plotId")
  ),
  notificationCreated: subscriptionFilterOwnedOrShared(
    "notificationCreated",
    pubsub,
    createDataLoaders
  ),
  deviceUpdated: subscriptionFilterOwnedOrShared(
    "deviceUpdated",
    pubsub,
    createDataLoaders,
    customFilterByField("deviceUpdated", "environmentId")
  ),
  environmentUpdated: subscriptionFilterOwnedOrShared(
    "environmentUpdated",
    pubsub,
    createDataLoaders
  ),
  valueUpdated: subscriptionFilterOwnedOrShared(
    "valueUpdated",
    pubsub,
    createDataLoaders,
    customFilterByField("valueUpdated", "deviceId")
  ),
  plotNodeUpdated: subscriptionFilterOwnedOrShared(
    "plotNodeUpdated",
    pubsub,
    createDataLoaders,
    customFilterByField("plotNodeUpdated", "plotId")
  ),
  categoryPlotNodeUpdated: subscriptionFilterOwnedOrShared(
    "categoryPlotNodeUpdated",
    pubsub,
    createDataLoaders,
    customFilterByField("categoryPlotNodeUpdated", "plotId")
  ),
  notificationUpdated: subscriptionFilterOwnedOrShared(
    "notificationUpdated",
    pubsub,
    createDataLoaders
  ),
  notificationDeleted: subscriptionFilterOwnedOrShared(
    "notificationDeleted",
    pubsub,
    createDataLoaders,
    customFilterOnSource("deviceId")
  ),
  valueDeleted: subscriptionFilterOwnedOrShared(
    "valueDeleted",
    pubsub,
    createDataLoaders,
    customFilterOnSource("deviceId")
  ),
  deviceDeleted: subscriptionFilterOwnedOrShared(
    "deviceDeleted",
    pubsub,
    createDataLoaders,
    customFilterOnSource("environmentId")
  ),
  deviceUnclaimed: subscriptionFilterOwnedOrShared(
    "deviceUnclaimed",
    pubsub,
    createDataLoaders,
    customFilterOnSource("environmentId")
  ),
  environmentDeleted: subscriptionFilterOwnedOrShared(
    "environmentDeleted",
    pubsub,
    createDataLoaders
  ),
  userDeleted: {
    subscribe: (root, args, context, info) => {
      if (context.auth) {
        const userDeletedId = args.id || context.auth.userId
        return withFilter(
          () => pubsub.asyncIterator("userDeleted"),
          payload => {
            context.dataLoaders = createDataLoaders()

            return payload.userId === userDeletedId
          }
        )(root, args, context, info)
      }
      throw new Error("No authorization token")
    },
  },
  plotNodeDeleted: subscriptionFilterOwnedOrShared(
    "plotNodeDeleted",
    pubsub,
    createDataLoaders,
    customFilterOnSource("plotId")
  ),
  categoryPlotNodeDeleted: subscriptionFilterOwnedOrShared(
    "categoryPlotNodeDeleted",
    pubsub,
    createDataLoaders,
    customFilterOnSource("plotId")
  ),
  permanentTokenDeleted: subscriptionFilterOnlyMine(
    "permanentTokenDeleted",
    pubsub,
    createDataLoaders
  ),
  keepOnline: {
    subscribe: (root, args, context) =>
      logErrorsPromise(
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
