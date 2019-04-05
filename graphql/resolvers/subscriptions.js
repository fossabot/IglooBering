import {
  socketToDeviceMap,
  deviceAuthorized,
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
    if (!context.auth) {
      throw new Error("No authorization token")
    }
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
  },
})

const customFilterByField = (subscriptionName, field) => (
  { [field]: requested },
  context,
  { [subscriptionName]: { [field]: actual } }
) => (requested ? requested === actual : true)

const customFilterByFields = (subscriptionName, ...fields) => (
  args,
  context,
  payload
) =>
  fields.reduce(
    (acc, curr) =>
      acc &&
      customFilterByField(subscriptionName, curr)(args, context, payload),
    true
  )

const customFiltersOnSource = (...fields) => (args, context, payload) =>
  fields.reduce(
    (acc, curr) => acc && customFilterOnSource(curr)(args, context, payload),
    true
  )

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
    customFilterByFields("deviceCreated", "environmentId")
  ),
  deviceClaimed: subscriptionFilterOwnedOrShared(
    "deviceClaimed",
    pubsub,
    createDataLoaders,
    customFilterByFields("deviceClaimed", "environmentId", "id")
  ),
  deviceMoved: subscriptionFilterOwnedOrShared(
    "deviceMoved",
    pubsub,
    createDataLoaders,
    customFilterByFields("deviceMoved", "environmentId", "id")
  ),
  valueCreated: subscriptionFilterOwnedOrShared(
    "valueCreated",
    pubsub,
    createDataLoaders,
    customFilterByFields("valueCreated", "deviceId", "visibility")
  ),
  permanentTokenCreated: subscriptionFilterOnlyMine(
    "permanentTokenCreated",
    pubsub,
    createDataLoaders
  ),
  floatSeriesNodeCreated: subscriptionFilterOwnedOrShared(
    "floatSeriesNodeCreated",
    pubsub,
    createDataLoaders,
    customFilterByFields("floatSeriesNodeCreated", "seriesId")
  ),
  categorySeriesNodeCreated: subscriptionFilterOwnedOrShared(
    "categorySeriesNodeCreated",
    pubsub,
    createDataLoaders,
    customFilterByFields("categorySeriesNodeCreated", "seriesId")
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
    customFilterByFields("deviceUpdated", "environmentId", "id")
  ),
  environmentUpdated: subscriptionFilterOwnedOrShared(
    "environmentUpdated",
    pubsub,
    createDataLoaders,
    customFilterByFields("environmentUpdated", "id")
  ),
  valueUpdated: subscriptionFilterOwnedOrShared(
    "valueUpdated",
    pubsub,
    createDataLoaders,
    customFilterByFields("valueUpdated", "deviceId", "id", "visibility")
  ),
  floatSeriesNodeUpdated: subscriptionFilterOwnedOrShared(
    "floatSeriesNodeUpdated",
    pubsub,
    createDataLoaders,
    customFilterByFields("floatSeriesNodeUpdated", "seriesId", "id")
  ),
  categorySeriesNodeUpdated: subscriptionFilterOwnedOrShared(
    "categorySeriesNodeUpdated",
    pubsub,
    createDataLoaders,
    customFilterByFields("categorySeriesNodeUpdated", "seriesId", "id")
  ),
  notificationUpdated: subscriptionFilterOwnedOrShared(
    "notificationUpdated",
    pubsub,
    createDataLoaders,
    customFilterByFields("notificationUpdated", "deviceId", "id")
  ),
  notificationDeleted: subscriptionFilterOwnedOrShared(
    "notificationDeleted",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("deviceId", "id")
  ),
  valueDeleted: subscriptionFilterOwnedOrShared(
    "valueDeleted",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("deviceId", "id", "visibility")
  ),
  deviceDeleted: subscriptionFilterOwnedOrShared(
    "deviceDeleted",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("environmentId", "id")
  ),
  deviceUnclaimed: subscriptionFilterOwnedOrShared(
    "deviceUnclaimed",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("environmentId", "id")
  ),
  environmentDeleted: subscriptionFilterOwnedOrShared(
    "environmentDeleted",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("id")
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
  floatSeriesNodeDeleted: subscriptionFilterOwnedOrShared(
    "floatSeriesNodeDeleted",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("seriesId", "id")
  ),
  categorySeriesNodeDeleted: subscriptionFilterOwnedOrShared(
    "categorySeriesNodeDeleted",
    pubsub,
    createDataLoaders,
    customFiltersOnSource("seriesId", "id")
  ),
  permanentTokenDeleted: subscriptionFilterOnlyMine(
    "permanentTokenDeleted",
    pubsub,
    createDataLoaders
  ),
  keepOnline: {
    subscribe: (root, args, context) =>
      logErrorsPromise(
        deviceAuthorized(
          args.deviceId,
          context,
          2,
          async (resolve, reject, deviceFound, userFound) => {
            const environmentFound =
              deviceFound.environmentId &&
              (await context.dataLoaders.environmentLoaderById.load(
                deviceFound.environmentId
              ))
            const newDevice = await deviceFound.update({ online: true })
            const userIds = environmentFound
              ? [
                  newDevice.producerId,
                  ...(await instanceToSharedIds(environmentFound, context)),
                ]
              : [newDevice.producerId]

            pubsub.publish("deviceUpdated", {
              deviceUpdated: newDevice.dataValues,
              userIds,
              allowedDeviceIds: [newDevice.id],
            })

            socketToDeviceMap[context.websocket] = {
              deviceId: args.deviceId,
              userIds,
            }

            resolve(pubsub.asyncIterator("bogusIterator")) // this iterator will never send any data
          }
        )
      ),
  },
})
export default subscriptionResolver
