import {
  inheritAuthorized,
  inheritAuthorizedScalarPropsResolvers,
  deviceToParent,
} from "./utilities"

const QUERY_COST = 1
const notificationToParent = notificationFound => notificationFound.deviceId

const UserResolver = ({ Notification, User, Device, Environment }) => ({
  ...inheritAuthorizedScalarPropsResolvers(
    "notificationLoaderById",
    User,
    ["content", "date"],
    notificationToParent,
    "deviceLoaderById",
    deviceToParent
  ),
  user(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders.notificationLoaderById,
      User,
      notificationToParent,
      context,
      context.dataLoaders.deviceLoaderById,
      1,
      async (resolve, reject, notificationFound) => {
        resolve({ id: notificationFound.userId })
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
  device(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders.notificationLoaderById,
      User,
      notificationToParent,
      context,
      context.dataLoaders.deviceLoaderById,
      1,
      async (resolve, reject, notificationFound) => {
        resolve({ id: notificationFound.deviceId })
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
  environment(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders.notificationLoaderById,
      User,
      notificationToParent,
      context,
      context.dataLoaders.deviceLoaderById,
      1,
      async (resolve, reject, notificationFound) => {
        resolve({ id: notificationFound.environmentId })
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
  read(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders.notificationLoaderById,
      User,
      notificationToParent,
      context,
      context.dataLoaders.deviceLoaderById,
      1,
      async (resolve, reject, notificationFound) => {
        resolve(notificationFound.notRead.indexOf(context.auth.userId) === -1)
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent
    )
  },
})

export default UserResolver
