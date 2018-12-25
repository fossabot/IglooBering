import {
  authenticated,
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
    deviceToParent(Environment)
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
      deviceToParent(Environment)
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
      deviceToParent(Environment)
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
      deviceToParent(Environment)
    )
  },
  visualized(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders.notificationLoaderById,
      User,
      notificationToParent,
      context,
      context.dataLoaders.deviceLoaderById,
      1,
      async (resolve, reject, notificationFound) => {
        resolve(
          notificationFound.visualized.indexOf(context.auth.userId) !== -1
        )
        context.billingUpdater.update(QUERY_COST)
      },
      deviceToParent(Environment)
    )
  },
})

export default UserResolver
