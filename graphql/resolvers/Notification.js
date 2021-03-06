import {
  inheritAuthorized,
  deviceInheritAuthorized,
  deviceInheritAuthorizedScalarPropsResolvers,
  deviceToParent,
} from "./utilities"

const notificationToParent = notificationFound => notificationFound.deviceId

const UserResolver = ({ User }) => ({
  ...deviceInheritAuthorizedScalarPropsResolvers("notificationLoaderById", [
    "content",
    "date",
  ]),
  device(root, args, context) {
    return deviceInheritAuthorized(
      root.id,
      context.dataLoaders.notificationLoaderById,
      context,
      1,
      async (resolve, reject, notificationFound) => {
        resolve({ id: notificationFound.deviceId })
      }
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
      },
      deviceToParent
    )
  },
})

export default UserResolver
