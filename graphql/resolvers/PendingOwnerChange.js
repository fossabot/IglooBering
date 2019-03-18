import { authenticated, authorizationLevel } from "./utilities"

const PendingOwnerChangeResolver = ({
  User,
  Environment,
  PendingOwnerChange,
}) => ({
  id(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingOwnerChange = await PendingOwnerChange.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        context.dataLoaders.environmentLoaderById.load(
          pendingOwnerChange.environmentId
        )
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser(),
          context
        )) < 3
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve(pendingOwnerChange.id)
      }
    })
  },
  receiver(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingOwnerChange = await PendingOwnerChange.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        context.dataLoaders.environmentLoaderById.load(
          pendingOwnerChange.environmentId
        )
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser(),
          context
        )) < 3
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingOwnerChange.receiverId })
      }
    })
  },
  sender(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingOwnerChange = await PendingOwnerChange.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        context.dataLoaders.environmentLoaderById.load(
          pendingOwnerChange.environmentId
        )
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser(),
          context
        )) < 3
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingOwnerChange.senderId })
      }
    })
  },
  environment(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingOwnerChange = await PendingOwnerChange.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        context.dataLoaders.environmentLoaderById.load(
          pendingOwnerChange.environmentId
        )
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser(),
          context
        )) < 3
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingOwnerChange.environmentId })
      }
    })
  },
})

module.exports = PendingOwnerChangeResolver
