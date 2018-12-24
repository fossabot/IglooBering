import { authenticated, instanceToRole } from "./utilities"

const PendingEnvironmentShareResolver = ({
  User,
  Environment,
  PendingEnvironmentShare,
}) => ({
  id(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingEnvironmentFound = await PendingEnvironmentShare.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        Environment.find({
          where: { id: pendingEnvironmentFound.environmentId },
        })
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingEnvironmentFound) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingEnvironmentFound.receiverId &&
        context.auth.userId !== pendingEnvironmentFound.senderId &&
        (await instanceToRole(
          await findSharedEnvironment(),
          await findUser()
        )) === null
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve(pendingEnvironmentFound.id)
      }
    })
  },
  sender(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingEnvironmentFound = await PendingEnvironmentShare.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        Environment.find({
          where: { id: pendingEnvironmentFound.environmentId },
        })
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingEnvironmentFound) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingEnvironmentFound.receiverId &&
        context.auth.userId !== pendingEnvironmentFound.senderId &&
        (await instanceToRole(
          await findSharedEnvironment(),
          await findUser()
        )) === null
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingEnvironmentFound.senderId })
      }
    })
  },
  receiver(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingEnvironmentFound = await PendingEnvironmentShare.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        Environment.find({
          where: { id: pendingEnvironmentFound.environmentId },
        })

      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingEnvironmentFound) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingEnvironmentFound.receiverId &&
        context.auth.userId !== pendingEnvironmentFound.senderId &&
        (await instanceToRole(
          await findSharedEnvironment(),
          await findUser()
        )) === null
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingEnvironmentFound.receiverId })
      }
    })
  },
  role(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingEnvironmentFound = await PendingEnvironmentShare.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        Environment.find({
          where: { id: pendingEnvironmentFound.environmentId },
        })
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingEnvironmentFound) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingEnvironmentFound.receiverId &&
        context.auth.userId !== pendingEnvironmentFound.senderId &&
        (await instanceToRole(
          await findSharedEnvironment(),
          await findUser()
        )) === null
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve(pendingEnvironmentFound.role)
      }
    })
  },
  environment(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingEnvironmentFound = await PendingEnvironmentShare.find({
        where: { id: root.id },
      })

      const findSharedEnvironment = () =>
        Environment.find({
          where: { id: pendingEnvironmentFound.environmentId },
        })
      const findUser = () =>
        context.dataLoaders.userLoaderById.load(context.auth.userId)

      if (!pendingEnvironmentFound) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingEnvironmentFound.receiverId &&
        context.auth.userId !== pendingEnvironmentFound.senderId &&
        (await instanceToRole(
          await findSharedEnvironment(),
          await findUser()
        )) === null
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingEnvironmentFound.environmentId })
      }
    })
  },
})

module.exports = PendingEnvironmentShareResolver
