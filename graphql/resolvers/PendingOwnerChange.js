import { authenticated, instanceToRole, authorizationLevel } from "./utilities"

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
        Environment.find({
          where: { id: pendingOwnerChange.environmentId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser()
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
        Environment.find({
          where: { id: pendingOwnerChange.environmentId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser()
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
        Environment.find({
          where: { id: pendingOwnerChange.environmentId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser()
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
        Environment.find({
          where: { id: pendingOwnerChange.environmentId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(
          await findSharedEnvironment(),
          await findUser()
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
