import { authenticated, instanceToRole, authorizationLevel } from "./utilities"

const PendingOwnerChangeResolver = ({ User, Board, PendingOwnerChange }) => ({
  id(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingOwnerChange = await PendingOwnerChange.find({
        where: { id: root.id },
      })

      const findSharedBoard = () =>
        Board.find({
          where: { id: pendingOwnerChange.boardId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(await findSharedBoard(), await findUser())) <
          3
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

      const findSharedBoard = () =>
        Board.find({
          where: { id: pendingOwnerChange.boardId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(await findSharedBoard(), await findUser())) <
          3
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

      const findSharedBoard = () =>
        Board.find({
          where: { id: pendingOwnerChange.boardId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(await findSharedBoard(), await findUser())) <
          3
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingOwnerChange.senderId })
      }
    })
  },
  board(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const pendingOwnerChange = await PendingOwnerChange.find({
        where: { id: root.id },
      })

      const findSharedBoard = () =>
        Board.find({
          where: { id: pendingOwnerChange.boardId },
        })
      const findUser = () => User.find({ where: { id: context.auth.userId } })

      if (!pendingOwnerChange) {
        reject("The requested resource does not exist")
      } else if (
        context.auth.userId !== pendingOwnerChange.receiverId &&
        (await authorizationLevel(await findSharedBoard(), await findUser())) <
          3
      ) {
        reject("You are not allowed to perform this operation")
      } else {
        resolve({ id: pendingOwnerChange.boardId })
      }
    })
  },
})

module.exports = PendingOwnerChangeResolver
