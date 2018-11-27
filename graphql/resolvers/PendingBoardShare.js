import { authenticated, logErrorsPromise, instanceToRole } from "./utilities"

const PendingBoardShareResolver = ({ User, Board, PendingBoardShare }) => ({
  id(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare id",
      588249587,
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: root.id },
        })

        const findSharedBoard = () =>
          Board.find({
            where: { id: pendingBoardFound.boardId },
          })
        const findUser = () => User.find({ where: { id: context.auth.userId } })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (
          context.auth.userId !== pendingBoardFound.receiverId &&
          context.auth.userId !== pendingBoardFound.senderId &&
          (await instanceToRole(await findSharedBoard(), await findUser())) ===
            null
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve(pendingBoardFound.id)
        }
      })
    )
  },
  sender(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare id",
      588249587,
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: root.id },
        })

        const findSharedBoard = () =>
          Board.find({
            where: { id: pendingBoardFound.boardId },
          })
        const findUser = () => User.find({ where: { id: context.auth.userId } })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (
          context.auth.userId !== pendingBoardFound.receiverId &&
          context.auth.userId !== pendingBoardFound.senderId &&
          (await instanceToRole(await findSharedBoard(), await findUser())) ===
            null
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve({ id: pendingBoardFound.senderId })
        }
      })
    )
  },
  receiver(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare id",
      588249587,
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: root.id },
        })

        const findSharedBoard = () =>
          Board.find({
            where: { id: pendingBoardFound.boardId },
          })
        const findUser = () => User.find({ where: { id: context.auth.userId } })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (
          context.auth.userId !== pendingBoardFound.receiverId &&
          context.auth.userId !== pendingBoardFound.senderId &&
          (await instanceToRole(await findSharedBoard(), await findUser())) ===
            null
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve({ id: pendingBoardFound.receiverId })
        }
      })
    )
  },
  role(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare id",
      588249587,
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: root.id },
        })

        const findSharedBoard = () =>
          Board.find({
            where: { id: pendingBoardFound.boardId },
          })
        const findUser = () => User.find({ where: { id: context.auth.userId } })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (
          context.auth.userId !== pendingBoardFound.receiverId &&
          context.auth.userId !== pendingBoardFound.senderId &&
          (await instanceToRole(await findSharedBoard(), await findUser())) ===
            null
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve(pendingBoardFound.role)
        }
      })
    )
  },
  board(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare board",
      588249587,
      authenticated(context, async (resolve, reject) => {
        const pendingBoardFound = await PendingBoardShare.find({
          where: { id: root.id },
        })

        const findSharedBoard = () =>
          Board.find({
            where: { id: pendingBoardFound.boardId },
          })
        const findUser = () => User.find({ where: { id: context.auth.userId } })

        if (!pendingBoardFound) {
          reject("The requested resource does not exist")
        } else if (
          context.auth.userId !== pendingBoardFound.receiverId &&
          context.auth.userId !== pendingBoardFound.senderId &&
          (await instanceToRole(await findSharedBoard(), await findUser())) ===
            null
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve({ id: pendingBoardFound.boardId })
        }
      })
    )
  },
})

module.exports = PendingBoardShareResolver
