import {
  authenticated,
  logErrorsPromise,
  instanceToRole,
  authorizationLevel,
} from "./utilities"

const PendingOwnerChangeResolver = ({ User, Board, PendingOwnerChange }) => ({
  id(root, args, context) {
    return logErrorsPromise(
      "PendingOwnerChange id",
      588249587,
      authenticated(context, async (resolve, reject) => {
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
          context.auth.userId !== pendingOwnerChange.newOwnerId &&
          (await authorizationLevel(
            await findSharedBoard(),
            await findUser()
          )) < 3
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve(pendingOwnerChange.id)
        }
      })
    )
  },
  newOwner(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare id",
      588249587,
      authenticated(context, async (resolve, reject) => {
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
          context.auth.userId !== pendingOwnerChange.newOwnerId &&
          (await authorizationLevel(
            await findSharedBoard(),
            await findUser()
          )) < 3
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve({ id: pendingOwnerChange.newOwnerId })
        }
      })
    )
  },
  formerOwner(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare id",
      588249587,
      authenticated(context, async (resolve, reject) => {
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
          context.auth.userId !== pendingOwnerChange.newOwnerId &&
          (await authorizationLevel(
            await findSharedBoard(),
            await findUser()
          )) < 3
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve({ id: pendingOwnerChange.formerOwnerId })
        }
      })
    )
  },
  board(root, args, context) {
    return logErrorsPromise(
      "PendingBoardShare board",
      588249587,
      authenticated(context, async (resolve, reject) => {
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
          context.auth.userId !== pendingOwnerChange.newOwnerId &&
          (await authorizationLevel(
            await findSharedBoard(),
            await findUser()
          )) < 3
        ) {
          reject("You are not allowed to access details about this user")
        } else {
          resolve({ id: pendingOwnerChange.boardId })
        }
      })
    )
  },
})

module.exports = PendingOwnerChangeResolver
