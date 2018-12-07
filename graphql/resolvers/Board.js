import {
  logErrorsPromise,
  authorizedScalarPropsResolvers,
  authorized,
  instanceToRole,
  boardToParent,
  authenticated,
  authorizationLevel,
} from "./utilities"
import { Op } from "sequelize"

const QUERY_COST = 1

const rolesResolver = (roleName, Board, User) => (root, args, context) =>
  logErrorsPromise(
    "rolesIds resolver",
    922,
    authorized(
      root.id,
      context,
      Board,
      User,
      1,
      async (resolve, reject, found) => {
        const boardFound = await Board.find({
          where: { id: root.id },
          include: [{ model: User, as: roleName }],
        })

        resolve(boardFound[roleName])

        context.billingUpdater.update(QUERY_COST * boardFound[roleName].length)
      },
      boardToParent
    )
  )

const retrievePublicBoardScalarProp = (Board, prop) => (root, args, context) =>
  logErrorsPromise(
    "retrieveScalarProp",
    106,
    authenticated(context, async (resolve, reject) => {
      const boardFound = await Board.find({ where: { id: root.id } })
      if (!boardFound) {
        reject("The requested resource does not exist")
      } else {
        resolve(boardFound[prop])
      }
    })
  )
const BoardResolver = ({
  User,
  Board,
  Device,
  Notification,
  joinTables,
  PendingBoardShare,
  PendingOwnerChange,
}) => ({
  ...authorizedScalarPropsResolvers(
    Board,
    User,
    ["avatar", "createdAt", "updatedAt", "index"],
    boardToParent
  ),
  name: retrievePublicBoardScalarProp(Board, "name"),
  muted(root, args, context) {
    return logErrorsPromise(
      "muted BoardResolver",
      902,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound, _, userFound) => {
          resolve(boardFound.muted || userFound.muted)
        },
        boardToParent
      )
    )
  },
  owner(root, args, context) {
    return logErrorsPromise(
      "user BoardResolver",
      902,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          resolve({
            id: boardFound.ownerId,
          })

          context.billingUpdater.update(QUERY_COST)
        },
        boardToParent
      )
    )
  },
  admins: rolesResolver("admin", Board, User),
  editors: rolesResolver("editor", Board, User),
  spectators: rolesResolver("spectator", Board, User),
  devices(root, args, context) {
    return logErrorsPromise(
      "devices BoardResolver",
      903,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          const devices = await Device.findAll({ where: { boardId: root.id } })

          resolve(devices)

          context.billingUpdater.update(QUERY_COST * devices.length)
        },
        boardToParent
      )
    )
  },
  deviceCount(root, args, context) {
    return logErrorsPromise(
      "devices BoardResolver",
      903,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          const devices = await Device.count({ where: { boardId: root.id } })

          resolve(devices)
        },
        boardToParent
      )
    )
  },
  pendingBoardShares(root, args, context) {
    return logErrorsPromise(
      "pendingBoardShares BoardResolver",
      903,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          /*
            users without admin authorization don't have access to pendingBoardShares,
            instead of throwing error we return null to allow queries like
            {
              user{
                  boards{
                    pendingBoardShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their boards
          */
          if ((await authorizationLevel(boardFound, userFound)) < 3) {
            resolve(null)
            return
          }

          const pendingBoardShares = await PendingBoardShare.findAll({
            where: { boardId: root.id },
          })

          resolve(pendingBoardShares)
          context.billingUpdater.update(QUERY_COST * pendingBoardShares.length)
        },
        boardToParent
      )
    )
  },
  pendingBoardShareCount(root, args, context) {
    return logErrorsPromise(
      "pendingBoardShareCount BoardResolver",
      903,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          /*
            users without admin authorization don't have access to pendingBoardShares,
            instead of throwing error we return null to allow queries like
            {
              user{
                  boards{
                    pendingBoardShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their boards
          */
          if ((await authorizationLevel(boardFound, userFound)) < 3) {
            resolve(null)
            return
          }

          const pendingBoardShareCount = await PendingBoardShare.count({
            where: { boardId: root.id },
          })

          resolve(pendingBoardShareCount)
        },
        boardToParent
      )
    )
  },
  pendingOwnerChanges(root, args, context) {
    return logErrorsPromise(
      "pendingOwnerChange BoardResolver",
      903,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          /*
            users without admin authorization don't have access to pendingOwnerShare,
            instead of throwing error we return null to allow queries like
            {
              user{
                  boards{
                    pendingBoardShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their boards
          */
          if ((await authorizationLevel(boardFound, userFound)) < 3) {
            resolve(null)
            return
          }

          const pendingOwnerChanges = await PendingOwnerChange.findAll({
            where: { boardId: root.id },
          })

          resolve(pendingOwnerChanges)
          context.billingUpdater.update(QUERY_COST * pendingOwnerChanges.length)
        },
        boardToParent
      )
    )
  },
  pendingOwnerChangeCount(root, args, context) {
    return logErrorsPromise(
      "pendingOwnerChange BoardResolver",
      903,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          const userFound = await User.find({
            where: { id: context.auth.userId },
          })

          /*
            users without admin authorization don't have access to pendingOwnerShare,
            instead of throwing error we return null to allow queries like
            {
              user{
                  boards{
                    pendingBoardShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their boards
          */
          if ((await authorizationLevel(boardFound, userFound)) < 3) {
            resolve(null)
            return
          }

          const pendingOwnerChangeCount = await PendingOwnerChange.count({
            where: { boardId: root.id },
          })

          resolve(pendingOwnerChangeCount)
        },
        boardToParent
      )
    )
  },
  notificationCount(root, args, context) {
    return logErrorsPromise(
      "notificationCount BoardResolver",
      915,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound) => {
          // TODO: consider changing implementation to that of user.notifications
          const devices = await Device.findAll({
            where: { boardId: root.id },
            attributes: ["id"],
          })

          const notificationCountsPromises = devices.map(device =>
            Notification.count({
              where: {
                deviceId: device.id,
                [Op.not]: {
                  visualized: { [Op.contains]: [context.auth.userId] },
                },
              },
            })
          )

          const notificationCounts = await Promise.all(
            notificationCountsPromises
          )
          const totalCount = notificationCounts.reduce((a, b) => a + b, 0)

          resolve(totalCount)
          context.billingUpdater.update(QUERY_COST)
        },
        boardToParent
      )
    )
  },
  myRole(root, args, context) {
    return logErrorsPromise(
      "myRole BoardResolver",
      931,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound, boardAndParents, userFound) => {
          const myRole = await instanceToRole(boardFound, userFound)

          resolve(myRole)
        },
        boardToParent
      )
    )
  },
})

export default BoardResolver
