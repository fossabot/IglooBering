import {
  logErrorsPromise,
  authorizedScalarPropsResolvers,
  authorized,
  instanceToRole,
  boardToParent,
} from './utilities'
import { Op } from 'sequelize'

const QUERY_COST = 1

const rolesResolver = (roleName, Board, User) => (root, args, context) =>
  logErrorsPromise(
    'rolesIds resolver',
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
      boardToParent,
    ),
  )

const BoardResolver = ({
  User, Board, Device, Notification, joinTables,
}) => ({
  ...authorizedScalarPropsResolvers(
    Board,
    User,
    ['customName', 'avatar', 'createdAt', 'updatedAt', 'index', 'quietMode'],
    boardToParent,
  ),
  owner(root, args, context) {
    return logErrorsPromise(
      'user BoardResolver',
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
        boardToParent,
      ),
    )
  },
  admins: rolesResolver('admin', Board, User),
  editors: rolesResolver('editor', Board, User),
  spectators: rolesResolver('spectator', Board, User),
  devices(root, args, context) {
    return logErrorsPromise(
      'devices BoardResolver',
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
        boardToParent,
      ),
    )
  },
  notificationsCount(root, args, context) {
    return logErrorsPromise(
      'notificationsCount BoardResolver',
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
            attributes: ['id'],
          })

          const notificationsCountsPromises = devices.map(device =>
            Notification.count({
              where: {
                deviceId: device.id,
                [Op.not]: {
                  visualized: { [Op.contains]: [context.auth.userId] },
                },
              },
            }))

          const notificationsCounts = await Promise.all(notificationsCountsPromises)
          const totalCount = notificationsCounts.reduce((a, b) => a + b, 0)

          resolve(totalCount)
          context.billingUpdater.update(QUERY_COST)
        },
        boardToParent,
      ),
    )
  },
  myRole(root, args, context) {
    return logErrorsPromise(
      'myRole BoardResolver',
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
        boardToParent,
      ),
    )
  },
})

export default BoardResolver
