import {
  logErrorsPromise,
  authorizedScalarPropsResolvers,
  authorized,
  rolesResolver,
  instanceToRole,
  boardToParent,
} from './utilities'
import { Op } from 'sequelize'

const QUERY_COST = 1

const BoardResolver = ({
  User, Board, Device, Notification, joinTables,
}) => ({
  ...authorizedScalarPropsResolvers(Board, User, [
    'customName',
    'avatar',
    'createdAt',
    'updatedAt',
    'index',
    'quietMode',
  ]),
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
  admins: rolesResolver('admin', Board, User, boardToParent),
  editors: rolesResolver('editor', Board, User, boardToParent),
  spectators: rolesResolver('spectator', Board, User, boardToParent),
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
          const myRole = await instanceToRole([boardFound], userFound)

          resolve(myRole)
        },
        boardToParent,
      ),
    )
  },
  favorite(root, args, context) {
    return logErrorsPromise(
      'favorite BoardResolver',
      932,
      authorized(
        root.id,
        context,
        Board,
        User,
        1,
        async (resolve, reject, boardFound, boardAndParents, userFound) => {
          const favorite =
            boardFound.favorite.indexOf(context.auth.userId) !== -1

          resolve(favorite)
        },
        boardToParent,
      ),
    )
  },
})

export default BoardResolver
