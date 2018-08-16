import {
  logErrorsPromise,
  authorizedScalarPropsResolvers,
  authorized,
} from './utilities'

const QUERY_COST = 1

const rolesResolver = (roleIdsField, Board) => (root, args, context) =>
  logErrorsPromise(
    'board rolesIds resolver',
    922,
    authorized(
      root.id,
      context,
      Board,
      1,
      async (resolve, reject, boardFound) => {
        const users = boardFound[roleIdsField].map(id => ({
          id,
        }))

        resolve(users)

        context.billingUpdater.update(QUERY_COST * users.length)
      },
    ),
  )

const BoardResolver = (Board, Device, Notification) => ({
  ...authorizedScalarPropsResolvers(Board, [
    'customName',
    'avatar',
    'createdAt',
    'updatedAt',
    'index',
    'favorite',
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
        1,
        async (resolve, reject, boardFound) => {
          resolve({
            id: boardFound.ownerId,
          })

          context.billingUpdater.update(QUERY_COST)
        },
      ),
    )
  },
  admins: rolesResolver('adminsIds', Board),
  editors: rolesResolver('editorsIds', Board),
  spectators: rolesResolver('spectatorsIds', Board),
  devices(root, args, context) {
    return logErrorsPromise(
      'devices BoardResolver',
      903,
      authorized(
        root.id,
        context,
        Board,
        1,
        async (resolve, reject, boardFound) => {
          const devices = await Device.findAll({ where: { boardId: root.id } })

          resolve(devices)

          context.billingUpdater.update(QUERY_COST * devices.length)
        },
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
        1,
        async (resolve, reject, boardFound) => {
          const devices = await Device.findAll({ where: { boardId: root.id } })

          const notificationsCountsPromises = devices.map(device =>
            Notification.count({ where: { deviceId: device.id } }))

          const notificationsCounts = await Promise.all(notificationsCountsPromises)
          // avoid reducing an empty array
          const totalCount =
            notificationsCounts.length === 0
              ? 0
              : notificationsCounts.reduce((a, b) => a + b, 0)

          resolve(totalCount)
          context.billingUpdater.update(QUERY_COST)
        },
      ),
    )
  },
})

export default BoardResolver
