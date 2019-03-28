import {
  authorizedScalarPropsResolvers,
  authorized,
  instanceToRole,
  environmentToParent,
  authenticated,
  authorizationLevel,
  parseStringFilter,
  parseFloatFilter,
} from "./utilities"
import { Op } from "sequelize"

const QUERY_COST = 1
const isNotNullNorUndefined = value => value !== undefined && value !== null

const rolesResolver = (roleName, Environment, User) => (root, args, context) =>
  authorized(
    root.id,
    context,
    context.dataLoaders.environmentLoaderById,
    User,
    1,
    async (resolve, reject, found) => {
      const environmentFound = await Environment.find({
        where: { id: root.id },
        include: [{ model: User, as: roleName }],
      })

      resolve(environmentFound[roleName])
    },
    environmentToParent
  )

const retrievePublicEnvironmentScalarProp = (Environment, prop) => (
  root,
  args,
  context
) =>
  authenticated(context, async (resolve, reject) => {
    const environmentFound = await context.dataLoaders.environmentLoaderById.load(
      root.id
    )
    if (!environmentFound) {
      reject("The requested resource does not exist")
    } else {
      resolve(environmentFound[prop])
    }
  })

const parseDeviceFilter = userId => filter => {
  if (!filter) return {}

  filter.hasOwnProperty = Object.prototype.hasOwnProperty

  const parsedFilter = {}
  if (filter.hasOwnProperty("AND"))
    parsedFilter[Op.and] = filter.AND.map(parseDeviceFilter(userId))
  if (filter.hasOwnProperty("OR"))
    parsedFilter[Op.or] = filter.OR.map(parseDeviceFilter(userId))
  if (filter.hasOwnProperty("NOT") && filter.NOT !== null)
    parsedFilter[Op.not] = parseDeviceFilter(userId)(filter.NOT)
  if (filter.hasOwnProperty("name") && Object.keys(filter.name).length !== 0)
    parsedFilter.name = parseStringFilter(filter.name)
  if (
    filter.hasOwnProperty("firmware") &&
    Object.keys(filter.firmware).length !== 0
  )
    parsedFilter.firmware = parseStringFilter(filter.firmware)
  if (
    filter.hasOwnProperty("batteryStatus") &&
    Object.keys(filter.batteryStatus).length !== 0
  )
    parsedFilter.batteryStatus = parseFloatFilter(filter.batteryStatus)
  if (
    filter.hasOwnProperty("signalStatus") &&
    Object.keys(filter.signalStatus).length !== 0
  )
    parsedFilter.signalStatus = parseFloatFilter(filter.signalStatus)
  if (filter.hasOwnProperty("online")) parsedFilter.online = filter.online
  if (filter.hasOwnProperty("muted")) parsedFilter.muted = filter.muted
  if (filter.hasOwnProperty("starred")) {
    if (filter.starred === true) {
      parsedFilter.starred = { [Op.contains]: [userId] }
    } else if (filter.starred === false) {
      parsedFilter[Op.not] = {
        ...(parsedFilter[Op.not] ? parsedFilter[Op.not] : {}),
        starred: { [Op.contains]: [userId] },
      }
    }
  }
  return parsedFilter
}

const EnvironmentResolver = ({
  User,
  Environment,
  Device,
  Notification,
  joinTables,
  PendingEnvironmentShare,
  PendingOwnerChange,
}) => ({
  ...authorizedScalarPropsResolvers(
    "environmentLoaderById",
    ["picture", "createdAt", "updatedAt", "index"],
    environmentToParent
  ),
  name: retrievePublicEnvironmentScalarProp(Environment, "name"),
  muted(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound, _, userFound) => {
        resolve(environmentFound.muted || userFound.quietMode)
      },
      environmentToParent
    )
  },
  owner(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        resolve({
          id: environmentFound.ownerId,
        })
      },
      environmentToParent
    )
  },
  admins: rolesResolver("admin", Environment, User),
  editors: rolesResolver("editor", Environment, User),
  spectators: rolesResolver("spectator", Environment, User),
  devices(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        if (
          args.sortBy === "index" &&
          isNotNullNorUndefined(args.sortDirection)
        ) {
          reject("Cannot set sort direction when sorting by index")
          return
        }
        args.sortDirection =
          args.sortDirection === "ASCENDING"
            ? "ASC"
            : args.sortDirection === "DESCENDING"
            ? "DESC"
            : args.sortDirection

        const sortDirection = args.sortDirection || "DESC"

        const devices = await Device.findAll({
          where: {
            environmentId: root.id,
            ...parseDeviceFilter(context.auth.userId)(args.filter),
          },
          limit: args.limit,
          offset: args.offset,
          order: args.sortBy
            ? [[args.sortBy, sortDirection]]
            : [["index", sortDirection]],
        })

        resolve(devices)
      },
      environmentToParent
    )
  },
  deviceCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const devices = await Device.count({
          where: {
            environmentId: root.id,
            ...parseDeviceFilter(context.auth.userId)(args.filter),
          },
        })

        resolve(devices)
      },
      environmentToParent
    )
  },
  pendingEnvironmentShares(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        /*
            users without admin authorization don't have access to pendingEnvironmentShares,
            instead of throwing error we return null to allow queries like
            {
              user{
                  environments{
                    pendingEnvironmentShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their environments
          */
        if (
          (await authorizationLevel(environmentFound, userFound, context)) < 3
        ) {
          resolve(null)
          return
        }

        const pendingEnvironmentShares = await PendingEnvironmentShare.findAll({
          where: { environmentId: root.id },
        })

        resolve(pendingEnvironmentShares)
      },
      environmentToParent
    )
  },
  pendingEnvironmentShareCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        /*
            users without admin authorization don't have access to pendingEnvironmentShares,
            instead of throwing error we return null to allow queries like
            {
              user{
                  environments{
                    pendingEnvironmentShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their environments
          */
        if (
          (await authorizationLevel(environmentFound, userFound, context)) < 3
        ) {
          resolve(null)
          return
        }

        const pendingEnvironmentShareCount = await PendingEnvironmentShare.count(
          {
            where: { environmentId: root.id },
          }
        )

        resolve(pendingEnvironmentShareCount)
      },
      environmentToParent
    )
  },
  pendingOwnerChanges(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        /*
            users without admin authorization don't have access to pendingOwnerShare,
            instead of throwing error we return null to allow queries like
            {
              user{
                  environments{
                    pendingEnvironmentShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their environments
          */
        if (
          (await authorizationLevel(environmentFound, userFound, context)) < 3
        ) {
          resolve(null)
          return
        }

        const pendingOwnerChanges = await PendingOwnerChange.findAll({
          where: { environmentId: root.id },
        })

        resolve(pendingOwnerChanges)
      },
      environmentToParent
    )
  },
  pendingOwnerChangeCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        /*
            users without admin authorization don't have access to pendingOwnerShare,
            instead of throwing error we return null to allow queries like
            {
              user{
                  environments{
                    pendingEnvironmentShares{ id }
                  }
              }
            }
            also for users that don't have admin access to all of their environments
          */
        if (
          (await authorizationLevel(environmentFound, userFound, context)) < 3
        ) {
          resolve(null)
          return
        }

        const pendingOwnerChangeCount = await PendingOwnerChange.count({
          where: { environmentId: root.id },
        })

        resolve(pendingOwnerChangeCount)
      },
      environmentToParent
    )
  },
  notificationCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        // TODO: consider changing implementation to that of user.notifications
        const devices = await Device.findAll({
          where: { environmentId: root.id },
          attributes: ["id"],
        })

        const notificationCountsPromises = devices.map(device =>
          Notification.count({
            where: {
              deviceId: device.id,
              notRead: { [Op.contains]: [context.auth.userId] },
            },
          })
        )

        const notificationCounts = await Promise.all(notificationCountsPromises)
        const totalCount = notificationCounts.reduce((a, b) => a + b, 0)

        resolve(totalCount)
      },
      environmentToParent
    )
  },
  myRole(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (
        resolve,
        reject,
        environmentFound,
        environmentAndParents,
        userFound
      ) => {
        const myRole = await instanceToRole(
          environmentFound,
          userFound,
          context
        )

        resolve(myRole)
      },
      environmentToParent
    )
  },
})

export default EnvironmentResolver
