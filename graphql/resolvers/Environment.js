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
    filter.hasOwnProperty("deviceType") &&
    Object.keys(filter.deviceType).length !== 0
  )
    parsedFilter.deviceType = parseStringFilter(filter.deviceType)
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
  EnvironmentAdmin,
  EnvironmentEditor,
  EnvironmentSpectator,
  Device,
  Notification,
  joinTables,
  PendingEnvironmentShare,
  PendingOwnerChange,
  sequelize,
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
  adminCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, found) => {
        const count = await EnvironmentAdmin.count({
          where: { environmentId: root.id },
        })

        resolve(count)
      },
      environmentToParent
    )
  },
  admins(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, found) => {
        const admins = await EnvironmentAdmin.findAll({
          where: { environmentId: root.id },
          limit: args.limit,
          offset: args.offset,
        })

        const users = admins.map(admin => ({ id: admin.userId }))

        resolve(users)
      },
      environmentToParent
    )
  },
  editorCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, found) => {
        const count = await EnvironmentEditor.count({
          where: { environmentId: root.id },
        })

        resolve(count)
      },
      environmentToParent
    )
  },
  editors(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, found) => {
        const editors = await EnvironmentEditor.findAll({
          where: { environmentId: root.id },
          limit: args.limit,
          offset: args.offset,
        })

        const users = editors.map(editor => ({ id: editor.userId }))

        resolve(users)
      },
      environmentToParent
    )
  },
  spectatorCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, found) => {
        const count = await EnvironmentSpectator.count({
          where: { environmentId: root.id },
        })

        resolve(count)
      },
      environmentToParent
    )
  },
  spectators(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, found) => {
        const spectators = await EnvironmentSpectator.findAll({
          where: { environmentId: root.id },
          limit: args.limit,
          offset: args.offset,
        })

        const users = spectators.map(spectator => ({ id: spectator.userId }))

        resolve(users)
      },
      environmentToParent
    )
  },
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

        function parseEnvironmentShareFilter(filter) {
          if (!filter) return {}
          const parsedFilter = {}
          filter.hasOwnProperty = Object.prototype.hasOwnProperty

          if (filter.hasOwnProperty("AND"))
            parsedFilter[Op.and] = filter.AND.map(parseEnvironmentShareFilter)
          if (filter.hasOwnProperty("OR"))
            parsedFilter[Op.or] = filter.OR.map(parseEnvironmentShareFilter)
          if (filter.hasOwnProperty("NOT") && filter.NOT !== null)
            parsedFilter[Op.not] = parseEnvironmentShareFilter(filter.NOT)
          if (filter.hasOwnProperty("role")) parsedFilter.role = filter.role

          return parsedFilter
        }

        const pendingEnvironmentShares = await PendingEnvironmentShare.findAll({
          where: {
            ...parseEnvironmentShareFilter(args.filter),
            environmentId: root.id,
          },
          limit: args.limit,
          offset: args.offset,
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
            users without admin authorization don't have access to pendingEnvironmentShareCount,
            instead of throwing error we return null to allow queries like
            {
              user{
                  environments{
                    pendingEnvironmentShareCount
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
  pendingOwnerChange(root, args, context) {
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

        const pendingOwnerChange = await PendingOwnerChange.find({
          where: { environmentId: root.id },
          limit: args.limit,
          offset: args.offset,
        })

        resolve(pendingOwnerChange)
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
  uniqueFirmwares(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.environmentLoaderById,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const query = `SELECT DISTINCT("device"."deviceType", "device"."firmware") FROM "devices" AS "device" WHERE "device"."environmentId" = '${
          environmentFound.id
        }'`
        const rawResponse = await sequelize.query(query)

        function splitPair(pairStr) {
          return pairStr.substring(1, pairStr.length - 1).split(",")
        }
        const parsedRes = rawResponse[0].map(({ row }) => splitPair(row))

        resolve(parsedRes)
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
