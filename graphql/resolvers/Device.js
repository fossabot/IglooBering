import {
  authorized,
  authorizedScalarPropsResolvers,
  deviceToParent,
  instanceToRole,
  parseStringFilter,
  parseDateFilter,
} from "./utilities"
import { Op } from "sequelize"
import SqlString from "sqlstring"
import QRCode from "qrcode-svg"

const QUERY_COST = 1
const isNotNullNorUndefined = value => value !== undefined && value !== null

const parseNotificationFilter = userId => filter => {
  if (!filter) return {}

  filter.hasOwnProperty = Object.prototype.hasOwnProperty

  const parsedFilter = {}
  if (filter.hasOwnProperty("AND"))
    parsedFilter[Op.and] = filter.AND.map(parseNotificationFilter(userId))
  if (filter.hasOwnProperty("OR"))
    parsedFilter[Op.or] = filter.OR.map(parseNotificationFilter(userId))
  if (filter.hasOwnProperty("content"))
    parsedFilter.content = parseStringFilter(filter.content)
  if (filter.hasOwnProperty("date"))
    parsedFilter.date = parseDateFilter(filter.date)
  if (filter.hasOwnProperty("read")) {
    if (filter.read === true) {
      parsedFilter[Op.not] = {
        ...(parsedFilter[Op.not] ? parsedFilter[Op.not] : {}),
        notRead: { [Op.contains]: [userId] },
      }
    } else if (filter.read === false) {
      parsedFilter.notRead = { [Op.contains]: [userId] }
    }
  }

  return parsedFilter
}

const DeviceResolver = ({
  Device,
  User,
  Environment,
  BooleanValue,
  FloatValue,
  StringValue,
  PlotValue,
  CategoryPlotValue,
  Notification,
  joinTables,
  sequelize,
}) => ({
  ...authorizedScalarPropsResolvers(
    "deviceLoaderById",
    [
      "createdAt",
      "updatedAt",
      "deviceType",
      "name",
      "index",
      "online",
      "signalStatus",
      "batteryStatus",
      "batteryCharging",
      "firmware",
    ],
    deviceToParent,
    ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
  ),
  values(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
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

        const parseRawStringFilter = (stringFilter, fieldName) => {
          stringFilter.hasOwnProperty = Object.prototype.hasOwnProperty

          if (stringFilter.hasOwnProperty("equals"))
            return `(${fieldName} = E${SqlString.escape(stringFilter.equals)})`
          else if (stringFilter.hasOwnProperty("like"))
            return `(${fieldName} LIKE E${SqlString.escape(stringFilter.like)})`
          else if (stringFilter.hasOwnProperty("similarTo"))
            return `(${fieldName} SIMILAR TO E${SqlString.escape(
              stringFilter.similarTo
            )})`
          else return ""
        }
        const parseValueFilter = (filter, table) => {
          if (!filter) return ""
          filter.hasOwnProperty = Object.prototype.hasOwnProperty

          const filtersStack = []
          if (filter.hasOwnProperty("AND"))
            filtersStack.push(
              `(${filter.AND.map(f => parseValueFilter(f, table))
                .filter(query => query !== "")
                .join(" AND ")})`
            )
          if (filter.hasOwnProperty("OR"))
            filtersStack.push(
              `(${filter.OR.map(f => parseValueFilter(f, table))
                .filter(query => query !== "")
                .join(" OR ")})`
            )
          if (filter.hasOwnProperty("cardSize"))
            filtersStack.push(
              `(public."${table}"."cardSize" = '${filter.cardSize}')`
            )
          if (filter.hasOwnProperty("name"))
            filtersStack.push(
              parseRawStringFilter(filter.name, `public."${table}"."name"`)
            )

          return filtersStack.filter(query => query !== "").join(" AND ")
        }

        const limitQuery = args.limit
          ? args.offset
            ? `LIMIT ${args.limit} OFFSET ${args.offset}`
            : `LIMIT ${args.limit}`
          : ""

        const orderQuery = args.sortBy
          ? args.sortDirection
            ? `ORDER BY "${args.sortBy}" ${args.sortDirection}`
            : `ORDER BY "${args.sortBy}"`
          : ""

        const additionalSelect = table =>
          args.sortBy ? `, public."${table}"."${args.sortBy}"` : ""

        const whereQuery = table => {
          const query = parseValueFilter(args.filter, table)
          return query !== "" ? " AND " + query : ""
        }

        const query = `
        SELECT public."floatValues".id ${additionalSelect(
          "floatValues"
        )} FROM public."floatValues"
          WHERE public."floatValues"."deviceId" = '${root.id}' ${whereQuery(
          "floatValues"
        )}
        UNION
        SELECT public."stringValues".id ${additionalSelect(
          "stringValues"
        )} FROM public."stringValues"
          WHERE public."stringValues"."deviceId" = '${root.id}' ${whereQuery(
          "stringValues"
        )}
        UNION
        SELECT public."categoryPlotValues".id ${additionalSelect(
          "categoryPlotValues"
        )} FROM public."categoryPlotValues"
          WHERE public."categoryPlotValues"."deviceId" = '${
            root.id
          }' ${whereQuery("categoryPlotValues")}
        UNION
        SELECT public."plotValues".id ${additionalSelect(
          "plotValues"
        )} FROM public."plotValues"
          WHERE public."plotValues"."deviceId" = '${root.id}' ${whereQuery(
          "plotValues"
        )}
        UNION
        SELECT public."booleanValues".id ${additionalSelect(
          "booleanValues"
        )} FROM public."booleanValues"
          WHERE public."booleanValues"."deviceId" = '${root.id}' ${whereQuery(
          "booleanValues"
        )}

        ${orderQuery}
        ${limitQuery}
        `

        const valuesFound = await sequelize.query(query, {
          type: sequelize.QueryTypes.SELECT,
        })

        resolve(valuesFound)
      },
      deviceToParent,
      ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
    )
  },
  starred(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const isStarred =
          deviceFound.starred.indexOf(context.auth.userId) !== -1

        resolve(isStarred)
      },
      deviceToParent
    )
  },
  muted(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (
        resolve,
        reject,
        deviceFound,
        [_, environmentFound],
        userFound
      ) => {
        // the Environment resolver will take care of loading the other props,
        // it only needs to know the environment id
        resolve(
          deviceFound.muted || environmentFound.muted || userFound.quietMode
        )
      },
      deviceToParent
    )
  },
  environment(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        // the Environment resolver will take care of loading the other props,
        // it only needs to know the environment id
        resolve(
          deviceFound.environmentId ? { id: deviceFound.environmentId } : null
        )
      },
      deviceToParent
    )
  },
  notifications(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const notifications = await Notification.findAll({
          where: {
            deviceId: deviceFound.id,
            ...parseNotificationFilter(context.auth.userId)(args.filter),
          },
          limit: args.limit,
          offset: args.offset,
          order: [["date", "DESC"]],
        })

        resolve(notifications)
      },
      deviceToParent,
      ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
    )
  },
  lastNotification(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const notificationFound = await Notification.find({
          where: {
            deviceId: deviceFound.id,
            ...parseNotificationFilter(context.auth.userId)(args.filter),
          },
          order: [["date", "DESC"]],
        })

        resolve(notificationFound)
      },
      deviceToParent,
      ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
    )
  },
  notificationCount(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject, deviceFound) => {
        const count = await Notification.count({
          where: {
            deviceId: root.id,
            ...parseNotificationFilter(context.auth.userId)(args.filter),
          },
        })

        resolve(count)
      },
      deviceToParent,
      ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
    )
  },
  myRole(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (
        resolve,
        reject,
        deviceFound,
        [_, environmentFound],
        userFound
      ) => {
        const myRole = await instanceToRole(
          environmentFound,
          userFound,
          context
        )

        resolve(myRole)
      },
      deviceToParent
    )
  },
  qrCode(root, args, context) {
    return authorized(
      root.id,
      context,
      context.dataLoaders.deviceLoaderById,
      User,
      1,
      async (resolve, reject) => {
        resolve(new QRCode({ content: root.id }).svg())
      },
      deviceToParent
    )
  },
})

export default DeviceResolver
