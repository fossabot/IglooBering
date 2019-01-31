import {
  authorized,
  findAllValues,
  authorizedScalarPropsResolvers,
  deviceToParent,
  instanceToRole,
} from "./utilities"
import { Op } from "sequelize"
import SqlString from "sqlstring"

const QUERY_COST = 1

const DeviceResolver = ({
  Device,
  User,
  Environment,
  BooleanValue,
  FloatValue,
  StringValue,
  PlotValue,
  CategoryPlotValue,
  MapValue,
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
        SELECT public."mapValues".id ${additionalSelect(
          "mapValues"
        )} FROM public."mapValues"
          WHERE public."mapValues"."deviceId" = '${root.id}' ${whereQuery(
          "mapValues"
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

        context.billingUpdater.update(QUERY_COST * valuesFound.length)
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
        resolve({ id: deviceFound.environmentId })

        context.billingUpdater.update(QUERY_COST)
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
          where: { deviceId: deviceFound.id },
          limit: args.limit,
          offset: args.offset,
          order: [["id", "DESC"]],
        })

        // the database returns ISO-format dates, so sorting the strings without casting is fine
        const compareDates = (a, b) =>
          a.date > b.date ? -1 : a.date === b.date ? 0 : 1

        resolve(notifications.sort(compareDates))
        context.billingUpdater.update(QUERY_COST * notifications.length)
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
            notRead: { [Op.contains]: [context.auth.userId] },
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
})

export default DeviceResolver
