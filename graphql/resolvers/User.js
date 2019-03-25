import { authenticated, getAll } from "./utilities"
import { Op } from "sequelize"
import SqlString from "sqlstring"

const QUERY_COST = 1
const isNotNullNorUndefined = value => value !== undefined && value !== null

const retrieveUserScalarProp = (prop, acceptedTokens) => (
  root,
  args,
  context
) =>
  authenticated(
    context,
    async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const userFound = await context.dataLoaders.userLoaderById.load(root.id)
        if (!userFound) {
          reject("User doesn't exist. Use `signUp` to create one")
        } else {
          resolve(userFound[prop])
        }
      }
    },
    acceptedTokens
  )

const scalarProps = props =>
  props.reduce((acc, prop) => {
    acc[prop] = retrieveUserScalarProp(prop)
    return acc
  }, {})

const retrievePublicUserScalarProp = prop => (root, args, context) => async (
  resolve,
  reject
) => {
  const userFound = await context.dataLoaders.userLoaderById.load(root.id)
  if (!userFound) {
    reject("User doesn't exist. Use `signUp` to create one")
  } else {
    resolve(userFound[prop])
  }
}

const publicScalarProps = props =>
  props.reduce((acc, prop) => {
    acc[prop] = retrievePublicUserScalarProp(prop)
    return acc
  }, {})

const UserResolver = ({
  User,
  PermanentToken,
  Device,
  Environment,
  FloatValue,
  StringValue,
  BooleanValue,
  PlotValue,
  CategoryPlotValue,
  Notification,
  PendingEnvironmentShare,
  PendingOwnerChange,
  sequelize,
}) => ({
  ...scalarProps(["quietMode", "devMode"]),
  ...publicScalarProps([
    "email",
    "name",
    "profileIconColor",
    "primaryAuthenticationMethods",
    "secondaryAuthenticationMethods",
    "emailIsVerified",
  ]),
  settings(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const userFound = await context.dataLoaders.userLoaderById.load(root.id)

        resolve({
          id: userFound.id,
          language: userFound.settings_language,
          lengthAndMass: userFound.settings_lengthAndMass,
          temperature: userFound.settings_temperature,
          dateFormat: userFound.settings_dateFormat,
          timeFormat: userFound.settings_timeFormat,
          passwordChangeEmail: userFound.settings_passwordChangeEmail,
          pendingOwnerChangeReceivedEmail:
            userFound.settings_pendingOwnerChangeReceivedEmail,
          pendingEnvironmentShareReceivedEmail:
            userFound.settings_pendingEnvironmentShareReceivedEmail,
          pendingOwnerChangeAcceptedEmail:
            userFound.settings_pendingOwnerChangeAcceptedEmail,
          pendingEnvironmentShareAcceptedEmail:
            userFound.settings_pendingEnvironmentShareAcceptedEmail,
          permanentTokenCreatedEmail:
            userFound.settings_permanentTokenCreatedEmail,
        })
      }
    })
  },
  deviceCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        //TODO: use a count instead
        const devicesInheritedByEnvironments = await getAll(
          Environment,
          User,
          root.id,
          [{ model: Device }]
        )

        const devices = devicesInheritedByEnvironments.reduce(
          (acc, curr) => [...acc, ...curr.devices],
          []
        )

        resolve(devices.length)
      }
    })
  },
  pendingEnvironmentShares(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const pendingEnvironmentShares = await PendingEnvironmentShare.findAll({
          where: { receiverId: context.auth.userId },
        })

        resolve(pendingEnvironmentShares)
      }
    })
  },
  pendingEnvironmentShareCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const pendingEnvironmentShareCount = await PendingEnvironmentShare.count(
          {
            where: { receiverId: context.auth.userId },
          }
        )

        resolve(pendingEnvironmentShareCount)
      }
    })
  },
  pendingOwnerChanges(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const pendingOwnerChanges = await PendingOwnerChange.findAll({
          where: { receiverId: context.auth.userId },
        })

        resolve(pendingOwnerChanges)
      }
    })
  },
  pendingOwnerChangeCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const pendingOwnerChanges = await PendingOwnerChange.count({
          where: { receiverId: context.auth.userId },
        })

        resolve(pendingOwnerChanges)
      }
    })
  },
  environmentCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        // TODO: use count query instead
        const environments = await getAll(Environment, User, root.id)

        resolve(environments.length)
      }
    })
  },
  environments(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (
        args.sortBy === "index" &&
        isNotNullNorUndefined(args.sortDirection)
      ) {
        reject("Cannot set sort direction when sorting by index")
        return
      } else if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
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

        const parseEnvironmentFilter = filter => {
          if (!filter) return ""
          filter.hasOwnProperty = Object.prototype.hasOwnProperty

          const filtersStack = []
          if (filter.hasOwnProperty("AND"))
            filtersStack.push(
              `(${filter.AND.map(parseEnvironmentFilter)
                .filter(query => query !== "")
                .join(" AND ")})`
            )
          if (filter.hasOwnProperty("NOT") && filter.NOT !== null) {
            const parsedNot = parseEnvironmentFilter(filter.NOT)
            if (parsedNot) filtersStack.push(`NOT (${parsedNot})`)
            else filtersStack.push("false")
          }
          if (filter.hasOwnProperty("OR"))
            filtersStack.push(
              `(${filter.OR.map(parseEnvironmentFilter)
                .filter(query => query !== "")
                .join(" OR ")})`
            )
          if (filter.hasOwnProperty("muted"))
            filtersStack.push(
              `(public."environments"."muted" = '${filter.muted}')`
            )
          if (filter.hasOwnProperty("name"))
            filtersStack.push(
              parseRawStringFilter(filter.name, `public."environments"."name"`)
            )

          return filtersStack.filter(query => query !== "").join(" AND ")
        }

        const whereQuery = parseEnvironmentFilter(args.filter)

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

        const query = `
        SELECT id FROM (
        SELECT  public."environments".id as id,
                public."environments".index as index,
                public."environments".name as name,
                public."environments"."updatedAt" as "updatedAt"
          FROM
            public."users" 
            
            LEFT JOIN public."environmentAdmins" ON public."environmentAdmins"."userId" = public."users".id
            LEFT JOIN public."environmentEditors" ON public."environmentEditors"."userId" = public."users".id
            LEFT JOIN public."environmentSpectators" ON public."environmentSpectators"."userId" = public."users".id
          
            LEFT JOIN public."environments" ON 
              public."environments"."ownerId" = public."users".id OR
              public."environments"."id" = public."environmentAdmins"."environmentId" OR
              public."environments"."id" = public."environmentEditors"."environmentId" OR
              public."environments"."id" = public."environmentSpectators"."environmentId"
          
          WHERE
            (public."users".id = '${context.auth.userId}')
            ${whereQuery !== "" ? "AND " + whereQuery : ""}
          GROUP BY public."environments".id
          ) sub
          WHERE id IS NOT NULL
          ${orderQuery}
          ${limitQuery};
        `

        const environments = await sequelize.query(query, {
          model: Environment,
          mapToModel: true,
          type: sequelize.QueryTypes.SELECT,
        })

        resolve(environments)
      }
    })
  },
  notificationCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const devicesInheritedByEnvironments = await getAll(
          Environment,
          User,
          root.id,
          [{ model: Device, include: [{ model: Notification }] }]
        )

        // flattens the notifications
        const allNotifications = devicesInheritedByEnvironments.reduce(
          (acc, environment) => [
            ...acc,
            ...environment.devices.reduce(
              (acc, device) => [...acc, ...device.notifications],
              []
            ),
          ],
          []
        )

        // count not read notifications
        const totalCount = allNotifications.filter(
          notification =>
            notification.notRead.indexOf(context.auth.userId) !== -1
        ).length

        resolve(totalCount)
      }
    })
  },
  valueCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        // TODO: use a count instead
        const valueModels = [
          FloatValue,
          StringValue,
          BooleanValue,
          PlotValue,
          CategoryPlotValue,
        ]

        const valuesInheritedFromEnvironments = await getAll(
          Environment,
          User,
          root.id,
          [
            {
              model: Device,
              include: valueModels.map(Model => ({ model: Model })),
            },
          ]
        )
        const flattenedAllValues = valuesInheritedFromEnvironments.reduce(
          (acc, curr) => [
            ...acc,
            ...curr.devices.reduce(
              (acc, device) => [
                ...acc,
                ...device.floatValues,
                ...device.stringValues,
                ...device.booleanValues,
                ...device.plotValues,
                ...device.categoryPlotValues,
              ],
              []
            ),
          ],
          []
        )

        resolve(flattenedAllValues.length)
      }
    })
  },
  developerDevices(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const parseDeveloperDeviceFilter = filter => {
          if (!filter) return {}

          filter.hasOwnProperty = Object.prototype.hasOwnProperty

          const parsedFilter = {}
          if (filter.hasOwnProperty("AND"))
            parsedFilter[Op.and] = filter.AND.map(parseDeveloperDeviceFilter)
          if (filter.hasOwnProperty("OR"))
            parsedFilter[Op.or] = filter.OR.map(parseDeveloperDeviceFilter)
          if (filter.hasOwnProperty("NOT") && filter.NOT !== null)
            parsedFilter[Op.not] = parseDeveloperDeviceFilter(filter.NOT)
          if (filter.hasOwnProperty("claimed")) {
            if (filter.claimed)
              parsedFilter.environmentId = {
                [Op.ne]: null,
              }
            else parsedFilter.environmentId = null
          }
          return parsedFilter
        }
        const devices = await Device.findAll({
          where: {
            ...parseDeveloperDeviceFilter(args.filter),
            producerId: root.id,
          },
          limit: args.limit,
          offset: args.offset,
          order: [["id", "DESC"]],
        })

        resolve(devices)
      }
    })
  },
  permanentTokens(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const tokens = await PermanentToken.findAll({
          where: { userId: root.id },
          limit: args.limit,
          offset: args.offset,
          order: [["id", "DESC"]],
        })

        resolve(tokens)
      }
    })
  },
  permanentTokenCount(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const tokens = await PermanentToken.count({
          where: { userId: root.id },
        })

        resolve(tokens)
      }
    })
  },
})

export default UserResolver
