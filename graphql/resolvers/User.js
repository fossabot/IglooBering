import { authenticated, findAllValues, getAll } from "./utilities"

const QUERY_COST = 1

const retrieveUserScalarProp = (User, prop, acceptedTokens) => (
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
        const userFound = await User.find({ where: { id: root.id } })
        if (!userFound) {
          reject("User doesn't exist. Use `` to create one")
        } else {
          resolve(userFound[prop])
        }
      }
    },
    acceptedTokens
  )

const scalarProps = (User, props) =>
  props.reduce((acc, prop) => {
    acc[prop] = retrieveUserScalarProp(User, prop)
    return acc
  }, {})

const retrievePublicUserScalarProp = (User, prop, acceptedTokens) => (
  root,
  args,
  context
) =>
  authenticated(
    context,
    async (resolve, reject) => {
      const userFound = await User.find({ where: { id: root.id } })
      if (!userFound) {
        reject("User doesn't exist. Use `` to create one")
      } else {
        resolve(userFound[prop])
      }
    },
    acceptedTokens
  )

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
  MapValue,
  Notification,
  PendingEnvironmentShare,
  PendingOwnerChange,
}) => ({
  ...scalarProps(User, ["quietMode", "monthUsage", "emailIsVerified"]),
  email: retrievePublicUserScalarProp(User, "email", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  name: retrievePublicUserScalarProp(User, "name", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  profileIcon: retrievePublicUserScalarProp(User, "profileIcon", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  profileIconColor: retrievePublicUserScalarProp(User, "profileIconColor", [
    "TEMPORARY",
    "PERMANENT",
    "PASSWORD_RECOVERY",
  ]),
  paymentPlan: retrieveUserScalarProp(User, "paymentPlan", [
    "TEMPORARY",
    "PERMANENT",
    "SWITCH_TO_PAYING",
  ]),
  usageCap: retrieveUserScalarProp(User, "usageCap", [
    "TEMPORARY",
    "PERMANENT",
    "CHANGE_USAGE_CAP",
  ]),
  settings(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const userFound = await User.find({ where: { id: root.id } })

        resolve({
          language: userFound.settings_language,
          lengthAndMass: userFound.settings_lengthAndMass,
          temperature: userFound.settings_temperature,
          dateFormat: userFound.settings_dateFormat,
          timeFormat: userFound.settings_timeFormat,
        })
        context.billingUpdater.update(QUERY_COST)
      }
    })
  },
  devices(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
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

        resolve(devices)
        context.billingUpdater.update(QUERY_COST * devices.length)
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
        context.billingUpdater.update(
          QUERY_COST * pendingEnvironmentShares.length
        )
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
        context.billingUpdater.update(QUERY_COST * pendingOwnerChanges.length)
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
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const environments = await getAll(Environment, User, root.id)

        resolve(environments)
        context.billingUpdater.update(QUERY_COST * environments.length)
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

        // count not visualized notifications
        const totalCount = allNotifications.filter(
          notification =>
            notification.visualized.indexOf(context.auth.userId) === -1
        ).length

        resolve(totalCount)
      }
    })
  },
  notifications(root, args, context) {
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

        // the database returns ISO-format dates, so sorting the strings without casting is fine
        const compareDates = (a, b) =>
          a.date > b.date ? -1 : a.date === b.date ? 0 : 1

        resolve(allNotifications.sort(compareDates))
        context.billingUpdater.update(QUERY_COST * allNotifications.length)
      }
    })
  },
  values(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        // TODO: tag the values with the right __resolveType
        const valueModels = [
          FloatValue,
          StringValue,
          BooleanValue,
          PlotValue,
          CategoryPlotValue,
          MapValue,
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
                ...device.mapValues,
              ],
              []
            ),
          ],
          []
        )

        resolve(flattenedAllValues)
        context.billingUpdater.update(QUERY_COST * flattenedAllValues.length)
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
          MapValue,
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
                ...device.mapValues,
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
  permanentTokens(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      if (context.auth.userId !== root.id) {
        reject("You are not allowed to perform this operation")
      } else {
        const tokens = await PermanentToken.findAll({
          where: { userId: root.id },
        })

        resolve(tokens)
        context.billingUpdater.update(QUERY_COST * tokens.length)
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
