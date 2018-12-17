import {
  authorizedScalarPropsResolvers,
  authorized,
  instanceToRole,
  environmentToParent,
  authenticated,
  authorizationLevel,
} from "./utilities"
import { Op } from "sequelize"

const QUERY_COST = 1

const rolesResolver = (roleName, Environment, User) => (root, args, context) =>
  authorized(
    root.id,
    context,
    Environment,
    User,
    1,
    async (resolve, reject, found) => {
      const environmentFound = await Environment.find({
        where: { id: root.id },
        include: [{ model: User, as: roleName }],
      })

      resolve(environmentFound[roleName])

      context.billingUpdater.update(
        QUERY_COST * environmentFound[roleName].length
      )
    },
    environmentToParent
  )

const retrievePublicEnvironmentScalarProp = (Environment, prop) => (
  root,
  args,
  context
) =>
  authenticated(context, async (resolve, reject) => {
    const environmentFound = await Environment.find({ where: { id: root.id } })
    if (!environmentFound) {
      reject("The requested resource does not exist")
    } else {
      resolve(environmentFound[prop])
    }
  })

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
    Environment,
    User,
    ["avatar", "createdAt", "updatedAt", "index"],
    environmentToParent
  ),
  name: retrievePublicEnvironmentScalarProp(Environment, "name"),
  muted(root, args, context) {
    return authorized(
      root.id,
      context,
      Environment,
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
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        resolve({
          id: environmentFound.ownerId,
        })

        context.billingUpdater.update(QUERY_COST)
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
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const devices = await Device.findAll({
          where: { environmentId: root.id },
        })

        resolve(devices)

        context.billingUpdater.update(QUERY_COST * devices.length)
      },
      environmentToParent
    )
  },
  deviceCount(root, args, context) {
    return authorized(
      root.id,
      context,
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const devices = await Device.count({
          where: { environmentId: root.id },
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
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

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
        if ((await authorizationLevel(environmentFound, userFound)) < 3) {
          resolve(null)
          return
        }

        const pendingEnvironmentShares = await PendingEnvironmentShare.findAll({
          where: { environmentId: root.id },
        })

        resolve(pendingEnvironmentShares)
        context.billingUpdater.update(
          QUERY_COST * pendingEnvironmentShares.length
        )
      },
      environmentToParent
    )
  },
  pendingEnvironmentShareCount(root, args, context) {
    return authorized(
      root.id,
      context,
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

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
        if ((await authorizationLevel(environmentFound, userFound)) < 3) {
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
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

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
        if ((await authorizationLevel(environmentFound, userFound)) < 3) {
          resolve(null)
          return
        }

        const pendingOwnerChanges = await PendingOwnerChange.findAll({
          where: { environmentId: root.id },
        })

        resolve(pendingOwnerChanges)
        context.billingUpdater.update(QUERY_COST * pendingOwnerChanges.length)
      },
      environmentToParent
    )
  },
  pendingOwnerChangeCount(root, args, context) {
    return authorized(
      root.id,
      context,
      Environment,
      User,
      1,
      async (resolve, reject, environmentFound) => {
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

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
        if ((await authorizationLevel(environmentFound, userFound)) < 3) {
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
      Environment,
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
              [Op.not]: {
                visualized: { [Op.contains]: [context.auth.userId] },
              },
            },
          })
        )

        const notificationCounts = await Promise.all(notificationCountsPromises)
        const totalCount = notificationCounts.reduce((a, b) => a + b, 0)

        resolve(totalCount)
        context.billingUpdater.update(QUERY_COST)
      },
      environmentToParent
    )
  },
  myRole(root, args, context) {
    return authorized(
      root.id,
      context,
      Environment,
      User,
      1,
      async (
        resolve,
        reject,
        environmentFound,
        environmentAndParents,
        userFound
      ) => {
        const myRole = await instanceToRole(environmentFound, userFound)

        resolve(myRole)
      },
      environmentToParent
    )
  },
})

export default EnvironmentResolver