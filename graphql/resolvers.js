import GraphQLToolsTypes from "graphql-tools-types"
import UserResolver from "./resolvers/User"
import EnvironmentResolver from "./resolvers/Environment"
import MutationResolver from "./resolvers/Mutation"
import PendingEnvironmentShareResolver from "./resolvers/PendingEnvironmentShare"
import PendingOwnerChangeResolver from "./resolvers/PendingOwnerChange"
import QueryResolver from "./resolvers/Query"
import DeviceResolver from "./resolvers/Device"
import SubscriptionsResolver from "./resolvers/subscriptions"
import NotificationResolver from "./resolvers/Notification"
import ValueResolver from "./resolvers/Value"
import ValueResolvers from "./resolvers/Values"
import PermanentTokenResolver from "./resolvers/PermanentToken"
import DateTime from "./resolvers/DateTime"
import IDResolver from "./resolvers/ID"
import SequelizeConnections from "../postgresql/models/index"
import { pubsub } from "../shared"
import { logErrorsPromise } from "./resolvers/utilities"

require("dotenv").config()

/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

const { JWT_SECRET } = process.env

const {
  User,
  Environment,
  PermanentToken,
  Device,
  Value,
  BooleanValue,
  FloatValue,
  StringValue,
  PlotValue,
  PlotNode,
  Notification,
  WebPushNotification,
  CategoryPlotNode,
  CategoryPlotValue,
} = SequelizeConnections

const resolvers = {
  User: UserResolver(SequelizeConnections),
  PendingEnvironmentShare: PendingEnvironmentShareResolver(
    SequelizeConnections
  ),
  PendingOwnerChange: PendingOwnerChangeResolver(SequelizeConnections),
  Environment: EnvironmentResolver(SequelizeConnections),
  Device: DeviceResolver(SequelizeConnections),
  Mutation: MutationResolver(
    SequelizeConnections,
    WebPushNotification,
    pubsub,
    JWT_SECRET
  ),
  Query: QueryResolver(SequelizeConnections),
  Value: ValueResolver,
  ...ValueResolvers(
    {
      BooleanValue,
      FloatValue,
      StringValue,
      PlotValue,
      PlotNode,
      CategoryPlotValue,
      CategoryPlotNode,
    },
    User,
    Device,
    Environment
  ),
  Notification: NotificationResolver(SequelizeConnections),
  PermanentToken: PermanentTokenResolver,
}

const wrappedResolvers = {
  Subscription: SubscriptionsResolver(pubsub, SequelizeConnections),
  DateTime: DateTime({ name: "DateTime" }),
  Json: GraphQLToolsTypes.JSON({ name: "Json" }),
  ID: IDResolver,
}

function wrapInLogger(resolverFunctions) {
  const wrappedResolverFunctions = {}

  for (let resolverFunctionName in resolverFunctions) {
    if (resolverFunctions.hasOwnProperty(resolverFunctionName)) {
      wrappedResolverFunctions[resolverFunctionName] = (root, args, context) =>
        logErrorsPromise(
          resolverFunctions[resolverFunctionName](root, args, context)
        )
    }
  }

  return wrappedResolverFunctions
}

for (let resolverName in resolvers) {
  if (resolvers.hasOwnProperty(resolverName)) {
    wrappedResolvers[resolverName] = wrapInLogger(resolvers[resolverName])
  }
}

export default wrappedResolvers
