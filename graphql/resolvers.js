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
import DateTime from "./resolvers/DateTime"
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
  MapValue,
  Notification,
  WebPushNotification,
  StringPlotNode,
  StringPlotValue,
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
  Value: ValueResolver(
    {
      BooleanValue,
      FloatValue,
      StringValue,
      PlotValue,
      StringPlotValue,
      MapValue,
    },
    User,
    Device,
    Environment
  ),
  ...ValueResolvers(
    {
      BooleanValue,
      FloatValue,
      StringValue,
      PlotValue,
      PlotNode,
      StringPlotValue,
      StringPlotNode,
    },
    User,
    Device,
    Environment
  ),
  Notification: NotificationResolver(SequelizeConnections),
}

const wrappedResolvers = {
  Subscription: SubscriptionsResolver(pubsub, SequelizeConnections),
  DateTime: DateTime({ name: "DateTime" }),
  Json: GraphQLToolsTypes.JSON({ name: "Json" }),
}

function wrapInLogger(resolverFunctions) {
  const wrappedResolverFunctions = {}

  for (let resolverFunctionName in resolverFunctions) {
    if (resolverFunctions.hasOwnProperty(resolverFunctionName)) {
      wrappedResolverFunctions[resolverFunctionName] = (root, args, context) =>
        logErrorsPromise(
          "login",
          1,
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
