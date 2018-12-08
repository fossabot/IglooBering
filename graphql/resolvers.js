import GraphQLToolsTypes from "graphql-tools-types"
import UserResolver from "./resolvers/User"
import BoardResolver from "./resolvers/Board"
import MutationResolver from "./resolvers/Mutation"
import PendingBoardShareResolver from "./resolvers/PendingBoardShare"
import PendingOwnerChangeResolver from "./resolvers/PendingOwnerChange"
import QueryResolver from "./resolvers/Query"
import DeviceResolver from "./resolvers/Device"
import SubscriptionsResolver from "./resolvers/subscriptions"
import NotificationResolver from "./resolvers/Notification"
import ValueResolver from "./resolvers/Value"
import ValueResolvers from "./resolvers/Values"
import DateTime from "./resolvers/DateTime"
import SequelizeConnections from "../postgresql/databaseConnection"
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
  Board,
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
  WebPushSubscription,
  StringPlotNode,
  StringPlotValue,
} = SequelizeConnections

const resolvers = {
  User: UserResolver(SequelizeConnections),
  PendingBoardShare: PendingBoardShareResolver(SequelizeConnections),
  PendingOwnerChange: PendingOwnerChangeResolver(SequelizeConnections),
  Board: BoardResolver(SequelizeConnections),
  Device: DeviceResolver(SequelizeConnections),
  Mutation: MutationResolver(
    SequelizeConnections,
    WebPushSubscription,
    pubsub,
    JWT_SECRET
  ),
  Query: QueryResolver(SequelizeConnections),
  Subscription: SubscriptionsResolver(pubsub, SequelizeConnections),
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
    Board
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
    Board
  ),
  Notification: NotificationResolver(SequelizeConnections),
}

const wrappedResolvers = {
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
