import {
  instanceToRole,
  authorized,
  authorizedScalarPropsResolvers,
  deviceInheritAuthorized,
  valueToParent,
  inheritAuthorizedScalarPropsResolvers,
  deviceInheritAuthorizedScalarPropsResolvers,
  inheritAuthorized,
} from "./utilities"

const QUERY_COST = 1

const GenericResolver = (
  loaderName,
  User,
  Device,
  Environment,
  hasPermission = true
) => ({
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "createdAt",
    "updatedAt",
    "visibility",
    "cardSize",
    "name",
    "value",
    "index",
  ]),
  ...(hasPermission
    ? deviceInheritAuthorizedScalarPropsResolvers(loaderName, ["permission"])
    : []),
  device: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, valueFound) => {
        resolve({ id: valueFound.deviceId })
      }
    ),
})

const BooleanValueResolver = GenericResolver
const FloatValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "unitOfMeasurement",
    "precision",
    "min",
    "max",
  ]),
})
const StringValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "maxChars",
    "allowedValues",
  ]),
})
const PlotValueResolver = (
  loaderName,
  User,
  Device,
  Environment,
  PlotNode
) => ({
  ...GenericResolver(loaderName, User, Device, Environment, false),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "unitOfMeasurement",
    "precision",
    "min",
    "max",
    "threshold",
  ]),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, plotFound) => {
        const nodes = await PlotNode.findAll({
          where: { plotId: plotFound.id },
          limit: args.limit,
          offset: args.offset,
          order: [["id", "DESC"]],
        })
        resolve(nodes)
      }
    ),
  lastNode: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, plotFound) => {
        const node = await PlotNode.find({
          where: { plotId: plotFound.id },
          order: [["timestamp", "DESC"]],
        })
        resolve(node)
      }
    ),
})
const CategoryPlotValueResolver = (
  loaderName,
  User,
  Device,
  Environment,
  CategoryPlotNode
) => ({
  ...GenericResolver(loaderName, User, Device, Environment, false),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, ["allowedValues"]),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, plotFound) => {
        const nodes = await CategoryPlotNode.findAll({
          where: { plotId: plotFound.id },
          limit: args.limit,
          offset: args.offset,
          order: [["id", "DESC"]],
        })

        resolve(nodes)
      }
    ),
  lastNode: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, plotFound) => {
        const node = await CategoryPlotNode.find({
          where: { plotId: plotFound.id },
          order: [["timestamp", "DESC"]],
        })

        resolve(node)
      }
    ),
})

const PlotNodeResolver = (
  nodeLoader,
  valueLoader,
  User,
  Device,
  Environment
) => ({
  ...deviceInheritAuthorizedScalarPropsResolvers(nodeLoader, [
    "timestamp",
    "value",
  ]),
  device(root, args, context) {
    return deviceInheritAuthorized(
      root.id,
      context.dataLoaders[nodeLoader],
      context,
      1,
      async (resolve, reject, plotNodeFound) => {
        resolve({ id: plotNodeFound.deviceId })
      }
    )
  },
  plot(root, args, context) {
    return deviceInheritAuthorized(
      root.id,
      context.dataLoaders[nodeLoader],
      context,
      1,
      async (resolve, reject, plotNodeFound) => {
        resolve({ id: plotNodeFound.plotId })
      }
    )
  },
})

export default (
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
) => ({
  BooleanValue: BooleanValueResolver(
    "booleanValueLoaderById",
    User,
    Device,
    Environment
  ),
  FloatValue: FloatValueResolver(
    "floatValueLoaderById",
    User,
    Device,
    Environment
  ),
  StringValue: StringValueResolver(
    "stringValueLoaderById",
    User,
    Device,
    Environment
  ),
  PlotValue: PlotValueResolver(
    "plotValueLoaderById",
    User,
    Device,
    Environment,
    PlotNode
  ),
  PlotNode: PlotNodeResolver(
    "plotNodeLoaderById",
    "plotValueLoaderById",
    User,
    Device,
    Environment
  ),
  CategoryPlotValue: CategoryPlotValueResolver(
    "categoryPlotValueLoaderById",
    User,
    Device,
    Environment,
    CategoryPlotNode
  ),
  CategoryPlotNode: PlotNodeResolver(
    "categoryPlotNodeLoaderById",
    "categoryPlotValueLoaderById",
    User,
    Device,
    Environment
  ),
})
