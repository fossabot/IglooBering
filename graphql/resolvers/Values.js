import {
  authenticated,
  instanceToRole,
  authorized,
  authorizedScalarPropsResolvers,
  valueToParent,
  inheritAuthorizedScalarPropsResolvers,
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
  ...authorizedScalarPropsResolvers(
    loaderName,
    [
      "createdAt",
      "updatedAt",
      "visibility",
      "unitOfMeasurement",
      "cardSize",
      "name",
      "value",
      "index",
    ],
    valueToParent
  ),
  ...(hasPermission
    ? authorizedScalarPropsResolvers(loaderName, ["permission"], valueToParent)
    : []),
  device: (root, args, context) =>
    authorized(
      root.id,
      context,
      context.dataLoaders[loaderName],
      User,
      1,
      async (resolve, reject, valueFound) => {
        resolve({ id: valueFound.deviceId })
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent
    ),
  environment: (root, args, context) =>
    authorized(
      root.id,
      context,
      context.dataLoaders[loaderName],
      User,
      1,
      async (resolve, reject, valueFound) => {
        resolve({ id: valueFound.environmentId })
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent
    ),
  myRole: (root, args, context) =>
    authorized(
      root.id,
      context,
      context.dataLoaders[loaderName],
      User,
      1,
      async (resolve, reject, valueFound, [_, environmentFound], userFound) => {
        const myRole = await instanceToRole(
          environmentFound,
          userFound,
          context
        )

        resolve(myRole)
      },
      valueToParent
    ),
})

const BooleanValueResolver = GenericResolver
const FloatValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment),
  ...authorizedScalarPropsResolvers(
    loaderName,
    [("precision", "min", "max")],
    valueToParent
  ),
})
const StringValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment),
  ...authorizedScalarPropsResolvers(
    loaderName,
    ["maxChars", "allowedValues"],
    valueToParent
  ),
})
const PlotValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment, false),
  ...authorizedScalarPropsResolvers(
    loaderName,
    [("precision", "min", "max", "threshold")],
    valueToParent
  ),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    authorized(
      root.id,
      context,
      context.dataLoaders.plotValueLoaderById,
      User,
      1,
      async (resolve, reject, plotFound) => {
        const nodes = await plotFound.getPlotNodes()
        resolve(nodes)
        context.billingUpdater.update(QUERY_COST * nodes.length)
      },
      valueToParent
    ),
})
const CategoryPlotValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment, false),
  ...authorizedScalarPropsResolvers(
    loaderName,
    ["allowedValues"],
    valueToParent
  ),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    authorized(
      root.id,
      context,
      context.dataLoaders.categoryPlotValueLoaderById,
      User,
      1,
      async (resolve, reject, plotFound) => {
        const nodes = await plotFound.getCategoryPlotNodes()

        resolve(nodes)
        context.billingUpdater.update(QUERY_COST * nodes.length)
      },
      valueToParent
    ),
})

const PlotNodeResolver = (
  nodeLoader,
  valueLoader,
  User,
  Device,
  Environment
) => ({
  ...inheritAuthorizedScalarPropsResolvers(
    nodeLoader,
    User,
    ["timestamp", "value"],
    plotNodeFound => plotNodeFound.plotId,
    valueLoader,
    valueToParent
  ),
  user(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders[nodeLoader],
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      context.dataLoaders[valueLoader],
      1,
      async (
        resolve,
        reject,
        plotNodeFound,
        plotValueFound,
        plotValueAndParents
      ) => {
        resolve({ id: plotNodeFound.userId })
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent
    )
  },
  device(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders[nodeLoader],
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      context.dataLoaders[valueLoader],
      1,
      async (
        resolve,
        reject,
        plotNodeFound,
        plotValueFound,
        plotValueAndParents
      ) => {
        resolve({ id: plotNodeFound.deviceId })
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent
    )
  },
  plot(root, args, context) {
    return inheritAuthorized(
      root.id,
      context.dataLoaders[nodeLoader],
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      context.dataLoaders[valueLoader],
      1,
      async (
        resolve,
        reject,
        plotNodeFound,
        plotValueFound,
        plotValueAndParents
      ) => {
        resolve(plotValueFound.dataValues)
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent
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
    Environment
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
    Environment
  ),
  CategoryPlotNode: PlotNodeResolver(
    "categoryPlotNodeLoaderById",
    "categoryPlotValueLoaderById",
    User,
    Device,
    Environment
  ),
})
