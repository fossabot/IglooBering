import {
  authenticated,
  authorized,
  authorizedScalarPropsResolvers,
  valueToParent,
  inheritAuthorizedScalarPropsResolvers,
  inheritAuthorized,
} from "./utilities"

const QUERY_COST = 1

const GenericResolver = (
  Model,
  User,
  Device,
  Environment,
  hasPermission = true
) => ({
  ...authorizedScalarPropsResolvers(
    Model,
    User,
    [
      "createdAt",
      "updatedAt",
      "visibility",
      "valueDetails",
      "tileSize",
      "name",
      "value",
      "index",
    ],
    valueToParent(Environment)
  ),
  ...(hasPermission
    ? authorizedScalarPropsResolvers(
        Model,
        User,
        ["permission"],
        valueToParent(Environment)
      )
    : []),
  device: (root, args, context) =>
    authorized(
      root.id,
      context,
      Model,
      User,
      1,
      async (resolve, reject, valueFound) => {
        resolve({ id: valueFound.deviceId })
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent(Environment)
    ),
  environment: (root, args, context) =>
    authorized(
      root.id,
      context,
      Model,
      User,
      1,
      async (resolve, reject, valueFound) => {
        resolve({ id: valueFound.environmentId })
        context.billingUpdater.update(QUERY_COST)
      },
      valueToParent(Environment)
    ),
})

const BooleanValueResolver = GenericResolver
const FloatValueResolver = (Model, User, Device, Environment) => ({
  ...GenericResolver(Model, User, Device, Environment),
  ...authorizedScalarPropsResolvers(
    Model,
    User,
    ["precision", "boundaries"],
    valueToParent(Environment)
  ),
})
const StringValueResolver = (Model, User, Device, Environment) => ({
  ...GenericResolver(Model, User, Device, Environment),
  ...authorizedScalarPropsResolvers(
    Model,
    User,
    ["maxChars", "allowedValues"],
    valueToParent(Environment)
  ),
})
const PlotValueResolver = (PlotValue, PlotNode, User, Device, Environment) => ({
  ...GenericResolver(PlotValue, User, Device, Environment, false),
  ...authorizedScalarPropsResolvers(
    PlotValue,
    User,
    ["precision", "boundaries", "threshold"],
    valueToParent(Environment)
  ),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    authorized(
      root.id,
      context,
      PlotValue,
      User,
      1,
      async (resolve, reject, plotFound) => {
        const nodes = await plotFound.getPlotNodes()
        resolve(nodes)
        context.billingUpdater.update(QUERY_COST * nodes.length)
      },
      valueToParent(Environment)
    ),
})
const CategoryPlotValueResolver = (
  CategoryPlotValue,
  CategoryPlotNode,
  User,
  Device,
  Environment
) => ({
  ...GenericResolver(CategoryPlotValue, User, Device, Environment, false),
  ...authorizedScalarPropsResolvers(
    CategoryPlotValue,
    User,
    ["allowedValues"],
    valueToParent(Environment)
  ),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    authorized(
      root.id,
      context,
      CategoryPlotValue,
      User,
      1,
      async (resolve, reject, plotFound) => {
        const nodes = await plotFound.getCategoryPlotNodes()

        resolve(nodes)
        context.billingUpdater.update(QUERY_COST * nodes.length)
      },
      valueToParent(Environment)
    ),
})

const PlotNodeResolver = (PlotNode, PlotValue, User, Device, Environment) => ({
  ...inheritAuthorizedScalarPropsResolvers(
    PlotNode,
    User,
    ["timestamp", "value"],
    plotNodeFound => plotNodeFound.plotId,
    PlotValue,
    valueToParent(Environment)
  ),
  user(root, args, context) {
    return inheritAuthorized(
      root.id,
      PlotNode,
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      PlotValue,
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
      valueToParent(Environment)
    )
  },
  device(root, args, context) {
    return inheritAuthorized(
      root.id,
      PlotNode,
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      PlotValue,
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
      valueToParent(Environment)
    )
  },
  plot(root, args, context) {
    return inheritAuthorized(
      root.id,
      PlotNode,
      User,
      plotNodeFound => plotNodeFound.plotId,
      context,
      PlotValue,
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
      valueToParent(Environment)
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
  BooleanValue: BooleanValueResolver(BooleanValue, User, Device, Environment),
  FloatValue: FloatValueResolver(FloatValue, User, Device, Environment),
  StringValue: StringValueResolver(StringValue, User, Device, Environment),
  PlotValue: PlotValueResolver(PlotValue, PlotNode, User, Device, Environment),
  PlotNode: PlotNodeResolver(PlotNode, PlotValue, User, Device, Environment),
  CategoryPlotValue: CategoryPlotValueResolver(
    CategoryPlotValue,
    CategoryPlotNode,
    User,
    Device,
    Environment
  ),
  CategoryPlotNode: PlotNodeResolver(
    CategoryPlotNode,
    CategoryPlotValue,
    User,
    Device,
    Environment
  ),
})
