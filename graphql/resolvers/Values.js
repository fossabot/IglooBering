import {
  authenticated,
  authorized,
  authorizedScalarPropsResolvers,
  logErrorsPromise,
  valueToParent,
  inheritAuthorizedScalarPropsResolvers,
  inheritAuthorized,
} from './utilities'

const QUERY_COST = 1

const GenericResolver = (Model, User, Device, Board) => ({
  ...authorizedScalarPropsResolvers(
    Model,
    User,
    [
      'createdAt',
      'updatedAt',
      'permission',
      'visibility',
      'valueDetails',
      'tileSize',
      'customName',
      'value',
      'index',
    ],
    valueToParent(Board),
  ),
  device: (root, args, context) =>
    logErrorsPromise(
      'device ValueResolver',
      135,
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
        valueToParent(Board),
      ),
    ),
  board: (root, args, context) =>
    logErrorsPromise(
      'board ValueResolver',
      135,
      authorized(
        root.id,
        context,
        Model,
        User,
        1,
        async (resolve, reject, valueFound) => {
          resolve({ id: valueFound.boardId })
          context.billingUpdater.update(QUERY_COST)
        },
        valueToParent(Board),
      ),
    ),
})

const BooleanValueResolver = GenericResolver
const FloatValueResolver = (Model, User, Device, Board) => ({
  ...GenericResolver(Model, User, Device, Board),
  ...authorizedScalarPropsResolvers(
    Model,
    User,
    ['precision', 'boundaries'],
    valueToParent(Board),
  ),
})
const StringValueResolver = (Model, User, Device, Board) => ({
  ...GenericResolver(Model, User, Device, Board),
  ...authorizedScalarPropsResolvers(
    Model,
    User,
    ['maxChars', 'allowedValues'],
    valueToParent(Board),
  ),
})
const PlotValueResolver = (PlotValue, PlotNode, User, Device, Board) => ({
  ...GenericResolver(PlotValue, User, Device, Board),
  ...authorizedScalarPropsResolvers(
    PlotValue,
    User,
    ['precision', 'boundaries', 'threshold'],
    valueToParent(Board),
  ),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    logErrorsPromise(
      'PlotValueResolver',
      135,
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
        valueToParent(Board),
      ),
    ),
})
const StringPlotValueResolver = (
  StringPlotValue,
  StringPlotNode,
  User,
  Device,
  Board,
) => ({
  ...GenericResolver(StringPlotValue, User, Device, Board),
  ...authorizedScalarPropsResolvers(
    StringPlotValue,
    User,
    ['allowedValues'],
    valueToParent(Board),
  ),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    logErrorsPromise(
      'StringPlotValueResolver',
      135,
      authorized(
        root.id,
        context,
        StringPlotValue,
        User,
        1,
        async (resolve, reject, plotFound) => {
          const nodes = await plotFound.getStringPlotNodes()

          resolve(nodes)
          context.billingUpdater.update(QUERY_COST * nodes.length)
        },
        valueToParent(Board),
      ),
    ),
})

const PlotNodeResolver = (PlotNode, PlotValue, User, Device, Board) => ({
  ...inheritAuthorizedScalarPropsResolvers(
    PlotNode,
    User,
    ['timestamp', 'value'],
    plotNodeFound => plotNodeFound.plotId,
    PlotValue,
    valueToParent(Board),
  ),
  user(root, args, context) {
    return logErrorsPromise(
      'PlotNodeResolver user resolver',
      136,
      inheritAuthorized(
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
          plotValueAndParents,
        ) => {
          resolve({ id: plotNodeFound.userId })
          context.billingUpdater.update(QUERY_COST)
        },
        valueToParent(Board),
      ),
    )
  },
  device(root, args, context) {
    return logErrorsPromise(
      'PlotNodeResolver device resolver',
      137,
      inheritAuthorized(
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
          plotValueAndParents,
        ) => {
          resolve({ id: plotNodeFound.deviceId })
          context.billingUpdater.update(QUERY_COST)
        },
        valueToParent(Board),
      ),
    )
  },
  plot(root, args, context) {
    return logErrorsPromise(
      'PlotNodeResolver plot resolver',
      138,
      inheritAuthorized(
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
          plotValueAndParents,
        ) => {
          resolve(plotValueFound.dataValues)
          context.billingUpdater.update(QUERY_COST)
        },
        valueToParent(Board),
      ),
    )
  },
})

export default (
  {
    BoolValue,
    FloatValue,
    StringValue,
    PlotValue,
    PlotNode,
    StringPlotValue,
    StringPlotNode,
  },
  User,
  Device,
  Board,
) => ({
  BooleanValue: BooleanValueResolver(BoolValue, User, Device, Board),
  FloatValue: FloatValueResolver(FloatValue, User, Device, Board),
  StringValue: StringValueResolver(StringValue, User, Device, Board),
  PlotValue: PlotValueResolver(PlotValue, PlotNode, User, Device, Board),
  PlotNode: PlotNodeResolver(PlotNode, PlotValue, User, Device, Board),
  StringPlotValue: StringPlotValueResolver(
    StringPlotValue,
    StringPlotNode,
    User,
    Device,
    Board,
  ),
  StringPlotNode: PlotNodeResolver(
    StringPlotNode,
    StringPlotValue,
    User,
    Device,
    Board,
  ),
})
