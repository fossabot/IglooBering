require("dotenv").config()
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
  hasPermission = true,
  hasValue = true
) => ({
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "createdAt",
    "updatedAt",
    "visibility",
    "cardSize",
    "name",
    "index",
  ]),
  ...(hasPermission
    ? deviceInheritAuthorizedScalarPropsResolvers(loaderName, ["permission"])
    : []),
  ...(hasPermission
    ? deviceInheritAuthorizedScalarPropsResolvers(loaderName, ["value"])
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
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "unitOfMeasurement",
    "precision",
    "min",
    "max",
  ]),
})
const FileValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "mimeType",
    "fileName",
  ]),
  // overriding GenericResolver's value
  value: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, fileFound) => {
        resolve(`https://${process.env.BASE_URL}/file/${fileFound.id}`)
      }
    ),
})
const StringValueResolver = (loaderName, User, Device, Environment) => ({
  ...GenericResolver(loaderName, User, Device, Environment),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "maxChars",
    "allowedValues",
  ]),
})
const FloatSeriesValueResolver = (
  loaderName,
  User,
  Device,
  Environment,
  FloatSeriesNode
) => ({
  ...GenericResolver(loaderName, User, Device, Environment, false, false),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, [
    "unitOfMeasurement",
    "precision",
    "min",
    "max",
    "threshold",
  ]),
  nodeCount: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, seriesFound) => {
        const count = await FloatSeriesNode.count({
          where: { seriesId: seriesFound.id },
        })
        resolve(count)
      }
    ),
  // overriding GenericResolver's value
  nodes: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, seriesFound) => {
        const nodes = await FloatSeriesNode.findAll({
          where: { seriesId: seriesFound.id },
          limit: args.limit,
          offset: args.offset,
          order: [["timestamp", "DESC"]],
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
      async (resolve, reject, seriesFound) => {
        const node = await FloatSeriesNode.find({
          where: { seriesId: seriesFound.id },
          order: [["timestamp", "DESC"]],
        })
        resolve(node)
      }
    ),
})
const CategorySeriesValueResolver = (
  loaderName,
  User,
  Device,
  Environment,
  CategorySeriesNode
) => ({
  ...GenericResolver(loaderName, User, Device, Environment, false, false),
  ...deviceInheritAuthorizedScalarPropsResolvers(loaderName, ["allowedValues"]),
  nodeCount: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, seriesFound) => {
        const count = await CategorySeriesNode.count({
          where: { seriesId: seriesFound.id },
        })
        resolve(count)
      }
    ),
  // overriding GenericResolver's value
  nodes: (root, args, context) =>
    deviceInheritAuthorized(
      root.id,
      context.dataLoaders[loaderName],
      context,
      1,
      async (resolve, reject, seriesFound) => {
        const nodes = await CategorySeriesNode.findAll({
          where: { seriesId: seriesFound.id },
          limit: args.limit,
          offset: args.offset,
          order: [["timestamp", "DESC"]],
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
      async (resolve, reject, seriesFound) => {
        const node = await CategorySeriesNode.find({
          where: { seriesId: seriesFound.id },
          order: [["timestamp", "DESC"]],
        })

        resolve(node)
      }
    ),
})

const FloatSeriesNodeResolver = (
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
      async (resolve, reject, floatSeriesNodeFound) => {
        resolve({ id: floatSeriesNodeFound.deviceId })
      }
    )
  },
  series(root, args, context) {
    return deviceInheritAuthorized(
      root.id,
      context.dataLoaders[nodeLoader],
      context,
      1,
      async (resolve, reject, floatSeriesNodeFound) => {
        resolve({ id: floatSeriesNodeFound.seriesId })
      }
    )
  },
})

export default (
  { FloatSeriesNode, CategorySeriesNode },
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
  FileValue: FileValueResolver(
    "fileValueLoaderById",
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
  FloatSeriesValue: FloatSeriesValueResolver(
    "floatSeriesValueLoaderById",
    User,
    Device,
    Environment,
    FloatSeriesNode
  ),
  FloatSeriesNode: FloatSeriesNodeResolver(
    "floatSeriesNodeLoaderById",
    "floatSeriesValueLoaderById",
    User,
    Device,
    Environment
  ),
  CategorySeriesValue: CategorySeriesValueResolver(
    "categorySeriesValueLoaderById",
    User,
    Device,
    Environment,
    CategorySeriesNode
  ),
  CategorySeriesNode: FloatSeriesNodeResolver(
    "categorySeriesNodeLoaderById",
    "categorySeriesValueLoaderById",
    User,
    Device,
    Environment
  ),
})
