import { authenticated, authorizationLevel, firstResolve } from "./utilities"

const ValueResolver = {
  __resolveType: (root, context) =>
    authenticated(
      context,
      async (resolve, reject) => {
        const NOT_ALLOWED = "You are not allowed to perform this operation"
        const NOT_EXIST = "The requested resource does not exist"

        const dataLoaderMap = {
          BooleanValue: context.dataLoaders.booleanValueLoaderById,
          CategoryPlotValue: context.dataLoaders.categoryPlotValueLoaderById,
          FloatValue: context.dataLoaders.floatValueLoaderById,
          PlotValue: context.dataLoaders.plotValueLoaderById,
          StringValue: context.dataLoaders.stringValueLoaderById,
        }
        const dataLoaders = Object.values(dataLoaderMap)
        const __resolveTypes = Object.keys(dataLoaderMap)

        // race all the models to find the looked for id, if a value is found
        // it is returned otherwise the correct error is returned
        const instancesLoaded = dataLoaders.map(
          (dataLoader, idx) =>
            new Promise(async (resolveInner, rejectInner) => {
              const resourceFound = await dataLoader.load(root.id)

              /* istanbul ignore next */
              if (!resourceFound) {
                rejectInner(NOT_EXIST)
              } else if (context.auth.userId) {
                const userFound = await context.dataLoaders.userLoaderById.load(
                  context.auth.userId
                )
                const environmentFound = await context.dataLoaders.environmentLoaderById.load(
                  resourceFound.environmentId
                )
                const deviceFound = await context.dataLoaders.deviceLoaderById.load(
                  resourceFound.deviceId
                )

                if (
                  deviceFound.producerId !== context.auth.userId &&
                  (await authorizationLevel(
                    environmentFound,
                    userFound,
                    context
                  )) < 1
                ) {
                  /* istanbul ignore next */
                  rejectInner(NOT_ALLOWED)
                } else {
                  resolveInner(__resolveTypes[idx])
                }
              } else if (context.auth.deviceId) {
                if (resourceFound.deviceId === context.auth.deviceId) {
                  resolveInner(__resolveTypes[idx])
                } else {
                  rejectInner(NOT_ALLOWED)
                }
              } else {
                rejectInner(NOT_ALLOWED)
              }
            })
        )

        firstResolve(instancesLoaded)
          .then(typeFound => resolve(typeFound))
          /* istanbul ignore next */
          .catch(e => {
            // choose the correct error, because normally most models
            // will reject with NOT_EXIST, simply because the value
            // looked for is of another type

            reject(
              e.reduce((acc, val) =>
                acc === NOT_ALLOWED || val === NOT_ALLOWED
                  ? NOT_ALLOWED
                  : NOT_EXIST
              )
            )
          })
      },
      ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
    ),
}

export default ValueResolver
