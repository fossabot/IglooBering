import { authenticated, authorizationLevel, firstResolve } from "./utilities"

const QUERY_COST = 1

const ValueResolver = (Values, User, Device, Environment) => ({
  __resolveType: (root, context) =>
    authenticated(context, async (resolve, reject) => {
      const NOT_ALLOWED = "You are not allowed to perform this operation"
      const NOT_EXIST = "The requested resource does not exist"
      const models = Object.values(Values)

      // TODO: resolve this stuff
      function indexToType(idx) {
        const modelName = Object.keys(Values)[idx]

        return modelName
      }

      // race all the models to find the looked for id, if a value is found
      // it is returned otherwise the correct error is returned
      const modelFetches = models.map(
        (Model, idx) =>
          new Promise(async (resolveInner, rejectInner) => {
            const resourceFound = await Model.find({
              where: { id: root.id },
            })

            /* istanbul ignore next */
            if (!resourceFound) {
              rejectInner(NOT_EXIST)
            } else {
              const userFound = await context.dataLoaders.userLoaderById.load(
                context.auth.userId
              )
              const environmentFound = await Environment.find({
                where: { id: resourceFound.environmentId },
              })

              if ((await authorizationLevel(environmentFound, userFound)) < 1) {
                /* istanbul ignore next */
                rejectInner(NOT_ALLOWED)
              } else {
                resolveInner(indexToType(idx))
              }
            }
          })
      )

      firstResolve(modelFetches)
        .then(typeFound => resolve(typeFound))
        /* istanbul ignore next */
        .catch(e => {
          // choose the correct error, because normally most models
          // will reject with NOT_EXIST, simply because the value
          // looked for is of another type

          reject(
            e.reduce(
              (acc, val) =>
                acc === NOT_ALLOWED || val === NOT_ALLOWED
                  ? NOT_ALLOWED
                  : NOT_EXIST
            )
          )
        })
    }),
})

export default ValueResolver
