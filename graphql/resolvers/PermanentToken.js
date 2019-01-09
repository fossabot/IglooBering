import { authenticated, authorizationLevel, firstResolve } from "./utilities"

const QUERY_COST = 1

const PermanentTokenResolver = {
  user(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const databaseToken = await context.dataLoaders.permanentTokenLoaderById.load(
        root.id
      )
      if (!databaseToken) {
        reject("This token doesn't exist")
      } else if (databaseToken.userId !== context.auth.userId) {
        reject("This token is not yours")
      } else {
        resolve({ id: databaseToken.userId })
      }
    })
  },
  name(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const databaseToken = await context.dataLoaders.permanentTokenLoaderById.load(
        root.id
      )

      if (!databaseToken) {
        reject("This token doesn't exist")
      } else if (databaseToken.userId !== context.auth.userId) {
        reject("This token is not yours")
      } else {
        resolve(databaseToken.name)
      }
    })
  },
  lastUsed(root, args, context) {
    return authenticated(context, async (resolve, reject) => {
      const databaseToken = await context.dataLoaders.permanentTokenLoaderById.load(
        root.id
      )

      if (!databaseToken) {
        reject("This token doesn't exist")
      } else if (databaseToken.userId !== context.auth.userId) {
        reject("This token is not yours")
      } else {
        resolve(databaseToken.lastUsed)
      }
    })
  },
}

export default PermanentTokenResolver
