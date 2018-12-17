import { SubscriptionServer } from "subscriptions-transport-ws"
import { execute, subscribe } from "graphql"
import jwt from "jwt-simple"
import { createServer } from "http"
import schema from "./graphql/schema"
import graphQLServer from "./app"
import { log } from "./graphql/resolvers/utilities"
import { socketToDeviceMap } from "./graphql/resolvers/utilities"
import { Device } from "./postgresql/models/index"
import { pubsub } from "./shared"

require("dotenv").config()
/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

const GRAPHQL_PORT = process.env.PORT || 3000

const httpServer = createServer(graphQLServer)
httpServer.listen(GRAPHQL_PORT, () => {
  log(`GraphiQL is now running on http://localhost:${GRAPHQL_PORT}/graphiql`, 0)

  new SubscriptionServer( // eslint-disable-line no-new
    {
      execute,
      subscribe,
      schema,
      onConnect: (connectionParams, websocket) => {
        if (!connectionParams.Authorization) {
          throw new Error("You should pass an authorization token")
        } else if (!connectionParams.Authorization.startsWith("Bearer ")) {
          throw new Error("Bearer should be prefixed with 'Bearer '")
        }
        try {
          // FIXME: does this check that the token was not forged?
          const decodedJwt = jwt.decode(
            connectionParams.Authorization.substring(7),
            process.env.JWT_SECRET
          )
          return { auth: decodedJwt, websocket }
        } catch (e) /* istanbul ignore next */ {
          if (e.message === "Token expired") {
            throw new Error("Token expired")
          } else {
            throw new Error("Malformed JWT")
          }
        }
      },
      onOperationComplete: async websocket => {
        if (socketToDeviceMap.hasOwnProperty(websocket)) {
          const { deviceId, userIds } = socketToDeviceMap[websocket]

          delete socketToDeviceMap[websocket]
          await Device.update({ online: false }, { where: { id: deviceId } })

          pubsub.publish("deviceUpdated", {
            deviceUpdated: { id: deviceId },
            userIds,
          })
        }
      },
    },
    {
      server: httpServer,
      path: "/subscriptions",
    }
  )
})

module.exports = httpServer
