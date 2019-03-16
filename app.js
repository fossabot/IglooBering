require("dotenv").config()
/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}
import createDataLoaders from "./dataloaders/index"
import express from "express"
import { graphqlExpress, graphiqlExpress } from "apollo-server-express"
import bodyParser from "body-parser"
import schema from "./graphql/schema"
import expressJwt from "express-jwt"
import cors from "cors"
import webpush from "web-push"
import { pipeStreamToS3, getObjectOwner } from "./s3helpers"
import Busboy from "connect-busboy"
import AWS from "aws-sdk"
import path from "path"
import {
  PermanentToken,
  WebPushNotification,
  User,
} from "./postgresql/models/index"
import jwt from "jwt-simple"
import { GenerateUserBillingBatcher } from "./graphql/resolvers/utilities"
import { pubsub } from "./shared"
import {
  isUserBlocked,
  increaseUserAccessCount,
  isIpBlocked,
  increaseIpAccessCount,
  isDeviceBlocked,
  increaseDeviceAccessCount,
} from "./redis"
import depthLimit from "graphql-depth-limit"

const expressPlayground = require("graphql-playground-middleware-express")
  .default

webpush.setVapidDetails(
  "http://igloo.witlab.io/",
  process.env.PUBLIC_VAPID_KEY,
  process.env.PRIVATE_VAPID_KEY
)

const GRAPHQL_PORT = process.env.PORT || 3000
/* istanbul ignore next */
const WEBSOCKET_URL =
  process.env.NODE_ENV !== "development"
    ? `wss://${process.env.BASE_URL}/subscriptions`
    : `ws://localhost:${GRAPHQL_PORT}/subscriptions`
const app = express()

app.use(cors())
app.use(bodyParser.json())
app.use(
  expressJwt({
    secret: process.env.JWT_SECRET,
    credentialsRequired: false,
    isRevoked: async (req, payload, done) => {
      if (!payload.tokenType) {
        done(null, true)
        return
      }

      switch (payload.tokenType) {
        case "PERMANENT":
          try {
            const tokenFound = await PermanentToken.find({
              where: { id: payload.tokenId },
            })

            if (tokenFound && tokenFound.userId === payload.userId) {
              tokenFound.update({ lastUsed: new Date() })
              done(null, false)
            } else {
              done(null, true)
            }
          } catch (e) {
            done("Internal error")
            console.log(e)
          }
          break

        case "TEMPORARY":
        case "DEVICE_ACCESS":
        case "MANAGE_PERMANENT_TOKENS":
        case "DELETE_USER":
        case "CHANGE_AUTHENTICATION":
        case "CHANGE_EMAIL":
          done(null, false)
          break

        default:
          done(null, true)
          break
      }
    },
  })
)

// handle the errors thrown by expressJwt
app.use((err, req, res, next) => {
  if (err.code === "invalid_token" || err.code === "revoked_token") {
    res.status(401).send({
      data: null,
      errors: [
        { message: "The token is invalid, expired, revoked or malformed" },
      ],
    })
  }
})

// TODO: replace with real free usage quota
const FREE_USAGE_QUOTA = 100 * 1000

app.set("trust proxy", 1)
// Check if usage threshold was exceeded
app.use("/graphql", async (req, res, next) => {
  // TODO: implement anti-DDOS here
  if (await isIpBlocked(req.ip)) {
    res.send(
      JSON.stringify({
        data: null,
        errors: [
          {
            message: "Your ip exceeded the rate limit",
            path: [],
            locations: [],
          },
        ],
      })
    )
    return
  } else {
    // TODO: handle errors
    increaseIpAccessCount(req.ip)
  }

  if (!req.user) next()
  else if (req.user.tokenType === "DEVICE_ACCESS") {
    if (await isDeviceBlocked(req.user.deviceId)) {
      res.send(
        JSON.stringify({
          data: null,
          errors: [
            {
              message: "Your device exceeded the rate limit",
              path: [],
              locations: [],
            },
          ],
        })
      )
      return
    } else {
      // TODO: handle errors
      increaseDeviceAccessCount(req.user.deviceId)
      next()
    }
  } else {
    const userFound = await User.find({ where: { id: req.user.userId } })
    if (await isUserBlocked(userFound.id)) {
      res.send(
        JSON.stringify({
          data: null,
          errors: [
            {
              message: "Your user exceeded the rate limit",
              path: [],
              locations: [],
            },
          ],
        })
      )
      return
    } else {
      // TODO: handle errors
      increaseUserAccessCount(userFound.id)
    }

    if (!userFound) {
      res.send(
        JSON.stringify({
          data: null,
          errors: [
            {
              message: "This user doesn't exist anymore",
              path: [],
              locations: [],
            },
          ],
        })
      )
      return
    } else if (
      userFound.paymentPlan === "FREE" &&
      userFound.monthUsage > FREE_USAGE_QUOTA
    ) {
      req.user.tokenType = "SWITCH_TO_PAYING"
    } else if (
      (userFound.paymentPlan === "INDIVIDUAL" ||
        userFound.paymentPlan === "BUSINESS") &&
      userFound.usageCap &&
      userFound.monthUsage > userFound.usageCap
    ) {
      req.user.tokenType = "CHANGE_USAGE_CAP"
    }

    next()
  }
})

app.use(
  "/graphql",
  graphqlExpress(req => {
    const dataLoaders = createDataLoaders()
    return {
      schema,
      context: {
        auth: req.user,
        billingUpdater: req.user
          ? GenerateUserBillingBatcher(dataLoaders, req.user)
          : undefined,
        dataLoaders,
      },
      validationRules: [depthLimit(10)],
      tracing: process.env.NODE_ENV === "development",
    }
  })
)
app.get(
  "/playground",
  expressPlayground({
    endpoint: "/graphql",
    subscriptionsEndpoint: WEBSOCKET_URL,
  })
)

app.post("/webPushSubscribe", async (req, res) => {
  if (req.user) {
    const notificationSubscription = req.body

    const oldSubscription = await WebPushNotification.find({
      where: { endpoint: notificationSubscription.endpoint },
    })

    const newSubscription = {
      endpoint: notificationSubscription.endpoint,
      expirationTime: notificationSubscription.expirationTime,
      p256dh: notificationSubscription.keys.p256dh,
      auth: notificationSubscription.keys.auth,
      userId: req.user.userId,
    }

    if (!oldSubscription) {
      await WebPushNotification.create(newSubscription)
    } else {
      oldSubscription.update(newSubscription)
    }

    res.send("ok")
  } else {
    res.status(401).send("Missing valid authentication token")
  }
})

AWS.config.update({ region: "eu-west-1" })
const s3 = new AWS.S3()

app.post("/fileupload", Busboy(), async (req, res) => {
  if (req.user) {
    req.pipe(req.busboy)

    req.busboy.on("error", err => {
      console.log(err)
    })

    req.busboy.on("file", async (fieldname, file, filename) => {
      const extension = path.extname(filename)
      const newObject = await pipeStreamToS3(
        s3,
        process.env.BUCKET_NAME,
        file,
        extension,
        req.user.userId
      )
      res.send(newObject.key)
    })
  } else {
    res.status(401).send("Missing valid authentication token")
  }
})

app.get("/file/:file", async (req, res) => {
  if (req.user) {
    const getParams = {
      Bucket: process.env.BUCKET_NAME,
      Key: req.params.file,
    }

    const objectOwner = await getObjectOwner(s3, getParams)

    if (objectOwner === req.user.userId) {
      s3.getObject(getParams)
        .createReadStream()
        .pipe(res)
    } else {
      res.status(401).send("You are not authorized to read this file")
    }
  } else {
    res.status(401).send("Missing valid authentication token")
  }
})

app.get("/fileuploadtest", (req, res) => {
  res.send(`<html><head></head><body>
  <script>
   function send(){
    var formData = new FormData();
    var fileField = document.querySelector("input[type='file']");

    formData.append('file', fileField.files[0]);
    
    fetch('http://localhost:3000/fileupload', {
      method: 'POST',
      body: formData,
      "headers": {
        "authorization": "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJleHAiOjE1MjU0NDQxODAsInVzZXJJZCI6ImFjZjJlODY2LWY4NTktNDAwMi05ZDdhLWRlY2JkMzE2NTFhZCIsImFjY2Vzc0xldmVsIjoiT1dORVIiLCJ0b2tlblR5cGUiOiJURU1QT1JBUlkifQ.gdcPD0i6XHe-5_bLWIcQzAQNecJ4st-dZ-1wiYwNXrkYTNa1pjw--ub4v3WRNXzN97ylQSH869Rfd3KnNZ3X5A"
      }
    })
    .catch(error => console.error('Error:', error))
    .then(response=>response.text().then(console.log))
   }
   
   </script>
     <input type="file" name="file"><br />
     <button onclick="send()">SEND</button>
   
 </body></html>`)
})

app.get("/verifyEmail/:verificationToken", async (req, res) => {
  const { verificationToken } = req.params
  try {
    const decodedToken = jwt.decode(
      verificationToken,
      process.env.JWT_SECRET,
      false,
      "HS512"
    )

    if (decodedToken.tokenType !== "EMAIL_VERIFICATION") {
      res.send("Malformed token")
    } else {
      const foundUser = await User.find({ where: { id: decodedToken.userId } })
      const sameEmailUserFound = await User.find({
        where: { email: decodedToken.email, emailIsVerified: true },
      })

      if (!foundUser) {
        res.send("User doesn't exist anymore")
      } else if (sameEmailUserFound) {
        res.send("A user with this email already exists")
      } else {
        foundUser.update({ emailIsVerified: true, email: decodedToken.email })

        pubsub.publish("userUpdated", {
          userUpdated: foundUser.dataValues,
          userId: foundUser.id,
        })
        res.redirect("https://aurora.igloo.ooo")
      }
    }
  } catch (e) {
    console.log(e)
    res.send("Failed verification")
  }
})

app.get("/", (req, res) => {
  res.send(
    "This is the backend for the igloo service, maybe you were looking for aurora.igloo.ooo"
  )
})

export default app
