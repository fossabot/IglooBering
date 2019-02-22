const AdminBro = require("admin-bro")
const AdminBroExpressjs = require("admin-bro-expressjs")
const AdminBroSequelize = require("admin-bro-sequelizejs")
const DashboardPage = require("./dashboard")
const express = require("express")
const jwt = require("jwt-simple")
import {
  User,
  Environment,
  Device,
  BooleanValue,
  CategoryPlotValue,
  EnvironmentAdmin,
  EnvironmentEditor,
  EnvironmentSpectator,
  FloatValue,
  MapValue,
  Notification,
  PendingEnvironmentShare,
  PendingOwnerChange,
  PermanentToken,
  PlotNode,
  PlotValue,
  CategoryPlotNode,
  StringValue,
  WebPushSubscription,
} from "./postgresql/models"

module.exports = app => {
  // We have to tell AdminBro that we will manage mongoose resources with it
  AdminBro.registerAdapter(AdminBroSequelize)

  const databaseParent = {
    name: "Database",
  }

  // Pass all configuration settings to AdminBro
  const adminBro = new AdminBro({
    dashboard: DashboardPage,
    rootPath: "/admin",
    // loginPath:
    //   "https://aurora.igloo.ooo/login?to=https://igloo-production.herokuapp.com/admin",
    resources: [
      {
        resource: User,
        options: {
          listProperties: [
            "id",
            "email",
            "name",
            "emailIsVerified",
            "paymentPlan",
            "monthUsage",
            "createdAt",
            "updatedAt",
          ],
          showProperties: [
            "id",
            "email",
            "name",
            "emailIsVerified",
            "paymentPlan",
            "monthUsage",
            "createdAt",
            "updatedAt",
          ],
          editProperties: ["name", "paymentPlan", "monthUsage"],
          filterProperties: [
            "id",
            "email",
            "name",
            "emailIsVerified",
            "paymentPlan",
            "monthUsage",
            "createdAt",
            "updatedAt",
          ],
          properties: { emailIsVerified: { type: " boolean" } },
          parent: databaseParent,
          name: "Users",
        },
      },
      {
        resource: Environment,
        options: {
          listProperties: ["id", "name", "picture", "muted"],
          filterProperties: ["id", "name", "picture", "muted"],
          showProperties: ["id", "name", "picture", "muted"],
          editProperties: ["name", "picture", "muted"],
          properties: { muted: { type: " boolean" } },
          parent: databaseParent,
          name: "Environments",
        },
      },
      {
        resource: Device,
        options: {
          listProperties: [
            "id",
            "name",
            "starred",
            "deviceType",
            "firmware",
            "online",
            "batteryStatus",
            "batteryCharging",
            "signalStatus",
            "muted",
          ],
          filterProperties: [
            "id",
            "name",
            "starred",
            "deviceType",
            "firmware",
            "online",
            "batteryStatus",
            "batteryCharging",
            "signalStatus",
            "muted",
          ],
          showProperties: [
            "id",
            "name",
            "starred",
            "deviceType",
            "firmware",
            "online",
            "batteryStatus",
            "batteryCharging",
            "signalStatus",
            "muted",
          ],
          editProperties: [
            "name",
            "deviceType",
            "firmware",
            "online",
            "batteryStatus",
            "batteryCharging",
            "signalStatus",
            "muted",
          ],
          properties: {
            muted: { type: " boolean" },
            batteryCharging: { type: " boolean" },
            online: { type: " boolean" },
          },
          parent: databaseParent,
          name: "Devices",
        },
      },
    ],
    branding: {
      logo:
        "https://raw.githubusercontent.com/IglooCloud/IglooBering/master/IglooLogo.png",
      companyName: "Igloo Bering",
      softwareBrothers: false,
    },
  })

  // let router = express.Router()
  // router.use((req, res, next) => {
  //   if (req.session && req.session.admin) {
  //     req.adminUser = req.session.admin
  //     next()
  //   } else {
  //     res.redirect(adminBro.options.loginPath)
  //   }
  // })

  let router = AdminBroExpressjs.buildAuthenticatedRouter(adminBro, {
    authenticate: (email, password) =>
      password === "sleepingpolarbear"
        ? {
            email,
          }
        : null,
    cookiePassword: "6576rt79668tut6787trtyu7",
  })

  app.use(adminBro.options.rootPath, router)
}
