import jwt from "jwt-simple"
import moment from "moment"
import chalk from "chalk"
import { appendFile } from "fs"
import uuid from "uuid"
import stackTrace from "stack-trace"
import OTP from "otp.js"
import fortuna from "javascript-fortuna"
import { withFilter } from "graphql-subscriptions"
import winston from "winston"
import AWS from "aws-sdk"
import UpdateBatcher from "update-batcher"
import { isNullOrUndefined } from "util"

require("dotenv").config()

/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

const ses = new AWS.SES({ region: "eu-west-1" })

const GA = OTP.googleAuthenticator
const JWT_EXPIRE_DAYS = 7

fortuna.init()

const authenticated = (
  context,
  callback,
  acceptedTokenTypes = ["TEMPORARY", "PERMANENT"]
) =>
  context.auth && acceptedTokenTypes.indexOf(context.auth.tokenType) > -1
    ? callback
    : (resolve, reject) => {
        if (!context.auth) {
          reject(
            "You are not authenticated. Use `logIn` to obtain an authentication token"
          )
        } else if (context.auth.tokenType === "SWITCH_TO_PAYING") {
          reject("You exceeded the free usage quota")
        } else if (context.auth.tokenType === "CHANGE_USAGE_CAP") {
          reject("You exceeded the usage cap that you set")
        } else reject("This token doesn't have the required authorizations")
      }

const generateAuthenticationToken = (userId, JWT_SECRET) =>
  jwt.encode(
    {
      exp: moment()
        .utc()
        .add({ days: JWT_EXPIRE_DAYS })
        .unix(),
      userId,
      accessLevel: "OWNER",
      tokenType: "TEMPORARY",
    },
    JWT_SECRET,
    "HS512"
  )

const generatePermanentAuthenticationToken = (
  userId,
  tokenId,
  accessLevel,
  JWT_SECRET
) =>
  jwt.encode(
    {
      userId,
      tokenId,
      accessLevel: accessLevel || "DEVICE",
      tokenType: "PERMANENT",
    },
    JWT_SECRET,
    "HS512"
  )

const generatePasswordRecoveryToken = (userId, JWT_SECRET) =>
  jwt.encode(
    {
      exp: moment()
        .utc()
        .add({ hours: 1 })
        .unix(),
      userId,
      accessLevel: "OWNER",
      tokenType: "PASSWORD_RECOVERY",
    },
    JWT_SECRET,
    "HS512"
  )

const getPropsIfDefined = (args, props) => {
  const propObject = {}
  for (let i = 0; i < props.length; i += 1) {
    if (args[props[i]] !== undefined && args[props[i]] !== null) {
      propObject[props[i]] = args[props[i]]
    }
  }

  return propObject
}

const MUTATION_COST = 2
// generic resolver for CreateXValue mutations
const CreateGenericValue = (
  User,
  Device,
  Environment,
  Model,
  ModelName,
  ValueModels,
  pubsub,
  argsChecks = (args, reject) => true
) => (root, args, context) =>
  authorized(
    args.deviceId,
    context,
    Device,
    User,
    2,
    async (resolve, reject, deviceFound, [_, environmentFound], userFound) => {
      if (!argsChecks(args, reject)) {
        return
      }

      if (args.name === "") {
        reject("name cannot be an empty string")
        return
      } else if (args.valueDetails === "") {
        reject("valueDetails cannot be an empty string, pass null instead")
        return
      }

      async function calculateIndex() {
        const valuesCountPromises = ValueModels.map(
          async model =>
            await model.max("index", { where: { deviceId: args.deviceId } })
        )
        const valuesCount = await Promise.all(valuesCountPromises)

        const maxIndex = valuesCount.reduce(
          (acc, curr) => Math.max(acc, !isNaN(curr) ? curr : 0),
          0
        )
        return maxIndex + 1
      }

      const index =
        args.index !== null && args.index !== undefined
          ? args.index
          : await calculateIndex()

      const newValue = await Model.create({
        ...args,
        environmentId: environmentFound.id,
        deviceId: deviceFound.id,
        tileSize: args.tileSize || "NORMAL",
        visibility: isNullOrUndefined(args.visibility)
          ? "VISIBLE"
          : args.visibility,
        index,
      })

      await environmentFound[`add${ModelName}`](newValue)

      const resolveObj = {
        ...newValue.dataValues,
        user: {
          id: newValue.userId,
        },
        device: {
          id: newValue.deviceId,
        },
      }

      resolve(resolveObj)

      Environment.update(
        { updatedAt: newValue.createdAt },
        { where: { id: environmentFound.id } }
      )
      Device.update(
        { updatedAt: newValue.createdAt },
        { where: { id: args.deviceId } }
      )

      pubsub.publish("valueCreated", {
        valueCreated: resolveObj,
        userIds: await instanceToSharedIds(environmentFound),
      })
      context.billingUpdater.update(MUTATION_COST)
    },
    deviceToParent(Environment)
  )

// logs messages colorized by priority, both to console and to `logs` file
function log(message, priority = 1) {
  // choose color
  let colorize =
    priority === 2 ? chalk.bgRedBright : priority === 1 ? chalk.green : id => id

  console.log(colorize(message))

  if (priority > 0) {
    appendFile(
      "./logs",
      message + "\n",
      "utf-8",
      err => err && console.log(err)
    )
  }
}

// returns promise that gracefully logs errors without crashing
function logErrorsPromise(_, _a, callback) {
  return new Promise(async (resolve, reject) => {
    try {
      return await callback(resolve, reject)
    } catch (e) {
      const uniqueErrorCode = uuid() // date in milliseconds

      const trace = stackTrace.parse(e)
      const fileName = trace[0].getFileName().substr(process.cwd().length)
      const lineNumber = trace[0].getLineNumber()
      const context = `${fileName}:${lineNumber}`
      const date = new Date().toISOString()

      const lineToPrint = `[${context}](${date}) Error ${uniqueErrorCode}\n${e}`
      log(lineToPrint, 2)

      const traceString = JSON.stringify(trace, null, 2)
      log(traceString)

      // sends error message as graphql response
      reject(
        new Error(
          `An internal error occured, please contact us. The error code is ${uniqueErrorCode}`
        )
      )
    }
  })
}

const genericValueMutation = (
  childModel,
  __resolveType,
  pubsub,
  User,
  Device,
  Environment,
  checkArgs = (args, valueFound, reject) => true
) => (root, args, context) =>
  authorized(
    args.id,
    context,
    childModel,
    User,
    2,
    async (resolve, reject, valueFound, [_, environmentFound]) => {
      if (!checkArgs(args, valueFound, reject)) return
      if (args.value === null) {
        reject("value cannot be null")
        return
      } else if (args.name === null || args.name === "") {
        reject("name cannot be null or an empty string")
        return
      } else if (Object.keys(args).length === 1) {
        reject("You cannot make a mutation with only the id field")
        return
      } else if (args.valueDetails === "") {
        reject("valueDetails cannot be an empty string, pass null instead")
        return
      }

      const newValue = await valueFound.update(args)
      const resolveObj = {
        ...newValue.dataValues,
        owner: {
          id: newValue.dataValues.userId,
        },
        device: {
          id: newValue.dataValues.deviceId,
        },
      }
      resolve(resolveObj)

      Environment.update(
        { updatedAt: newValue.updatedAt },
        { where: { id: environmentFound.id } }
      )
      Device.update(
        { updatedAt: newValue.updatedAt },
        { where: { id: valueFound.deviceId } }
      )

      pubsub.publish("valueUpdated", {
        valueUpdated: { ...resolveObj, __resolveType },
        userIds: await instanceToSharedIds(environmentFound),
      })
      context.billingUpdater.update(MUTATION_COST)
    },
    valueToParent(Environment)
  )

const create2FSecret = user => {
  const allowedChars = "QWERTYUIOPASDFGHJKLZXCVBNM234567"
  let secret = ""
  for (let i = 0; i < 12; i += 1) {
    const randomNumber = Math.floor(fortuna.random() * allowedChars.length)
    secret += allowedChars[randomNumber]
  }
  secret = GA.encode(secret)
  return { secret, qrCode: GA.qrCode(user, "igloo", secret) }
}

const check2FCode = (code, secret) => {
  try {
    const { delta } = GA.verify(code, secret)
    return Math.abs(delta) < 3
  } catch (e) {
    return false
  }
}

const subscriptionFilterOnlyMine = (subscriptionName, pubsub) => ({
  subscribe: (root, args, context, info) => {
    if (context.auth) {
      const myUserId = context.auth.userId
      return withFilter(
        () => pubsub.asyncIterator(subscriptionName),
        payload => payload.userId === myUserId
      )(root, args, context, info)
    }
    throw new Error("No authorization token")
  },
})

const subscriptionFilterOwnedOrShared = (subscriptionName, pubsub) => ({
  subscribe: (root, args, context, info) => {
    if (context.auth) {
      const myUserId = context.auth.userId
      return withFilter(
        () => pubsub.asyncIterator(subscriptionName),
        payload => payload.userIds.indexOf(myUserId) !== -1
      )(root, args, context, info)
    }
    throw new Error("No authorization token")
  },
})

// races promises returning the first resolve or all the rejects if none resolves
const firstResolve = promises =>
  new Promise((resolve, reject) => {
    const errors = []
    let count = 0
    let resolved = false
    promises.forEach((promise, idx) => {
      promise
        .then(found => {
          if (!resolved) {
            resolved = true
            resolve(found)
          }
        })
        /* istanbul ignore next */

        .catch(err => {
          errors[idx] = err
          count += 1
          if (count === promises.length) {
            reject(errors)
          }
        })
    })
  })

// !! doesn't check if the user has the authorizations needed
const findAllValues = (
  {
    BooleanValue,
    FloatValue,
    StringValue,
    PlotValue,
    CategoryPlotValue,
    MapValue,
  },
  query
) => {
  const booleanValues = BooleanValue.findAll(query)
  const floatValues = FloatValue.findAll(query)
  const stringValues = StringValue.findAll(query)
  const plotValues = PlotValue.findAll(query)
  const categoryPlotValues = CategoryPlotValue.findAll(query)
  const mapValues = MapValue.findAll(query)

  return Promise.all([
    booleanValues,
    floatValues,
    stringValues,
    plotValues,
    categoryPlotValues,
    mapValues,
  ]).then(
    ([
      booleanValues,
      floatValues,
      stringValues,
      plotValues,
      categoryPlotValues,
      mapValues,
    ]) => [
      ...booleanValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "BooleanValue",
      })),
      ...floatValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "FloatValue",
      })),
      ...stringValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "StringValue",
      })),
      ...plotValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "PlotValue",
      })),
      ...categoryPlotValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "CategoryPlotValue",
      })),
      ...mapValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "MapValue",
      })),
    ]
  )
}

// try refactoring this with firstResolve
const findValue = (
  {
    BooleanValue,
    FloatValue,
    StringValue,
    PlotValue,
    CategoryPlotValue,
    MapValue,
  },
  Device,
  Environment,
  query,
  userFound
) => {
  const booleanValue = BooleanValue.find(query).then(
    value =>
      value
        ? {
            ...value.dataValues,
            user: { id: value.dataValues.userId },
            device: { id: value.dataValues.deviceId },
            __resolveType: "BooleanValue",
          }
        : value
  )
  const floatValue = FloatValue.find(query).then(
    value =>
      value
        ? {
            ...value.dataValues,
            user: { id: value.dataValues.userId },
            device: { id: value.dataValues.deviceId },
            __resolveType: "FloatValue",
          }
        : value
  )
  const stringValue = StringValue.find(query).then(
    value =>
      value
        ? {
            ...value.dataValues,
            user: { id: value.dataValues.userId },
            device: { id: value.dataValues.deviceId },
            __resolveType: "StringValue",
          }
        : value
  )

  const mapValue = MapValue.find(query).then(
    value =>
      value
        ? {
            ...value.dataValues,
            user: { id: value.dataValues.userId },
            device: { id: value.dataValues.deviceId },
            __resolveType: "MapValue",
          }
        : value
  )

  const plotValue = PlotValue.find(query).then(
    value =>
      value
        ? {
            ...value.dataValues,
            user: { id: value.dataValues.userId },
            device: { id: value.dataValues.deviceId },
            __resolveType: "PlotValue",
          }
        : value
  )

  const categoryPlotValue = CategoryPlotValue.find(query).then(
    value =>
      value
        ? {
            ...value.dataValues,
            user: { id: value.dataValues.userId },
            device: { id: value.dataValues.deviceId },
            __resolveType: "CategoryPlotValue",
          }
        : value
  )

  return Promise.all([
    booleanValue,
    floatValue,
    stringValue,
    mapValue,
    plotValue,
    categoryPlotValue,
  ])
    .then(values => values.reduce((acc, val) => val || acc, null))
    .then(async value => {
      if (!value) throw new Error("The requested resource does not exist")
      else {
        const environmentFound = await Environment.find({
          where: { id: value.environmentId },
        })

        if ((await authorizationLevel(environmentFound, userFound)) < 1) {
          throw new Error("You are not allowed to perform this operation")
        } else return value
      }
    })
}

const socketToDeviceMap = {}

const sendVerificationEmail = (email, userId) => {
  // TODO: use different jwt secrets?
  const verificationToken = jwt.encode(
    {
      userId,
      email,
      tokenType: "EMAIL_VERIFICATION",
    },
    process.env.JWT_SECRET,
    "HS512"
  )

  const GRAPHQL_PORT = process.env.PORT || 3000
  const serverLink =
    process.env.NODE_ENV === "production"
      ? `https://${process.env.BASE_URL}/verifyEmail/`
      : `http://localhost:${GRAPHQL_PORT}/verifyEmail/`
  const emailVerificationLink = serverLink + verificationToken

  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo Cloud' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `Verify your account clicking this link: <a href="${emailVerificationLink}">VERIFY</a>`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `Verify your account visiting this link: ${emailVerificationLink}`,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "Verify your account",
        },
      },
    },
    console.log
  )
}

const sendPasswordRecoveryEmail = (email, userId) => {
  // TODO: use different jwt secrets?
  const recoveryToken = generatePasswordRecoveryToken(
    userId,
    process.env.JWT_SECRET
  )

  // TODO: update this with the real link
  const emailRecoverylink = `https://igloo.ooo/recovery?token=${recoveryToken}`

  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo Cloud' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `Change your password clicking this link: <a href="${emailRecoverylink}">Recover password</a>`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `Change your password at this link: ${emailRecoverylink}`,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "Recover your password",
        },
      },
    },
    console.log
  )
}

const sendPasswordUpdatedEmail = email => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo Cloud' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data:
              "Your password has been changed, if it was you that changed it you can ignore this email",
          },
          Text: {
            Charset: "UTF-8",
            Data:
              "Your password has been changed, if it was you that changed it you can ignore this email",
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "Password has been changed",
        },
      },
    },
    console.log
  )
}

const sendTokenCreatedEmail = email => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo Cloud' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data:
              "A new permanent token has been created, if it was you that created it you can ignore this email",
          },
          Text: {
            Charset: "UTF-8",
            Data:
              "A new permanent token has been created, if it was you that created it you can ignore this email",
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "A new permanent token has been created",
        },
      },
    },
    console.log
  )
}
const sendEnvironmentSharedEmail = (email, userName, environmentName) => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo Cloud' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `${userName} has shared the environment ${environmentName} with you. <a href="igloo.ooo">Check it out now</a>`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `${userName} has shared the environment ${environmentName} with you. Check it out on igloo.ooo`,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "A environment was shared with you",
        },
      },
    },
    console.log
  )
}

async function authorizationLevel(instance, userFound) {
  const isOwner = await userFound.hasOwnEnvironment(instance)
  const isAdmin = await userFound.hasAdminEnvironment(instance)
  const isEditor = await userFound.hasEditorEnvironment(instance)
  const isSpectator = await userFound.hasSpectatorEnvironment(instance)

  if (isOwner) return 4
  else if (isAdmin) return 3
  else if (isEditor) return 2
  else if (isSpectator) return 1
  return 0
}

function authorized(
  id,
  context,
  Model,
  User,
  authorizationRequired,
  callback,
  childToParent,
  acceptedTokenTypes
) {
  return authenticated(
    context,
    async (resolve, reject) => {
      const found = await Model.find({ where: { id } })

      if (!found) {
        reject("The requested resource does not exist")
      } else {
        const parent = await childToParent(found)
        const userFound = await User.find({
          where: { id: context.auth.userId },
        })

        if (
          (await authorizationLevel(parent, userFound)) < authorizationRequired
        ) {
          /* istanbul ignore next */
          reject("You are not allowed to perform this operation")
        } else {
          return callback(resolve, reject, found, [found, parent], userFound)
        }
      }
    },
    acceptedTokenTypes
  )
}

const authorizedRetrieveScalarProp = (
  Model,
  User,
  prop,
  childToParent,
  acceptedTokenTypes
) => (root, args, context) =>
  authorized(
    root.id,
    context,
    Model,
    User,
    1,
    async (resolve, reject, resourceFound) => {
      resolve(resourceFound[prop])
    },
    childToParent,
    acceptedTokenTypes
  )

const authorizedScalarPropsResolvers = (
  Model,
  User,
  props,
  childToParent,
  acceptedTokenTypes
) =>
  props.reduce((acc, prop) => {
    acc[prop] = authorizedRetrieveScalarProp(
      Model,
      User,
      prop,
      childToParent,
      acceptedTokenTypes
    )
    return acc
  }, {})

const deviceToParent = Environment => async deviceFound => {
  const environmentFound = await Environment.find({
    where: { id: deviceFound.environmentId },
  })

  return environmentFound
}

const valueToParent = Environment => async valueFound => {
  const environmentFound = await Environment.find({
    where: { id: valueFound.environmentId },
  })

  return environmentFound
}

const authorizedValue = (
  id,
  context,
  Values,
  User,
  authorizationRequired,
  callbackFunc,
  Device,
  Environment
) =>
  authenticated(context, async (resolve, reject) => {
    const NOT_ALLOWED = "You are not allowed to perform this operation"
    const NOT_EXIST = "The requested resource does not exist"
    const models = Object.values(Values)

    const userFound = await User.find({ where: { id: context.auth.userId } })

    const findPromises = models.map(async Model => {
      const resourceFound = await Model.find({
        where: { id },
      })

      if (!resourceFound) {
        throw new Error(NOT_EXIST)
      } else {
        const environmentFound = await Environment.find({
          where: { id: resourceFound.environmentId },
        })

        if (
          (await authorizationLevel(environmentFound, userFound)) <
          authorizationRequired
        ) {
          throw new Error(NOT_ALLOWED)
        } else {
          resourceFound.Model = Model
          return [resourceFound, environmentFound]
        }
      }
    })
    // race all the models to find the looked for id, if a value is found
    // it is returned otherwise the correct error is returned
    const resourcesFound = await firstResolve(findPromises).catch(e => {
      // choose the correct error, because normally most models
      // will reject with NOT_EXIST, simply because the value
      // looked for is of another type

      reject(
        e.reduce(
          (acc, val) =>
            acc === NOT_ALLOWED || val === NOT_ALLOWED
              ? NOT_ALLOWED
              : NOT_EXIST,
          NOT_EXIST
        )
      )
    })

    return callbackFunc(
      resolve,
      reject,
      resourcesFound[0],
      resourcesFound,
      userFound
    )
  })

const instanceToRole = async (instance, userFound) => {
  const roleLevel = await authorizationLevel(instance, userFound)

  switch (roleLevel) {
    case 4:
      return "OWNER"
    case 3:
      return "ADMIN"
    case 2:
      return "EDITOR"
    case 1:
      return "SPECTATOR"
    case 0:
      return null
  }
}

const instanceToSharedIds = async instance => {
  const owner = await instance.getOwner()
  const admins = await instance.getAdmin()
  const editors = await instance.getEditor()
  const spectators = await instance.getSpectator()

  return [owner, ...admins, ...editors, ...spectators].map(user => user.id)
}

const inheritAuthorized = (
  ownId,
  ownModel,
  User,
  ownIstanceToParentId,
  context,
  parentModel,
  authorizationRequired,
  callback,
  childToParent,
  acceptedTokenTypes
) => async (resolve, reject) => {
  const entityFound = await ownModel.find({
    where: { id: ownId },
  })

  if (!entityFound) {
    reject("The requested resource does not exist")
  } else {
    return authorized(
      ownIstanceToParentId(entityFound),
      context,
      parentModel,
      User,
      authorizationRequired,
      (resolve, reject, parentFound, allParents) =>
        callback(resolve, reject, entityFound, parentFound, allParents),
      childToParent,
      acceptedTokenTypes
    )(resolve, reject)
  }
}

const inheritAuthorizedRetrieveScalarProp = (
  Model,
  User,
  prop,
  ownIstanceToParentId,
  parentModel,
  childToParent,
  acceptedTokenTypes
) => (root, args, context) =>
  inheritAuthorized(
    root.id,
    Model,
    User,
    ownIstanceToParentId,
    context,
    parentModel,
    1,
    (resolve, reject, resourceFound) => resolve(resourceFound[prop]),
    childToParent,
    acceptedTokenTypes
  )

const inheritAuthorizedScalarPropsResolvers = (
  Model,
  User,
  props,
  ownIstanceToParentId,
  parentModel,
  childToParent,
  acceptedTokenTypes
) =>
  props.reduce((acc, prop) => {
    acc[prop] = inheritAuthorizedRetrieveScalarProp(
      Model,
      User,
      prop,
      ownIstanceToParentId,
      parentModel,
      childToParent,
      acceptedTokenTypes
    )
    return acc
  }, {})

async function getAll(Model, User, userId, includesList = []) {
  // for some reason sequelize needs the includes to be different instances,
  // so we clone every include object
  function deepCloneIncludes(includes) {
    const clonedList = []

    for (let i = 0; i < includes.length; i++) {
      const clonedInclude = {}
      for (const key in includes[i]) {
        if (includes[i].hasOwnProperty(key)) {
          if (key !== "include") {
            clonedInclude[key] = includes[i][key]
          } else {
            clonedInclude[key] = deepCloneIncludes(includes[i][key])
          }
        }
      }
      clonedList.push(clonedInclude)
    }

    return clonedList
  }

  const allAccessibles = await User.find({
    where: { id: userId },
    attributes: ["id"],
    include: [
      {
        model: Model,
        as: Model.Owner,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
      {
        model: Model,
        as: Model.Admins,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
      {
        model: Model,
        as: Model.Editors,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
      {
        model: Model,
        as: Model.Spectators,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
    ],
  })

  const allFlattened = [
    ...allAccessibles[Model.Owner],
    ...allAccessibles[Model.Admins],
    ...allAccessibles[Model.Editors],
    ...allAccessibles[Model.Spectators],
  ]

  return allFlattened
}

const randomChoice = (...args) => {
  let chooseAmong = args
  if (args.length === 1) chooseAmong = args[0]

  const randomIndex = Math.floor(Math.random() * chooseAmong.length)

  return chooseAmong[randomIndex]
}

const randomEnvironmentAvatar = () =>
  randomChoice(["NORTHERN_LIGHTS", "DENALI", "FOX", "PUFFIN", "TREETOPS"])

const randomUserIconColor = () => randomChoice(["blue", "red", "green"])

const updateUserBilling = (User, auth) => async bill => {
  const userFound = await User.find({ where: { id: auth.userId } })

  // TODO: handle this failure gracefully
  if (!userFound) {
    throw new Error("User doesn't exist. Use `` to create one")
  } else {
    const newUser = await userFound.increment("monthUsage", { by: bill })
    return newUser.monthUsage
  }
}

const GenerateUserBillingBatcher = (User, auth) =>
  new UpdateBatcher(updateUserBilling(User, auth))

// an environment is it's own parent
const environmentToParent = x => x

const runInParallel = async (...funcs) => await Promise.all(funcs.map(f => f()))

module.exports = {
  authenticated,
  generateAuthenticationToken,
  CreateGenericValue,
  getPropsIfDefined,
  genericValueMutation,
  create2FSecret,
  check2FCode,
  logErrorsPromise,
  log,
  subscriptionFilterOnlyMine,
  findAllValues,
  findValue,
  generatePermanentAuthenticationToken,
  socketToDeviceMap,
  sendVerificationEmail,
  generatePasswordRecoveryToken,
  sendPasswordRecoveryEmail,
  sendPasswordUpdatedEmail,
  sendTokenCreatedEmail,
  sendEnvironmentSharedEmail,
  authorizationLevel,
  authorized,
  authorizedScalarPropsResolvers,
  deviceToParent,
  valueToParent,
  authorizedValue,
  firstResolve,
  instanceToRole,
  instanceToSharedIds,
  subscriptionFilterOwnedOrShared,
  inheritAuthorized,
  inheritAuthorizedScalarPropsResolvers,
  getAll,
  randomChoice,
  randomEnvironmentAvatar,
  randomUserIconColor,
  updateUserBilling,
  GenerateUserBillingBatcher,
  environmentToParent,
  runInParallel,
}
