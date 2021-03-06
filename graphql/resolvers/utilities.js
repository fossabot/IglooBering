/**
 * Various function that can be reused
 * @namespace Utilities
 */

require("dotenv").config()

/* istanbul ignore if */
if (!process.env.JWT_SECRET) {
  throw new Error("Could not load .env")
}

import jwt from "jwt-simple"
import moment from "moment"
import chalk from "chalk"
import { appendFile } from "fs"
import uuid from "uuid"
import stackTrace from "stack-trace"
import OTP from "otp.js"
import fortuna from "javascript-fortuna"
import AWS from "aws-sdk"
import webpush from "web-push"
import { Op } from "sequelize"
import { isNullOrUndefined, isNull } from "util"

webpush.setVapidDetails(
  "http://igloo.witlab.io/",
  process.env.PUBLIC_VAPID_KEY,
  process.env.PRIVATE_VAPID_KEY
)

const ses = new AWS.SES({ region: "eu-west-1" })

const GA = OTP.googleAuthenticator
const JWT_EXPIRE_DAYS = 7

fortuna.init()

/**
 * @callback PromiseExecutor
 * @param {number} resolve - resolve callback for the promise
 * @param {number} reject - resolve callback for the promise
 */

/**
 * @typedef SequelizeModel
 */

/**
 * @typedef pubsub
 */

/**
 * Checks that the request is authenticated with the correct authorizations and executes the callback
 * @param {Object} context
 * @param {Object} context.auth - the decoded jwt token used to authenticate
 * @param {PromiseExecutor} callback - callback to call if authorized
 * @param {string[]} [acceptedTokenTypes=["TEMPORARY", "PERMANENT"]] - List of the authorized token types
 * @returns {PromiseExecutor} function accepting (resolve, reject) arguments running the callback if authorized otherwise rejects
 *
 * @memberof Utilities
 */
export function authenticated(
  context,
  callback,
  acceptedTokenTypes = ["TEMPORARY", "PERMANENT"]
) {
  return context.auth && acceptedTokenTypes.indexOf(context.auth.tokenType) > -1
    ? callback
    : (resolve, reject) => {
        if (!context.auth) {
          reject(
            "You are not authenticated. Use `logIn` to obtain an authentication token"
          )
        } else if (context.auth.tokenType === "SWITCH_TO_PAYING") {
          reject("You exceeded the free usage quota")
        } else reject("This token doesn't have the required authorizations")
      }
}
/**
 * Creates an authentication token expiring after JWT_EXPIRE (global constant) days
 * @param {string} userId - id of the user to be authenticated as
 * @param {string} JWT_SECRET - encryption key for the JWT
 * @returns {string} authentication token
 *
 * @memberof Utilities
 */
export function generateAuthenticationToken(userId, JWT_SECRET) {
  return jwt.encode(
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
}
/**
 * Creates a permanent authentication token
 * @param {string} userId - id of the user to be authenticated as
 * @param {string} tokenId - id of the token on the database (used for checking whether the token was revoked)
 * @param {string} accessLevel - TODO
 * @param {string} JWT_SECRET - encryption key for the JWT
 * @returns {string} authentication token
 *
 * @memberof Utilities
 */
export function generatePermanentAuthenticationToken(
  userId,
  tokenId,
  accessLevel,
  JWT_SECRET
) {
  return jwt.encode(
    {
      userId,
      tokenId,
      accessLevel: accessLevel || "DEVICE",
      tokenType: "PERMANENT",
    },
    JWT_SECRET,
    "HS512"
  )
}

/**
 * Creates a permanent authentication token bound to a device (instead of a user)
 * @param {string} deviceId - id of the user to be authenticated as
 * @param {string} JWT_SECRET - encryption key for the JWT
 * @returns {string} authentication token
 *
 * @memberof Utilities
 */
export function generateDeviceAuthenticationToken(deviceId, JWT_SECRET) {
  return jwt.encode(
    {
      deviceId,
      tokenType: "DEVICE_ACCESS",
    },
    JWT_SECRET,
    "HS512"
  )
}

/**
 * Creates an object containing the required fields if they have a not null value in the args object
 * @param {Object} args - object to read the fields from
 * @param {string[]} props - fields to read
 * @returns {Object} filtered object
 *
 * @memberof Utilities
 */
export function getPropsIfDefined(args, props) {
  const propObject = {}
  for (let i = 0; i < props.length; i += 1) {
    if (args[props[i]] !== undefined && args[props[i]] !== null) {
      propObject[props[i]] = args[props[i]]
    }
  }

  return propObject
}

const MUTATION_COST = 2

/**
 * Function checking that the passed arguments are correct and rejecting if not
 * @callback argsChecker
 * @param {object} args - object containing the passed arguments
 * @param {function} reject - function to call to reject the GraphQL operation
 *
 * @returns {boolean} true if arguments are correct, false if not
 */

/**
 * Function checking that the passed arguments are correct and rejecting if not
 * @callback GraphQLHandler
 * @param {object} root - object containing the root element (for example in the user.email resolver root contains the id of the user)
 * @param {object} args - arguments passed to the graphql query/mutation
 * @param {object} context - object containing information on the api call, for example the token used to authenticate the request
 *
 * @returns {PromiseExecutor} promise executor resolving the result of the query/mutation or rejecting with an error
 */

const MAX_STORAGE = 250000 // BETA

/**
 * generic resolver for CreateXValue mutations
 * @param {SequelizeModel} User
 * @param {SequelizeModel} Device
 * @param {SequelizeModel} Environment
 * @param {SequelizeModel} Model - Model of the value to create
 * @param {string} ModelName - name of the model used
 * @param {SequelizeModel[]} ValueModels - Models of all the Value types
 * @param {pubsub} pubsub - PubSub manager
 * @param {argsChecker} argsChecks - function checking that the arguments are valid
 *
 * @returns {GraphQLHandler} function handling the mutation
 *
 * @memberof Utilities
 */
export function CreateGenericValue(
  Device,
  Environment,
  Model,
  ValueModels,
  pubsub,
  argsChecks = (args, reject) => true
) {
  return (root, args, context) =>
    producerAuthorized(
      args.deviceId,
      context,
      async (resolve, reject, deviceFound) => {
        if (!argsChecks(args, reject)) {
          return
        } else if (deviceFound.storageUsed >= MAX_STORAGE) {
          reject("Storage space fully used")
          return
        }

        if (args.name === "") {
          reject("name cannot be an empty string")
          return
        } else if (args.private === null) {
          reject("Cannot set private to null")
          return
        } else if (args.hidden === null) {
          reject("Cannot set hidden to null")
          return
        }

        // finds highest used index among values of that device and returns maxIndex + 1
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
          deviceId: deviceFound.id,
          cardSize: args.cardSize || "NORMAL",
          private: args.private || false,
          hidden: args.hidden || false,
          index,
        })

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

        deviceFound.increment({ storageUsed: 1 })
        if (deviceFound.environmentId !== null) {
          Environment.update(
            { updatedAt: newValue.createdAt },
            { where: { id: deviceFound.environmentId } }
          )
        }
        Device.update(
          { updatedAt: newValue.createdAt },
          { where: { id: args.deviceId } }
        )

        pubsub.publish("valueCreated", {
          valueCreated: resolveObj,
          userIds: deviceFound.environmentId
            ? [
                deviceFound.producerId,
                ...(await instanceToSharedIds(
                  await context.dataLoaders.environmentLoaderById.load(
                    deviceFound.environmentId
                  ),
                  context
                )),
              ]
            : [deviceFound.producerId],
          allowedDeviceIds: [deviceFound.id],
        })
      }
    )
}

/**  logs messages colorized by priority, both to console and to `logs` file
 * @param {String} message - message to log
 * @param {Integer} importance - 2=HIGH 1=NORMAL 0=VERBOSE
 *
 * @memberof Utilities
 */
export function log(message, importance = 1) {
  // choose color
  let colorize =
    importance === 2
      ? chalk.bgRedBright
      : importance === 1
      ? chalk.green
      : id => id

  console.log(colorize(message))

  if (importance > 0) {
    appendFile(
      "./logs",
      message + "\n",
      "utf-8",
      err => err && console.log(err)
    )
  }
}

/** catches internal errors in PromiseExecutor handles them gracefully
 * @param {PromiseExecutor} callback
 *
 * @returns {Promise}
 *
 * @memberof Utilities
 */
export function logErrorsPromise(callback) {
  return new Promise(async (resolve, reject) => {
    try {
      return await callback(resolve, reject)
    } catch (e) {
      if (e.parent && e.parent.routine === "string_to_uuid") {
        reject("This ID is not valid")
        return
      }

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

/**
 * generic resolver for xValue mutations
 * @param {String} childLoaderName - key of the dataloader for the value being updated, for example `floatValueLoaderById`
 * @param {String} __resolveType - resolveType to return (for example `FloatValue`)
 * @param {pubsub} pubsub - pubsub carrying the subscriptions
 * @param {SequelizeModel} User
 * @param {SequelizeModel} Device
 * @param {SequelizeModel} Environment
 * @param {argsChecker} argsChecks - function checking that the arguments are valid
 *
 * @returns {GraphQLHandler} function handling the mutation
 *
 * @memberof Utilities
 */
export const genericValueMutation = (
  childLoaderName,
  __resolveType,
  pubsub,
  User,
  Device,
  Environment,
  checkArgs = (args, valueFound, reject) => true
) => (root, args, context) =>
  deviceInheritValueAuthorized(
    args.id,
    context.dataLoaders[childLoaderName],
    context,
    2,
    async (resolve, reject, valueFound, environmentFound) => {
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
      } else if (args.private === null) {
        reject("Cannot set private to null")
        return
      } else if (args.hidden === null) {
        reject("Cannot set hidden to null")
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
        userIds: await instanceToSharedIds(environmentFound, context),
      })
    },
    undefined,
    args.private !== undefined ? 3 : 2
  )

export const create2FSecret = user => {
  const allowedChars = "QWERTYUIOPASDFGHJKLZXCVBNM234567"
  let secret = ""
  for (let i = 0; i < 12; i += 1) {
    const randomNumber = Math.floor(fortuna.random() * allowedChars.length)
    secret += allowedChars[randomNumber]
  }
  secret = GA.encode(secret)
  return { secret, qrCode: GA.qrCode(user, "Igloo", secret) }
}

export const check2FCode = (code, secret) => {
  try {
    const { delta } = GA.verify(code, secret)
    return Math.abs(delta) < 3
  } catch (e) {
    return false
  }
}

/** races promises returning the first resolve or all the rejects if none resolves
 * @param {Promise[]} promises - promises to be raced
 *
 * @returns {Promise} promise returning the first resolve or all the rejects
 */

export const firstResolve = promises =>
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

// FIXME: doesn't check if the user has the authorizations needed
export const findAllValues = (
  {
    BooleanValue,
    FloatValue,
    StringValue,
    FloatSeriesValue,
    CategorySeriesValue,
  },
  query
) => {
  const booleanValues = BooleanValue.findAll(query)
  const floatValues = FloatValue.findAll(query)
  const stringValues = StringValue.findAll(query)
  const floatSeriesValues = FloatSeriesValue.findAll(query)
  const categorySeriesValues = CategorySeriesValue.findAll(query)

  return Promise.all([
    booleanValues,
    floatValues,
    stringValues,
    floatSeriesValues,
    categorySeriesValues,
  ]).then(
    ([
      booleanValues,
      floatValues,
      stringValues,
      floatSeriesValues,
      categorySeriesValues,
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
      ...floatSeriesValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "FloatSeriesValue",
      })),
      ...categorySeriesValues.map(value => ({
        ...value.dataValues,
        owner: { id: value.dataValues.ownerId },
        device: { id: value.dataValues.deviceId },
        __resolveType: "CategorySeriesValue",
      })),
    ]
  )
}

// try refactoring this with firstResolve
export const findValue = (id, context) => {
  const {
    booleanValueLoaderById,
    floatValueLoaderById,
    stringValueLoaderById,
    floatSeriesValueLoaderById,
    categorySeriesValueLoaderById,
    fileValueLoaderById,
    environmentLoaderById,
  } = context.dataLoaders
  const booleanValue = booleanValueLoaderById.load(id)
  const floatValue = floatValueLoaderById.load(id)
  const stringValue = stringValueLoaderById.load(id)
  const floatSeriesValue = floatSeriesValueLoaderById.load(id)
  const categorySeriesValue = categorySeriesValueLoaderById.load(id)
  const fileValue = fileValueLoaderById.load(id)

  return Promise.all([
    booleanValue,
    floatValue,
    stringValue,
    floatSeriesValue,
    categorySeriesValue,
    fileValue,
  ]).then(values => values.reduce((acc, val) => val || acc, null))
}

export const socketToDeviceMap = {}

export const sendVerificationEmail = (email, userId) => {
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
    process.env.NODE_ENV !== "development"
      ? `https://${process.env.BASE_URL}/verifyEmail/`
      : `http://localhost:${GRAPHQL_PORT}/verifyEmail/`
  const emailVerificationLink = serverLink + verificationToken

  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
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

export const sendConfirmationEmail = (email, token, operation) => {
  const operationMap = {
    LOG_IN: {
      htmlBody: `Log into your account clicking this link: <a href="https://aurora.igloo.ooo/login?token=${token}">LOGIN</a>`,
      textBody: `Log into your account visiting this link: https://aurora.igloo.ooo/login?token=${token}`,
      subject: "Login Magic Link",
    },
    DELETE_USER: {
      htmlBody: `You asked to delete your account, click here to confirm: <a href="https://aurora.igloo.ooo/delete-user?token=${token}">LOGIN</a>`,
      textBody: `You asked to delete your account, visit this link to confirm https://aurora.igloo.ooo/delete-user?token=${token}`,
      subject: "Delete your account",
    },
    CHANGE_EMAIL: {
      htmlBody: `You asked to change your email, click here to confirm <a href="https://aurora.igloo.ooo/change-email?token=${token}">LOGIN</a>`,
      textBody: `You asked to change your email, visit this link to confirm https://aurora.igloo.ooo/change-email?token=${token}`,
      subject: "Email change",
    },
    MANAGE_PERMANENT_TOKENS: {
      htmlBody: `You attempted to change your permanent tokens, click here to confirm <a href="https://aurora.igloo.ooo/manage-permanent-tokens?token=${token}">LOGIN</a>`,
      textBody: `You attempted to change your permanent tokens, visit this link to confirm https://aurora.igloo.ooo/manage-permanent-tokens?token=${token}`,
      subject: "Manage permanent tokens",
    },
    CHANGE_AUTHENTICATION: {
      htmlBody: `You attempted to change your authentication factors, click here to confirm <a href="https://aurora.igloo.ooo/change-authentication?token=${token}">LOGIN</a>`,
      textBody: `You attempted to change your authentication factors, visit this link to confirm: https://aurora.igloo.ooo/change-authentication?token=${token}`,
      subject: "Change authentication",
    },
  }

  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: operationMap[operation].htmlBody,
          },
          Text: {
            Charset: "UTF-8",
            Data: operationMap[operation].textBody,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: operationMap[operation].subject,
        },
      },
    },
    console.log
  )
}

export const sendAccountDeletedEmail = email => {
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `Your account was deleted.\nWe are sad to see you go`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `Your account was deleted.\nWe are sad to see you go`,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "Account deleted",
        },
      },
    },
    console.log
  )
}

export const sendPasswordUpdatedEmail = email => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
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

export const sendTokenCreatedEmail = email => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
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
export const sendEnvironmentSharedEmail = (
  email,
  userName,
  environmentName
) => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `${userName} has shared the environment ${environmentName} with you. <a href="https://aurora.igloo.ooo">Check it out now</a>`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `${userName} has shared the environment ${environmentName} with you. Check it out on aurora.igloo.ooo`,
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
export const sendOwnerChangeEmail = (email, userName, environmentName) => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `The user ${userName} wants to make you the new owner of the environment "${environmentName}". <a href="https://aurora.igloo.ooo">Check it out now</a>`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `The user ${userName} wants to make you the new owner of the environment "${environmentName}". Check it out on aurora.igloo.ooo`,
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
export const sendOwnerChangeAcceptedEmail = (
  email,
  userName,
  environmentName
) => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `${userName} accepted your transfer request of "${environmentName}"`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `${userName} accepted your transfer request of "${environmentName}"`,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: "Transfer request accepted",
        },
      },
    },
    console.log
  )
}
export const sendEnvironmentShareAcceptedEmail = (
  email,
  userName,
  environmentName
) => {
  // TODO: create a template for the email verification
  ses.sendEmail(
    {
      Source: "'Igloo' <noreply@igloo.ooo>",
      Destination: { ToAddresses: [email] },
      Message: {
        Body: {
          Html: {
            Charset: "UTF-8",
            Data: `${userName} accepted your environment share of "${environmentName}"`,
          },
          Text: {
            Charset: "UTF-8",
            Data: `${userName} accepted your environment share of "${environmentName}"`,
          },
        },
        Subject: {
          Charset: "UTF-8",
          Data: `Environment ${environmentName} got accepted`,
        },
      },
    },
    console.log
  )
}

export async function authorizationLevel(
  instance,
  userFound,
  {
    dataLoaders: {
      environmentAdminLoaderByEnvironmentAndUserId,
      editorAdminLoaderByEnvironmentAndUserId,
      spectatorAdminLoaderByEnvironmentAndUserId,
    },
  }
) {
  const isOwner = userFound.id === instance.ownerId
  const userEnvironmentTuple = userFound.id + "|" + instance.id
  const isAdmin = await environmentAdminLoaderByEnvironmentAndUserId.load(
    userEnvironmentTuple
  )
  const isEditor = await editorAdminLoaderByEnvironmentAndUserId.load(
    userEnvironmentTuple
  )
  const isSpectator = await spectatorAdminLoaderByEnvironmentAndUserId.load(
    userEnvironmentTuple
  )

  if (isOwner) return 4
  else if (isAdmin) return 3
  else if (isEditor) return 2
  else if (isSpectator) return 1
  return 0
}

export function authorized(
  id,
  context,
  loader,
  User,
  authorizationRequired,
  callback,
  childToParent,
  acceptedTokenTypes
) {
  return authenticated(
    context,
    async (resolve, reject) => {
      const found = await loader.load(id)

      if (!found) {
        reject("The requested resource does not exist")
      } else {
        const parent = await childToParent(context)(found)

        if (!parent) {
          reject("You are not allowed to perform this operation")
          return
        }

        const userFound = await context.dataLoaders.userLoaderById.load(
          context.auth.userId
        )

        if (
          (await authorizationLevel(parent, userFound, context)) >=
          authorizationRequired
        ) {
          return callback(resolve, reject, found, [found, parent], userFound)
        } else {
          reject("You are not allowed to perform this operation")
        }
      }
    },
    acceptedTokenTypes
  )
}

export const authorizedRetrieveScalarProp = (
  loaderName,
  prop,
  childToParent,
  acceptedTokenTypes
) => (root, args, context) =>
  authorized(
    root.id,
    context,
    context.dataLoaders[loaderName],
    null,
    1,
    async (resolve, reject, resourceFound) => {
      resolve(resourceFound[prop])
    },
    childToParent,
    acceptedTokenTypes
  )

export const authorizedScalarPropsResolvers = (
  loaderName,
  props,
  childToParent,
  acceptedTokenTypes
) =>
  props.reduce((acc, prop) => {
    acc[prop] = authorizedRetrieveScalarProp(
      loaderName,
      prop,
      childToParent,
      acceptedTokenTypes
    )
    return acc
  }, {})

export function deviceAuthorized(
  id,
  context,
  authorizationRequired,
  callback = (resolve, reject, found, userFound) => {},
  acceptedTokenTypes = ["TEMPORARY", "PERMANENT", "DEVICE_ACCESS"]
) {
  return authenticated(
    context,
    async (resolve, reject) => {
      const found = await context.dataLoaders.deviceLoaderById.load(id)

      if (!found) {
        reject("The requested resource does not exist")
      } else if (found.producerId === context.auth.userId) {
        return callback(
          resolve,
          reject,
          found,
          await context.dataLoaders.userLoaderById.load(context.auth.userId)
        )
      } else if (
        context.auth.tokenType === "DEVICE_ACCESS" &&
        found.id === context.auth.deviceId
      ) {
        return callback(resolve, reject, found, null)
      } else {
        return authorized(
          id,
          context,
          context.dataLoaders.deviceLoaderById,
          null,
          authorizationRequired,
          (resolve, reject, found, _, userFound) =>
            callback(resolve, reject, found, userFound),
          deviceToParent,
          acceptedTokenTypes
        )(resolve, reject)
      }
    },
    acceptedTokenTypes
  )
}
export function producerAuthorized(id, context, callback) {
  return authenticated(context, async (resolve, reject) => {
    const found = await context.dataLoaders.deviceLoaderById.load(id)

    if (!found) {
      reject("The requested resource does not exist")
    } else if (
      found.producerId === context.auth.userId ||
      (context.auth.tokenType === "DEVICE_ACCESS" &&
        found.id === context.auth.deviceId)
    ) {
      return callback(resolve, reject, found)
    } else {
      reject("You are not allowed to perform this operation")
    }
  })
}

export const deviceToParent = ({
  dataLoaders: { environmentLoaderById },
}) => async deviceFound => {
  if (!deviceFound.environmentId) return null
  const environmentFound = await environmentLoaderById.load(
    deviceFound.environmentId
  )

  return environmentFound
}

export const valueToParent = ({
  dataLoaders: { environmentLoaderById, deviceLoaderById },
}) => async valueFound => {
  const { environmentId } = await deviceLoaderById.load(valueFound.deviceId)
  if (!environmentId) return null
  const environmentFound = await environmentLoaderById.load(environmentId)

  return environmentFound
}

export const producerAuthorizedValue = (
  id,
  context,
  callbackFunc = (
    resolve,
    reject,
    valueFound,
    deviceFound,
    environmentFound
  ) => {}
) => async (resolve, reject) => {
  const valueFound = await findValue(id, context)
  if (!valueFound) {
    reject("The requested resource does not exist")
  }

  return producerAuthorized(
    valueFound.deviceId,
    context,
    async (resolve, reject, deviceFound) => {
      const environmentFound =
        deviceFound.environmentId &&
        (await context.dataLoaders.environmentLoaderById.load(
          deviceFound.environmentId
        ))

      return callbackFunc(
        resolve,
        reject,
        valueFound,
        deviceFound,
        environmentFound
      )
    }
  )(resolve, reject)
}

export const valueAuthorized = (
  id,
  context,
  authorizationRequired,
  callback = (resolve, reject, valueFound, deviceFound, userFound) => {},
  acceptedTokenTypes
) => async (resolve, reject) => {
  const valueFound = await findValue(id, context)
  if (!valueFound) {
    reject("The requested resource does not exist")
  }

  return deviceAuthorized(
    valueFound.deviceId,
    context,
    authorizationRequired,
    (resolve, reject, deviceFound, userFound) =>
      callback(resolve, reject, valueFound, deviceFound, userFound),
    acceptedTokenTypes
  )(resolve, reject)
}

export const instanceToRole = async (instance, userFound, context) => {
  if (!userFound) return null
  const roleLevel = await authorizationLevel(instance, userFound, context)

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

export const instanceToSharedIds = async (
  instance,
  {
    dataLoaders: {
      allEnvironmentAdminsLoaderByEnvironmentId,
      allEnvironmentEditorsLoaderByEnvironmentId,
      allEnvironmentSpectatorsLoaderByEnvironmentId,
    },
  }
) => {
  const owner = instance.ownerId
  const admins = (await allEnvironmentAdminsLoaderByEnvironmentId.load(
    instance.id
  )).map(joinTable => joinTable.userId)
  const editors = (await allEnvironmentEditorsLoaderByEnvironmentId.load(
    instance.id
  )).map(joinTable => joinTable.userId)
  const spectators = (await allEnvironmentSpectatorsLoaderByEnvironmentId.load(
    instance.id
  )).map(joinTable => joinTable.userId)

  return [owner, ...admins, ...editors, ...spectators]
}

export const inheritAuthorized = (
  ownId,
  ownLoader,
  User,
  ownIstanceToParentId,
  context,
  parentLoader,
  authorizationRequired,
  callback,
  childToParent,
  acceptedTokenTypes
) => async (resolve, reject) => {
  const entityFound = await ownLoader.load(ownId)

  if (!entityFound) {
    reject("The requested resource does not exist")
  } else {
    return authorized(
      ownIstanceToParentId(entityFound),
      context,
      parentLoader,
      User,
      authorizationRequired,
      (resolve, reject, parentFound, allParents, userFound) =>
        callback(
          resolve,
          reject,
          entityFound,
          parentFound,
          allParents,
          userFound
        ),
      childToParent,
      acceptedTokenTypes
    )(resolve, reject)
  }
}

export const deviceInheritAuthorized = (
  ownId,
  ownLoader,
  context,
  authorizationRequired,
  callback,
  acceptedTokenTypes,
  ownIstanceToParentId = instance => instance.deviceId
) => async (resolve, reject) => {
  const entityFound = await ownLoader.load(ownId)

  if (!entityFound) {
    reject("The requested resource does not exist")
  } else {
    return deviceAuthorized(
      ownIstanceToParentId(entityFound),
      context,
      authorizationRequired,
      (resolve, reject, parentFound, userFound) =>
        callback(resolve, reject, entityFound, parentFound, userFound),
      acceptedTokenTypes
    )(resolve, reject)
  }
}

export const deviceInheritValueAuthorized = (
  ownId,
  ownLoader,
  context,
  authorizationRequired,
  callback,
  acceptedTokenTypes,
  accessLevel = 1 // 1 = READ, 2 = READ and WRITE, 3 = PRODUCER
) => async (resolve, reject) => {
  const found = await ownLoader.load(ownId)

  if (!found) {
    reject("The requested resource does not exist")
  } else if (
    found.private ||
    (accessLevel > 1 && found.permission === "READ_ONLY") ||
    accessLevel > 2
  ) {
    return producerAuthorized(found.deviceId, context, callback)(
      resolve,
      reject
    )
  } else {
    return deviceInheritAuthorized(
      ownId,
      ownLoader,
      context,
      authorizationRequired,
      callback,
      acceptedTokenTypes
    )(resolve, reject)
  }
}

export const deviceInheritValueAuthorizedScalarPropsResolvers = (
  ownLoaderName,
  props,
  acceptedTokenTypes
) =>
  props.reduce((acc, prop) => {
    acc[prop] = (root, args, context) =>
      deviceInheritValueAuthorized(
        root.id,
        context.dataLoaders[ownLoaderName],
        context,
        1,
        (resolve, reject, resourceFound) => resolve(resourceFound[prop]),
        acceptedTokenTypes
      )
    return acc
  }, {})

export const deviceInheritAuthorizedRetrieveScalarProp = (
  ownLoaderName,
  prop,
  acceptedTokenTypes,
  ownIstanceToParentId = instance => instance.deviceId
) => (root, args, context) =>
  deviceInheritAuthorized(
    root.id,
    context.dataLoaders[ownLoaderName],
    context,
    1,
    (resolve, reject, resourceFound) => resolve(resourceFound[prop]),
    acceptedTokenTypes,
    ownIstanceToParentId
  )

export const deviceInheritAuthorizedScalarPropsResolvers = (
  ownLoaderName,
  props,
  acceptedTokenTypes,
  ownIstanceToParentId = instance => instance.deviceId
) =>
  props.reduce((acc, prop) => {
    acc[prop] = deviceInheritAuthorizedRetrieveScalarProp(
      ownLoaderName,
      prop,
      acceptedTokenTypes,
      ownIstanceToParentId
    )
    return acc
  }, {})

export const inheritAuthorizedRetrieveScalarProp = (
  ownLoaderName,
  User,
  prop,
  ownIstanceToParentId,
  parentLoaderName,
  childToParent,
  acceptedTokenTypes
) => (root, args, context) =>
  inheritAuthorized(
    root.id,
    context.dataLoaders[ownLoaderName],
    User,
    ownIstanceToParentId,
    context,
    context.dataLoaders[parentLoaderName],
    1,
    (resolve, reject, resourceFound) => resolve(resourceFound[prop]),
    childToParent,
    acceptedTokenTypes
  )

export const inheritAuthorizedScalarPropsResolvers = (
  ownLoaderName,
  User,
  props,
  ownIstanceToParentId,
  parentLoaderName,
  childToParent,
  acceptedTokenTypes
) =>
  props.reduce((acc, prop) => {
    acc[prop] = inheritAuthorizedRetrieveScalarProp(
      ownLoaderName,
      User,
      prop,
      ownIstanceToParentId,
      parentLoaderName,
      childToParent,
      acceptedTokenTypes
    )
    return acc
  }, {})

export async function getAll(
  Model,
  User,
  userId,
  includesList = [],
  limit,
  offset,
  filter = {}
) {
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

  const allAccessibles = await User.findAll({
    where: { id: userId },
    attributes: ["id"],
    include: [
      {
        // where: filter,
        model: Model,
        as: Model.Owner,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
      {
        // where: filter,
        model: Model,
        as: Model.Admins,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
      {
        // where: filter,
        model: Model,
        as: Model.Editors,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
      {
        // where: filter,
        model: Model,
        as: Model.Spectators,
        attributes: ["id"],
        include: deepCloneIncludes(includesList),
      },
    ],
    limit,
    offset,
    subQuery: false,
    order: [["id", "DESC"]],
  })

  const allFlattened = [
    ...allAccessibles[0][Model.Owner],
    ...allAccessibles[0][Model.Admins],
    ...allAccessibles[0][Model.Editors],
    ...allAccessibles[0][Model.Spectators],
  ]

  return allFlattened
}

const randomChoice = (...args) => {
  let chooseAmong = args
  if (args.length === 1) chooseAmong = args[0]

  const randomIndex = Math.floor(Math.random() * chooseAmong.length)

  return chooseAmong[randomIndex]
}

export const randomEnvironmentPicture = () =>
  randomChoice(["NORTHERN_LIGHTS", "DENALI", "FOX", "PUFFIN", "TREETOPS"])

export const randomUserIconColor = () =>
  randomChoice(["#43A047", "#0097A7", "#9C27B0", "#D81B60", "#FF5722"])

// an environment is it's own parent
export const environmentToParent = context => x => x

export const runInParallel = async (...funcs) =>
  await Promise.all(funcs.map(f => f()))

export const sendPushNotification = async (
  userIds,
  payload,
  WebPushNotification
) => {
  const notificationSubscriptions = await WebPushNotification.findAll({
    where: {
      userId: {
        [Op.in]: userIds,
      },
    },
  })

  notificationSubscriptions.map(notificationSubscription =>
    webpush
      .sendNotification(
        {
          endpoint: notificationSubscription.endpoint,
          expirationTime: notificationSubscription.expirationTime,
          keys: {
            p256dh: notificationSubscription.p256dh,
            auth: notificationSubscription.auth,
          },
        },
        JSON.stringify(payload)
      )
      .catch(console.log)
  )
}

export const parseStringFilter = filter => {
  const parsedFilter = {}
  filter.hasOwnProperty = Object.prototype.hasOwnProperty

  if (filter.hasOwnProperty("equals")) parsedFilter[Op.eq] = filter.equals
  else if (filter.hasOwnProperty("similarTo"))
    parsedFilter[Op.regexp] = filter.similarTo
  else if (filter.hasOwnProperty("like")) parsedFilter[Op.like] = filter.like

  return parsedFilter
}

export const parseFloatFilter = filter => {
  const parsedFilter = {}
  filter.hasOwnProperty = Object.prototype.hasOwnProperty

  if (filter.hasOwnProperty("equals")) parsedFilter[Op.eq] = filter.equals
  else if (filter.hasOwnProperty("greaterThan"))
    parsedFilter[Op.gt] = filter.greaterThan
  else if (filter.hasOwnProperty("greaterOrEqualTo"))
    parsedFilter[Op.gte] = filter.greaterOrEqualTo
  else if (filter.hasOwnProperty("lessThan"))
    parsedFilter[Op.lt] = filter.lessThan
  else if (filter.hasOwnProperty("lessOrEqualTo"))
    parsedFilter[Op.lte] = filter.lessOrEqualTo

  return parsedFilter
}

export const parseDateFilter = parseFloatFilter
