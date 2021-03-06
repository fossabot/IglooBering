import {
  User,
  Environment,
  Device,
  BooleanValue,
  CategorySeriesValue,
  EnvironmentAdmin,
  EnvironmentEditor,
  EnvironmentSpectator,
  FloatValue,
  Notification,
  PendingEnvironmentShare,
  PendingOwnerChange,
  PermanentToken,
  FloatSeriesNode,
  FileValue,
  FloatSeriesValue,
  CategorySeriesNode,
  StringValue,
  WebPushSubscription,
} from "../postgresql/models"
import DataLoader from "dataloader"
import { Op } from "sequelize"

const genericLoadById = Model => async keys => {
  const instancesFound = await Model.findAll({
    where: { id: { [Op.in]: keys.filter(key => key !== null) } },
  })

  const findInstanceWithId = id => {
    for (let instance of instancesFound) {
      if (instance.dataValues.id === id) {
        return instance
      }
    }

    return null
  }

  return keys.map(findInstanceWithId)
}

const genericLoadAllByField = (Model, field) => async keys => {
  const instancesFound = await Model.findAll({
    where: { [field]: { [Op.in]: keys } },
  })

  const findInstanceWithField = key => {
    let found = []
    for (let instance of instancesFound) {
      if (instance.dataValues[field] === key) {
        found.push(instance)
      }
    }

    return found
  }

  return keys.map(findInstanceWithField)
}

// keys should be an array of strings formatted like userId|environmentId
// this way deep equality check isn't needed and caching works
const genericRoleLoadByEnvironmentAndUserId = Model => async keys => {
  const parsedKeys = keys.map(key => key.split("|"))

  const instancesFound = await Model.findAll({
    where: {
      userId: { [Op.in]: parsedKeys.map(key => key[0]) },
      environmentId: { [Op.in]: parsedKeys.map(key => key[1]) },
    },
  })

  const findInstanceWithField = key => {
    for (let instance of instancesFound) {
      if (
        instance.dataValues.userId === key[0] &&
        instance.dataValues.environmentId === key[1]
      ) {
        return instance
      }
    }

    return null
  }

  return parsedKeys.map(findInstanceWithField)
}

const loadUsersByIds = genericLoadById(User)
const loadEnvironmentsByIds = genericLoadById(Environment)
const loadDevicesByIds = genericLoadById(Device)
const loadBooleanValuesByIds = genericLoadById(BooleanValue)
const loadCategorySeriesValuesByIds = genericLoadById(CategorySeriesValue)
const loadEnvironmentAdminsByIds = genericLoadById(EnvironmentAdmin)
const loadEnvironmentEditorsByIds = genericLoadById(EnvironmentEditor)
const loadEnvironmentSpectatorsByIds = genericLoadById(EnvironmentSpectator)
const loadFloatValuesByIds = genericLoadById(FloatValue)
const loadFileValuesByIds = genericLoadById(FileValue)
const loadNotificationsByIds = genericLoadById(Notification)
const loadPendingEnvironmentSharesByIds = genericLoadById(
  PendingEnvironmentShare
)
const loadPendingOwnerChangesByIds = genericLoadById(PendingOwnerChange)
const loadPermanentTokensByIds = genericLoadById(PermanentToken)
const loadSeriesNodesByIds = genericLoadById(FloatSeriesNode)
const loadFloatSeriesValuesByIds = genericLoadById(FloatSeriesValue)
const loadcategorySeriesNodesByIds = genericLoadById(CategorySeriesNode)
const loadStringValuesByIds = genericLoadById(StringValue)
const loadWebPushSubscriptionsByIds = genericLoadById(WebPushSubscription)

const loadAllEnvironmentAdminsByEnvironmentId = genericLoadAllByField(
  EnvironmentAdmin,
  "environmentId"
)
const loadAllEnvironmentEditorsByEnvironmentId = genericLoadAllByField(
  EnvironmentEditor,
  "environmentId"
)
const loadAllEnvironmentSpectatorsByEnvironmentId = genericLoadAllByField(
  EnvironmentSpectator,
  "environmentId"
)

const loadEnvironmentAdminByEnvironmentAndUserId = genericRoleLoadByEnvironmentAndUserId(
  EnvironmentAdmin
)
const loadEnvironmentEditorByEnvironmentAndUserId = genericRoleLoadByEnvironmentAndUserId(
  EnvironmentEditor
)
const loadEnvironmentSpectatorByEnvironmentAndUserId = genericRoleLoadByEnvironmentAndUserId(
  EnvironmentSpectator
)

module.exports = () => ({
  userLoaderById: new DataLoader(loadUsersByIds),
  environmentLoaderById: new DataLoader(loadEnvironmentsByIds),
  deviceLoaderById: new DataLoader(loadDevicesByIds),
  booleanValueLoaderById: new DataLoader(loadBooleanValuesByIds),
  categorySeriesValueLoaderById: new DataLoader(loadCategorySeriesValuesByIds),
  environmentAdminLoaderById: new DataLoader(loadEnvironmentAdminsByIds),
  environmentEditorLoaderById: new DataLoader(loadEnvironmentEditorsByIds),
  environmentSpectatorLoaderById: new DataLoader(
    loadEnvironmentSpectatorsByIds
  ),
  floatValueLoaderById: new DataLoader(loadFloatValuesByIds),
  fileValueLoaderById: new DataLoader(loadFileValuesByIds),
  notificationLoaderById: new DataLoader(loadNotificationsByIds),
  pendingEnvironmentShareLoaderById: new DataLoader(
    loadPendingEnvironmentSharesByIds
  ),
  pendingOwnerChangeLoaderById: new DataLoader(loadPendingOwnerChangesByIds),
  permanentTokenLoaderById: new DataLoader(loadPermanentTokensByIds),
  floatSeriesNodeLoaderById: new DataLoader(loadSeriesNodesByIds),
  floatSeriesValueLoaderById: new DataLoader(loadFloatSeriesValuesByIds),
  categorySeriesNodeLoaderById: new DataLoader(loadcategorySeriesNodesByIds),
  stringValueLoaderById: new DataLoader(loadStringValuesByIds),
  webPushSubscriptionLoaderById: new DataLoader(loadWebPushSubscriptionsByIds),

  allEnvironmentAdminsLoaderByEnvironmentId: new DataLoader(
    loadAllEnvironmentAdminsByEnvironmentId
  ),
  allEnvironmentEditorsLoaderByEnvironmentId: new DataLoader(
    loadAllEnvironmentEditorsByEnvironmentId
  ),
  allEnvironmentSpectatorsLoaderByEnvironmentId: new DataLoader(
    loadAllEnvironmentSpectatorsByEnvironmentId
  ),

  environmentAdminLoaderByEnvironmentAndUserId: new DataLoader(
    loadEnvironmentAdminByEnvironmentAndUserId
  ),
  editorAdminLoaderByEnvironmentAndUserId: new DataLoader(
    loadEnvironmentEditorByEnvironmentAndUserId
  ),
  spectatorAdminLoaderByEnvironmentAndUserId: new DataLoader(
    loadEnvironmentSpectatorByEnvironmentAndUserId
  ),
})
