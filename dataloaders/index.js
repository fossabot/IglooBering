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
  StringPlotNode,
  StringValue,
  WebPushSubscription,
} from "../postgresql/models"
import { Op } from "sequelize"

const genericLoadById = Model => async keys => {
  const instancesFound = await Model.findAll({
    where: { id: { [Op.in]: keys } },
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

module.exports = {
  loadUsersByIds: genericLoadById(User),
  loadEnvironmentsByIds: genericLoadById(Environment),
  loadDevicesByIds: genericLoadById(Device),
  loadBooleanValuesByIds: genericLoadById(BooleanValue),
  loadCategoryPlotValuesByIds: genericLoadById(CategoryPlotValue),
  loadEnvironmentAdminsByIds: genericLoadById(EnvironmentAdmin),
  loadEnvironmentEditorsByIds: genericLoadById(EnvironmentEditor),
  loadEnvironmentSpectatorsByIds: genericLoadById(EnvironmentSpectator),
  loadFloatValuesByIds: genericLoadById(FloatValue),
  loadMapValuesByIds: genericLoadById(MapValue),
  loadNotificationsByIds: genericLoadById(Notification),
  loadPendingEnvironmentSharesByIds: genericLoadById(PendingEnvironmentShare),
  loadPendingOwnerChangesByIds: genericLoadById(PendingOwnerChange),
  loadPermanentTokensByIds: genericLoadById(PermanentToken),
  loadPlotNodesByIds: genericLoadById(PlotNode),
  loadPlotValuesByIds: genericLoadById(PlotValue),
  loadStringPlotNodesByIds: genericLoadById(StringPlotNode),
  loadStringValuesByIds: genericLoadById(StringValue),
  loadWebPushSubscriptionsByIds: genericLoadById(WebPushSubscription),
}
