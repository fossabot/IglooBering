import { User, Environment } from "../postgresql/models"
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
}
