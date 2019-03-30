const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const FloatValue = queryInterface.define("floatValue", {
    ...ValueModel(Sequelize),
    value: {
      type: Sequelize.FLOAT,
    },
    precision: {
      type: Sequelize.FLOAT,
    },
    min: {
      type: Sequelize.FLOAT,
    },
    max: {
      type: Sequelize.FLOAT,
    },
    unitOfMeasurement: {
      type: Sequelize.STRING,
    },
  })

  FloatValue.associate = function(models) {
    models.Device.hasMany(FloatValue)
  }
  return FloatValue
}
