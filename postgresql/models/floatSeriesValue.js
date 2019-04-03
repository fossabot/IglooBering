const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const FloatSeriesValue = queryInterface.define("floatSeriesValue", {
    ...ValueModel(Sequelize, false),
    precision: {
      type: Sequelize.FLOAT,
    },
    threshold: {
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

  FloatSeriesValue.associate = function(models) {
    models.Device.hasMany(FloatSeriesValue)
  }
  return FloatSeriesValue
}
