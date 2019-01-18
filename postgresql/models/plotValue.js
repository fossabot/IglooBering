const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const PlotValue = queryInterface.define("plotValue", {
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

  PlotValue.associate = function(models) {
    models.Device.hasMany(PlotValue)
    models.Environment.hasMany(PlotValue)
  }
  return PlotValue
}
