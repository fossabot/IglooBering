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
    boundaries: {
      type: Sequelize.ARRAY(Sequelize.FLOAT),
    },
  })

  PlotValue.associate = function(models) {
    models.Device.hasMany(PlotValue)
    models.Environment.hasMany(PlotValue)
  }
  return PlotValue
}
