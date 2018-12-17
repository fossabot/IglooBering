const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const StringPlotValue = queryInterface.define("stringPlotValue", {
    ...ValueModel(Sequelize),
    allowedValues: {
      type: Sequelize.ARRAY(Sequelize.STRING),
    },
  })

  StringPlotValue.associate = function(models) {
    models.Device.hasMany(StringPlotValue)
    models.Environment.hasMany(StringPlotValue)
  }
  return StringPlotValue
}
