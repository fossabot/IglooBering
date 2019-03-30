const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const CategoryPlotValue = queryInterface.define("categoryPlotValue", {
    ...ValueModel(Sequelize, false),
    allowedValues: {
      type: Sequelize.ARRAY(Sequelize.STRING),
    },
  })

  CategoryPlotValue.associate = function(models) {
    models.Device.hasMany(CategoryPlotValue)
  }
  return CategoryPlotValue
}
