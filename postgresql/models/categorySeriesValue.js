const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const CategorySeriesValue = queryInterface.define("categorySeriesValue", {
    ...ValueModel(Sequelize, false),
    allowedValues: {
      type: Sequelize.ARRAY(Sequelize.STRING),
    },
  })

  CategorySeriesValue.associate = function(models) {
    models.Device.hasMany(CategorySeriesValue)
  }
  return CategorySeriesValue
}
