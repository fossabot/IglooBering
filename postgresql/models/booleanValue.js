const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const BooleanValue = queryInterface.define("booleanValue", {
    ...ValueModel(Sequelize),
    value: {
      type: Sequelize.BOOLEAN,
    },
  })

  BooleanValue.associate = function(models) {
    models.Device.hasMany(BooleanValue)
  }
  return BooleanValue
}
