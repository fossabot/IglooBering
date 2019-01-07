const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const FloatValue = queryInterface.define("floatValue", {
    ...ValueModel(Sequelize),
    value: {
      type: Sequelize.FLOAT,
      allowNull: false,
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
  })

  FloatValue.associate = function(models) {
    models.Device.hasMany(FloatValue)
    models.Environment.hasMany(FloatValue)
  }
  return FloatValue
}
