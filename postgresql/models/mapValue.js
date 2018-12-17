const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const MapValue = queryInterface.define("mapValue", {
    ...ValueModel(Sequelize),
    latitude: {
      type: Sequelize.FLOAT,
    },
    longitude: {
      type: Sequelize.FLOAT,
    },
    height: {
      type: Sequelize.FLOAT,
    },
    value: {
      type: Sequelize.TEXT,
    },
  })

  MapValue.associate = function(models) {
    models.Device.hasMany(MapValue)
    models.Environment.hasMany(MapValue)
  }
  return MapValue
}
