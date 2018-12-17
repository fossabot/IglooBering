const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const StringValue = queryInterface.define("stringValue", {
    ...ValueModel(Sequelize),
    value: {
      type: Sequelize.TEXT,
      allowNull: false,
    },
    maxChars: {
      type: Sequelize.INTEGER,
    },
    allowedValues: {
      type: Sequelize.ARRAY(Sequelize.STRING),
    },
  })

  StringValue.associate = function(models) {
    models.Device.hasMany(StringValue)
    models.Environment.hasMany(StringValue)
  }
  return StringValue
}
