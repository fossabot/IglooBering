const { ValueModel } = require("../modelUtilities")

module.exports = (queryInterface, Sequelize) => {
  const FileValue = queryInterface.define("fileValue", {
    ...ValueModel(Sequelize),
    value: {
      type: Sequelize.TEXT,
    },
    mimeType: {
      type: Sequelize.STRING,
    },
    fileName: {
      type: Sequelize.STRING,
    },
  })

  FileValue.associate = function(models) {
    models.Device.hasMany(FileValue)
  }
  return FileValue
}
