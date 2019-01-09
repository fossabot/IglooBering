"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn("booleanValues", "tileSize", "cardSize")
    await queryInterface.renameColumn("floatValues", "tileSize", "cardSize")
    await queryInterface.renameColumn("stringValues", "tileSize", "cardSize")
    await queryInterface.renameColumn("plotValues", "tileSize", "cardSize")
    await queryInterface.renameColumn(
      "categoryPlotValues",
      "tileSize",
      "cardSize"
    )
    await queryInterface.renameColumn("mapValues", "tileSize", "cardSize")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn("booleanValues", "cardSize", "tileSize")
    await queryInterface.renameColumn("floatValues", "cardSize", "tileSize")
    await queryInterface.renameColumn("stringValues", "cardSize", "tileSize")
    await queryInterface.renameColumn("plotValues", "cardSize", "tileSize")
    await queryInterface.renameColumn(
      "categoryPlotValues",
      "cardSize",
      "tileSize"
    )
    await queryInterface.renameColumn("mapValues", "cardSize", "tileSize")
  },
}
