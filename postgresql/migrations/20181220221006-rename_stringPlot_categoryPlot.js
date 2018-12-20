"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameTable("stringPlotValues", "categoryPlotValues")
    await queryInterface.renameTable("stringPlotNodes", "categoryPlotNodes")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameTable("categoryPlotValues", "stringPlotValues")
    await queryInterface.renameTable("categoryPlotNodes", "stringPlotNodes")
  },
}
