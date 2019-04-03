"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameTable(
      "categoryPlotValues",
      "categorySeriesValues"
    )
    await queryInterface.renameTable("plotValues", "floatSeriesValues")
    await queryInterface.renameTable("categoryPlotNodes", "categorySeriesNodes")
    await queryInterface.renameTable("plotNodes", "floatSeriesNodes")
    await queryInterface.renameColumn("floatSeriesNodes", "plotId", "seriesId")
    await queryInterface.renameColumn(
      "categorySeriesNodes",
      "plotId",
      "seriesId"
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn("floatSeriesNodes", "seriesId", "plotId")
    await queryInterface.renameColumn(
      "categorySeriesNodes",
      "seriesId",
      "plotId"
    )
    await queryInterface.renameTable(
      "categorySeriesValues",
      "categoryPlotValues"
    )
    await queryInterface.renameTable("floatSeriesValues", "plotValues")
    await queryInterface.renameTable("categorySeriesNodes", "categoryPlotNodes")
    await queryInterface.renameTable("floatSeriesNodes", "plotNodes")
  },
}
