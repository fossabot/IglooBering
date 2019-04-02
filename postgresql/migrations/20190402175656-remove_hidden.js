"use strict"

const replaceEnum = require("sequelize-replace-enum-postgres").default

module.exports = {
  up: async (queryInterface, Sequelize) => {
    const ValueVisibility = Sequelize.ENUM("VISIBLE", "INVISIBLE")

    await queryInterface.addColumn("floatValues", "visibility_tmp", {
      type: ValueVisibility,
    })
    await queryInterface.removeColumn("floatValues", "visibility")
    await queryInterface.renameColumn(
      "floatValues",
      "visibility_tmp",
      "visibility"
    )

    await queryInterface.addColumn("stringValues", "visibility_tmp", {
      type: ValueVisibility,
    })
    await queryInterface.removeColumn("stringValues", "visibility")
    await queryInterface.renameColumn(
      "stringValues",
      "visibility_tmp",
      "visibility"
    )

    await queryInterface.addColumn("booleanValues", "visibility_tmp", {
      type: ValueVisibility,
    })
    await queryInterface.removeColumn("booleanValues", "visibility")
    await queryInterface.renameColumn(
      "booleanValues",
      "visibility_tmp",
      "visibility"
    )

    await queryInterface.addColumn("plotValues", "visibility_tmp", {
      type: ValueVisibility,
    })
    await queryInterface.removeColumn("plotValues", "visibility")
    await queryInterface.renameColumn(
      "plotValues",
      "visibility_tmp",
      "visibility"
    )

    await queryInterface.addColumn("categoryPlotValues", "visibility_tmp", {
      type: ValueVisibility,
    })
    await queryInterface.removeColumn("categoryPlotValues", "visibility")
    await queryInterface.renameColumn(
      "categoryPlotValues",
      "visibility_tmp",
      "visibility"
    )
  },

  down: async (queryInterface, Sequelize) => {},
}
