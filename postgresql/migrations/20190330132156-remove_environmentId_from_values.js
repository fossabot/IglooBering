"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("floatValues", "environmentId")
    await queryInterface.removeColumn("stringValues", "environmentId")
    await queryInterface.removeColumn("booleanValues", "environmentId")
    await queryInterface.removeColumn("categoryPlotValues", "environmentId")
    await queryInterface.removeColumn("plotValues", "environmentId")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.createColumn("floatValues", "environmentId", {
      type: Sequelize.UUID,
      references: {
        model: "environments",
        key: "id",
      },
    })
    await queryInterface.createColumn("stringValues", "environmentId", {
      type: Sequelize.UUID,
      references: {
        model: "environments",
        key: "id",
      },
    })
    await queryInterface.createColumn("booleanValues", "environmentId", {
      type: Sequelize.UUID,
      references: {
        model: "environments",
        key: "id",
      },
    })
    await queryInterface.createColumn("categoryPlotValues", "environmentId", {
      type: Sequelize.UUID,
      references: {
        model: "environments",
        key: "id",
      },
    })
    await queryInterface.createColumn("plotValues", "environmentId", {
      type: Sequelize.UUID,
      references: {
        model: "environments",
        key: "id",
      },
    })
  },
}
