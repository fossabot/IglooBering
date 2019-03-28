"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("floatValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: true,
    })
    await queryInterface.changeColumn("stringValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: true,
    })
    await queryInterface.changeColumn("booleanValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: true,
    })
    await queryInterface.changeColumn("categoryPlotValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: true,
    })
    await queryInterface.changeColumn("plotValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: true,
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("floatValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: false,
    })
    await queryInterface.changeColumn("stringValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: false,
    })
    await queryInterface.changeColumn("booleanValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: false,
    })
    await queryInterface.changeColumn("categoryPlotValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: false,
    })
    await queryInterface.changeColumn("plotValues", "environmentId", {
      type: Sequelize.UUID,
      alloNull: false,
    })
  },
}
