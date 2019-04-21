"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("floatValues", "visibility")
    await queryInterface.addColumn("floatValues", "hidden", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.addColumn("floatValues", "private", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.removeColumn("stringValues", "visibility")
    await queryInterface.addColumn("stringValues", "hidden", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.addColumn("stringValues", "private", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.removeColumn("booleanValues", "visibility")
    await queryInterface.addColumn("booleanValues", "hidden", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.addColumn("booleanValues", "private", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.removeColumn("floatSeriesValues", "visibility")
    await queryInterface.addColumn("floatSeriesValues", "hidden", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.addColumn("floatSeriesValues", "private", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.removeColumn("fileValues", "visibility")
    await queryInterface.addColumn("fileValues", "hidden", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.addColumn("fileValues", "private", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.removeColumn("categorySeriesValues", "visibility")
    await queryInterface.addColumn("categorySeriesValues", "hidden", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
    await queryInterface.addColumn("categorySeriesValues", "private", {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    })
  },

  down: (queryInterface, Sequelize) => {},
}
