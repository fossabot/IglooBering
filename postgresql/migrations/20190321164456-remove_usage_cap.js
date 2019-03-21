"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("users", "usageCap")
    await queryInterface.removeColumn("users", "monthUsage")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("users", "usageCap", {
      type: Sequelize.INTEGER,
    })
    await queryInterface.addColumn("users", "monthUsage", {
      type: Sequelize.INTEGER,
    })
  },
}
