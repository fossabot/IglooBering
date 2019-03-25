"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("users", "profileIcon")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("users", "profileIcon", {
      type: String,
    })
  },
}
