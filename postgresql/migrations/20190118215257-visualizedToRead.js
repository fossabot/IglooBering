"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn(
      "notifications",
      "notVisualized",
      "notRead"
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn(
      "notifications",
      "notVisualized",
      "notRead"
    )
  },
}
