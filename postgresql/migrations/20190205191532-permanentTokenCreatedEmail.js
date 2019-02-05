"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn(
      "users",
      "settings_permanentTokenCreatedEmail",
      {
        type: Sequelize.BOOLEAN,
        defaultValue: true,
      }
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn(
      "users",
      "settings_permanentTokenCreatedEmail"
    )
  },
}
