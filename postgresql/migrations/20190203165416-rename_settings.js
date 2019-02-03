"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn(
      "users",
      "settings_pendingEnvironmentChangeReceiverEmail",
      "settings_pendingEnvironmentShareReceivedEmail"
    )
    await queryInterface.renameColumn(
      "users",
      "settings_permanentEnvironmentChangeAcceptedEmail",
      "settings_pendingEnvironmentShareAcceptedEmail"
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn(
      "users",
      "settings_pendingEnvironmentShareReceivedEmail",
      "settings_pendingEnvironmentChangeReceiverEmail"
    )
    await queryInterface.renameColumn(
      "users",
      "settings_pendingEnvironmentShareAcceptedEmail",
      "settings_permanentEnvironmentChangeAcceptedEmail"
    )
  },
}
