"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("users", "settings_passwordChangeEmail", {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    })
    await queryInterface.addColumn(
      "users",
      "settings_pendingOwnerChangeReceivedEmail",
      {
        type: Sequelize.BOOLEAN,
        defaultValue: true,
      }
    )
    await queryInterface.addColumn(
      "users",
      "settings_pendingEnvironmentChangeReceiverEmail",
      {
        type: Sequelize.BOOLEAN,
        defaultValue: true,
      }
    )
    await queryInterface.addColumn(
      "users",
      "settings_pendingOwnerChangeAcceptedEmail",
      {
        type: Sequelize.BOOLEAN,
        defaultValue: true,
      }
    )
    await queryInterface.addColumn(
      "users",
      "settings_permanentEnvironmentChangeAcceptedEmail",
      {
        type: Sequelize.BOOLEAN,
        defaultValue: true,
      }
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("users", "settings_passwordChangeEmail")
    await queryInterface.removeColumn(
      "users",
      "settings_pendingOwnerChangeReceivedEmail"
    )
    await queryInterface.removeColumn(
      "users",
      "settings_pendingEnvironmentChangeReceiverEmail"
    )
    await queryInterface.removeColumn(
      "users",
      "settings_pendingOwnerChangeAcceptedEmail"
    )
    await queryInterface.removeColumn(
      "users",
      "settings_permanentEnvironmentChangeAcceptedEmail"
    )
  },
}
