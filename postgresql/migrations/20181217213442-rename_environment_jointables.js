"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameTable("EnvironmentAdmins", "environmentAdmins")
    await queryInterface.renameTable("EnvironmentEditors", "environmentEditors")
    await queryInterface.renameTable(
      "EnvironmentSpectators",
      "environmentSpectators"
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameTable("environmentAdmins", "EnvironmentAdmins")
    await queryInterface.renameTable("environmentEditors", "EnvironmentEditors")
    await queryInterface.renameTable(
      "environmentSpectators",
      "EnvironmentSpectators"
    )
  },
}
