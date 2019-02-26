"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("users", "primaryAuthenticationMethods", {
      type: Sequelize.ARRAY(Sequelize.STRING),
      defaultValue: [],
    })
    await queryInterface.addColumn("users", "secondaryAuthenticationMethods", {
      type: Sequelize.ARRAY(Sequelize.STRING),
      defaultValue: [],
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("users", "primaryAuthenticationMethods")
    await queryInterface.removeColumn("users", "secondaryAuthenticationMethods")
  },
}
