"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("webauthnKeys", "publicKey", {
      type: Sequelize.STRING(2000),
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("webauthnKeys", "publicKey", {
      type: Sequelize.STRING,
    })
  },
}
