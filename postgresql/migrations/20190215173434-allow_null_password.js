"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("users", "password", {
      type: Sequelize.STRING,
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("users", "password", {
      type: Sequelize.STRING,
      allowNull: false,
    })
  },
}
