"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("devices", "starred", {
      type: Sequelize.ARRAY(Sequelize.UUID),
      defaultValue: [],
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("devices", "starred")
  },
}
