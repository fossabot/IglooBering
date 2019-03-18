"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("devices", "producerHasAccess")
    await queryInterface.removeColumn("devices", "producerIsBilled")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("devices", "producerHasAccess", {
      type: Sequelize.BOOLEAN,
    })
    await queryInterface.addColumn("devices", "producerIsBilled", {
      type: Sequelize.BOOLEAN,
    })
  },
}
