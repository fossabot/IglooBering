"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("devices", "producerId", {
      type: Sequelize.UUID,
      references: {
        model: "users",
        key: "id",
      },
    })
    await queryInterface.addColumn("devices", "producerHasAccess", {
      type: Sequelize.BOOLEAN,
    })
    await queryInterface.addColumn("devices", "producerIsBilled", {
      type: Sequelize.BOOLEAN,
    })
    await queryInterface.changeColumn("devices", "environmentId", {
      type: Sequelize.UUID,
      allowNull: true,
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("devices", "producerId")
    await queryInterface.removeColumn("devices", "producerHasAccess")
    await queryInterface.removeColumn("devices", "producerIsBilled")
    await queryInterface.changeColumn("devices", "environmentId", {
      type: Sequelize.UUID,
      allowNull: false,
    })
  },
}
