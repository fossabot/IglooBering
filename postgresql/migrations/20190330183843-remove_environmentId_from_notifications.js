"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("notifications", "environmentId")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.createColumn("notifications", "environmentId", {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "environments",
        key: "id",
      },
    })
  },
}
