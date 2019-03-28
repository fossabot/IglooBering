"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("notifications", "userId")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("notifications", "userId", {
      type: Sequelize.UUID,
      references: {
        model: "users", // name of Target model
        key: "id", // key in Target model that we're referencing
      },
      onUpdate: "CASCADE",
      onDelete: "SET NULL",
    })
  },
}
