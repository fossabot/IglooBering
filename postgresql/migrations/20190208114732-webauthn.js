"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.createTable("webauthnKeys", {
      id: {
        type: Sequelize.UUID,
        defaultValue: Sequelize.UUIDV4,
        primaryKey: true,
      },
      updatedAt: {
        type: Sequelize.DATE,
      },
      createdAt: {
        type: Sequelize.DATE,
      },
      userId: {
        type: Sequelize.UUID,
        allowNull: false,
        references: {
          model: "users",
          key: "id",
        },
      },
      counter: {
        type: Sequelize.FLOAT,
      },
      publicKey: {
        type: Sequelize.STRING,
      },
      credId: {
        type: Sequelize.STRING,
      },
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.dropTable("webauthnKeys")
  },
}
