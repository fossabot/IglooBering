"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
    const ValueVisibility = Sequelize.ENUM("VISIBLE", "INVISIBLE")
    const cardSize = Sequelize.ENUM("NORMAL", "WIDE", "TALL", "LARGE")

    await queryInterface.createTable("fileValues", {
      id: {
        type: Sequelize.UUID,
        defaultValue: Sequelize.UUIDV4,
        primaryKey: true,
      },
      createdAt: {
        type: Sequelize.DATE,
      },
      updatedAt: {
        type: Sequelize.DATE,
      },
      deviceId: {
        type: Sequelize.UUID,
        allowNull: false,
        references: {
          model: "devices",
          key: "id",
        },
      },
      permission: {
        type: ValuePermission,
      },
      visibility: {
        type: ValueVisibility,
      },
      cardSize: {
        type: cardSize,
      },
      name: {
        type: Sequelize.STRING,
      },
      index: {
        type: Sequelize.INTEGER,
      },
      value: {
        type: Sequelize.TEXT,
      },
      mimeType: {
        type: Sequelize.STRING,
      },
      fileName: {
        type: Sequelize.STRING,
      },
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.dropTable("fileValues")
  },
}
