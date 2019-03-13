"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.dropTable("mapValues")
  },

  down: async (queryInterface, Sequelize) => {
    const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
    const ValueVisibility = Sequelize.ENUM("VISIBLE", "HIDDEN", "INVISIBLE")
    const cardSize = Sequelize.ENUM("NORMAL", "WIDE", "TALL", "LARGE")

    const Value = {
      id: {
        type: Sequelize.UUID,
        defaultValue: Sequelize.UUIDV4,
        primaryKey: true,
      },
      environmentId: {
        type: Sequelize.UUID,
        allowNull: false,
        references: {
          model: "environments",
          key: "id",
        },
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
    }
    await queryInterface.createTable("mapValues", {
      ...Value,
      latitude: {
        type: Sequelize.FLOAT,
      },
      longitude: {
        type: Sequelize.FLOAT,
      },
      height: {
        type: Sequelize.FLOAT,
      },
      value: {
        type: Sequelize.TEXT,
      },
    })
  },
}
