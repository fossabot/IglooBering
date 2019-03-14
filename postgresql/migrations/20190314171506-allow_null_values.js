"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("floatValues", "value", {
      type: Sequelize.FLOAT,
      allowNull: true,
    })
    await queryInterface.changeColumn("stringValues", "value", {
      type: Sequelize.TEXT,
      allowNull: true,
    })
    await queryInterface.changeColumn("booleanValues", "value", {
      type: Sequelize.BOOLEAN,
      allowNull: true,
    })
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.changeColumn("floatValues", "value", {
      type: Sequelize.FLOAT,
      allowNull: false,
    })
    await queryInterface.changeColumn("stringValues", "value", {
      type: Sequelize.TEXT,
      allowNull: false,
    })
    await queryInterface.changeColumn("booleanValues", "value", {
      type: Sequelize.BOOLEAN,
      allowNull: false,
    })
  },
}
