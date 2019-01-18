"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("stringValues", "unitOfMeasurement")
    await queryInterface.removeColumn("booleanValues", "unitOfMeasurement")
    await queryInterface.removeColumn("categoryPlotValues", "unitOfMeasurement")
    await queryInterface.removeColumn("mapValues", "unitOfMeasurement")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("stringValues", "unitOfMeasurement", {
      type: Sequelize.STRING,
    })
    await queryInterface.addColumn("booleanValues", "unitOfMeasurement", {
      type: Sequelize.STRING,
    })
    await queryInterface.addColumn("categoryPlotValues", "unitOfMeasurement", {
      type: Sequelize.STRING,
    })
    await queryInterface.addColumn("mapValues", "unitOfMeasurement", {
      type: Sequelize.STRING,
    })
  },
}
