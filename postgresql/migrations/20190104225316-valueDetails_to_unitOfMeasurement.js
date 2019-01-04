"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn(
      "booleanValues",
      "valueDetails",
      "unitOfMeasurement"
    )
    await queryInterface.renameColumn(
      "floatValues",
      "valueDetails",
      "unitOfMeasurement"
    )
    await queryInterface.renameColumn(
      "stringValues",
      "valueDetails",
      "unitOfMeasurement"
    )
    await queryInterface.renameColumn(
      "plotValues",
      "valueDetails",
      "unitOfMeasurement"
    )
    await queryInterface.renameColumn(
      "categoryPlotValues",
      "valueDetails",
      "unitOfMeasurement"
    )
    await queryInterface.renameColumn(
      "mapValues",
      "valueDetails",
      "unitOfMeasurement"
    )
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.renameColumn(
      "booleanValues",
      "unitOfMeasurement",
      "valueDetails"
    )
    await queryInterface.renameColumn(
      "floatValues",
      "unitOfMeasurement",
      "valueDetails"
    )
    await queryInterface.renameColumn(
      "stringValues",
      "unitOfMeasurement",
      "valueDetails"
    )
    await queryInterface.renameColumn(
      "plotValues",
      "unitOfMeasurement",
      "valueDetails"
    )
    await queryInterface.renameColumn(
      "categoryPlotValues",
      "unitOfMeasurement",
      "valueDetails"
    )
    await queryInterface.renameColumn(
      "mapValues",
      "unitOfMeasurement",
      "valueDetails"
    )
  },
}
