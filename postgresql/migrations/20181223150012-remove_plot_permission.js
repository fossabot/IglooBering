"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("plotValues", "permission")
    await queryInterface.removeColumn("categoryPlotValues", "permission")
  },

  down: async (queryInterface, Sequelize) => {
    const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")

    await queryInterface.addColumn("plotValues", "permission", {
      type: ValuePermission,
    })
    await queryInterface.addColumn("categoryPlotValues", "permission", {
      type: ValuePermission,
    })
  },
}
