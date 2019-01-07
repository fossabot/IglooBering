"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("notifications", "notVisualized", {
      type: Sequelize.ARRAY(Sequelize.UUID),
    })
    await queryInterface.sequelize.query(
      `UPDATE public."notifications" SET "notVisualized" = '{}'`
    )
    await queryInterface.removeColumn("notifications", "visualized")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("notifications", "notVisualized")
    await queryInterface.addColumn("notifications", "visualized", {
      type: Sequelize.ARRAY(Sequelize.UUID),
    })
  },
}
