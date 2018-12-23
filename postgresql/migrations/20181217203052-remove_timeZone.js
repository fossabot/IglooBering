"use strict"

module.exports = {
  up: (queryInterface, Sequelize) => {
    return queryInterface.removeColumn("users", "settings_timeZone")
  },

  down: (queryInterface, Sequelize) => {
    return queryInterface.addColumn("users", "settings_timeZone", {
      type: Sequelize.STRING,
    })
  },
}
