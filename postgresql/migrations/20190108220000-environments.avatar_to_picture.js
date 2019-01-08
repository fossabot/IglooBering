"use strict"

module.exports = {
  up: (queryInterface, Sequelize) => {
    return queryInterface.renameColumn("environments", "avatar", "picture")
  },

  down: (queryInterface, Sequelize) => {
    return queryInterface.renameColumn("environments", "picture", "avatar")
  },
}
