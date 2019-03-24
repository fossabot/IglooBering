"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.removeColumn("users", "paymentPlan")
  },

  down: async (queryInterface, Sequelize) => {
    const PaymentPlan = Sequelize.ENUM("FREE", "PAYING")

    await queryInterface.addColumn("users", "paymentPlan", {
      type: PaymentPlan,
    })
  },
}
