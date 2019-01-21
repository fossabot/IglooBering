"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    const PaymentPlan = Sequelize.ENUM("FREE", "INDIVIDUAL", "BUSINESS")
    await queryInterface.addColumn("users", "_paymentPlan", {
      type: PaymentPlan,
    })
    await queryInterface.sequelize.query(
      `UPDATE public."users" SET "_paymentPlan" = 'FREE' WHERE "paymentPlan" = 'FREE'`
    )
    await queryInterface.sequelize.query(
      `UPDATE public."users" SET "_paymentPlan" = 'INDIVIDUAL' WHERE "paymentPlan" = 'PAYING'`
    )
    await queryInterface.removeColumn("users", "paymentPlan")
    await queryInterface.renameColumn("users", "_paymentPlan", "paymentPlan")
    await queryInterface.sequelize.query(`DROP TYPE "enum_users_paymentPlan"`)
    await queryInterface.sequelize.query(
      `ALTER TYPE "enum_users__paymentPlan" RENAME TO "enum_users_paymentPlan"`
    )
  },

  down: async (queryInterface, Sequelize) => {
    const PaymentPlan = Sequelize.ENUM("FREE", "PAYING")

    await queryInterface.addColumn("users", "_paymentPlan", {
      type: PaymentPlan,
    })
    await queryInterface.sequelize.query(
      `UPDATE public."users" SET "_paymentPlan" = 'FREE' WHERE "paymentPlan" = 'FREE'`
    )
    await queryInterface.sequelize.query(
      `UPDATE public."users" SET "_paymentPlan" = 'PAYING' WHERE "paymentPlan" = 'INDIVIDUAL'`
    )
    await queryInterface.sequelize.query(
      `UPDATE public."users" SET "_paymentPlan" = 'PAYING' WHERE "paymentPlan" = 'BUSINESS'`
    )
    await queryInterface.removeColumn("users", "paymentPlan")
    await queryInterface.renameColumn("users", "_paymentPlan", "paymentPlan")
    await queryInterface.sequelize.query(`DROP TYPE "enum_users_paymentPlan"`)
    await queryInterface.sequelize.query(
      `ALTER TYPE "enum_users__paymentPlan" RENAME TO "enum_users_paymentPlan"`
    )
  },
}
