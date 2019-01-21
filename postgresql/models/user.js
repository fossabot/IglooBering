module.exports = (queryInterface, Sequelize) => {
  const PaymentPlan = Sequelize.ENUM("FREE", "INDIVIDUAL", "BUSINESS")

  const User = queryInterface.define("user", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    email: {
      type: Sequelize.STRING,
      allowNull: false,
      validate: {
        isEmail: true,
      },
    },
    password: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    twoFactorSecret: {
      type: Sequelize.STRING,
    },
    quietMode: {
      type: Sequelize.BOOLEAN,
    },
    stripeCustomerId: {
      type: Sequelize.STRING,
    },
    monthUsage: {
      type: Sequelize.INTEGER,
    },
    paymentPlan: {
      type: PaymentPlan,
    },
    usageCap: {
      type: Sequelize.INTEGER,
    },
    name: {
      type: Sequelize.STRING,
    },
    profileIcon: {
      type: Sequelize.STRING,
    },
    profileIconColor: {
      type: Sequelize.STRING,
    },
    emailIsVerified: {
      type: Sequelize.BOOLEAN,
    },
    settings_language: {
      type: Sequelize.STRING,
    },
    settings_lengthAndMass: {
      type: Sequelize.STRING,
    },
    settings_temperature: {
      type: Sequelize.STRING,
    },
    settings_dateFormat: {
      type: Sequelize.STRING,
    },
    settings_timeFormat: {
      type: Sequelize.STRING,
    },
  })
  User.associate = function(models) {
    // associations can be defined here
  }
  return User
}
