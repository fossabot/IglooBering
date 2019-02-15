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
    settings_passwordChangeEmail: {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    },
    settings_pendingOwnerChangeReceivedEmail: {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    },
    settings_pendingEnvironmentShareReceivedEmail: {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    },
    settings_pendingOwnerChangeAcceptedEmail: {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    },
    settings_pendingEnvironmentShareAcceptedEmail: {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    },
    settings_permanentTokenCreatedEmail: {
      type: Sequelize.BOOLEAN,
      defaultValue: true,
    },
  })
  User.associate = function(models) {
    // associations can be defined here
  }
  return User
}
