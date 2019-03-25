module.exports = (queryInterface, Sequelize) => {
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
    devMode: {
      type: Sequelize.BOOLEAN,
    },
    stripeCustomerId: {
      type: Sequelize.STRING,
    },
    name: {
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
    primaryAuthenticationMethods: {
      type: Sequelize.ARRAY(Sequelize.STRING),
      defaultValue: [],
    },
    secondaryAuthenticationMethods: {
      type: Sequelize.ARRAY(Sequelize.STRING),
      defaultValue: [],
    },
  })
  User.associate = function(models) {
    // associations can be defined here
  }
  return User
}
