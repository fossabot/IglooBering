module.exports = (queryInterface, Sequelize) => {
  const EmailLoginToken = queryInterface.define("emailLoginToken", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    createdAt: {
      type: Sequelize.DATE,
    },
    updatedAt: {
      type: Sequelize.DATE,
    },
    userId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "users",
        key: "id",
      },
    },
  })
  EmailLoginToken.associate = function(models) {
    EmailLoginToken.belongsTo(models.User)
    models.User.hasMany(EmailLoginToken)
  }
  return EmailLoginToken
}
