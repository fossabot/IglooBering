module.exports = (queryInterface, Sequelize) => {
  const WebauthnKey = queryInterface.define("webauthnKey", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    userId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "users",
        key: "id",
      },
    },
    counter: {
      type: Sequelize.FLOAT,
    },
    publicKey: {
      type: Sequelize.STRING(2000),
    },
    credId: {
      type: Sequelize.STRING,
    },
  })

  WebauthnKey.associate = function(models) {
    WebauthnKey.belongsTo(models.User)
    models.User.hasMany(WebauthnKey)
  }
  return WebauthnKey
}
