module.exports = (queryInterface, Sequelize) => {
  const WebPushNotification = queryInterface.define("webPushNotification", {
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
    endpoint: {
      type: Sequelize.STRING(2000),
    },
    expirationTime: {
      type: Sequelize.DATE,
    },
    p256dh: {
      type: Sequelize.STRING,
    },
    auth: {
      type: Sequelize.STRING,
    },
  })
  WebPushNotification.associate = function(models) {
    WebPushNotification.belongsTo(models.User)
    models.User.hasMany(WebPushNotification)
  }
  return WebPushNotification
}
