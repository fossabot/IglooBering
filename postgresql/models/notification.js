module.exports = (queryInterface, Sequelize) => {
  const Notification = queryInterface.define("notification", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    userId: {
      type: Sequelize.UUID,
      references: {
        model: "users", // name of Target model
        key: "id", // key in Target model that we're referencing
      },
      onUpdate: "CASCADE",
      onDelete: "SET NULL",
    },
    environmentId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "environments",
        key: "id",
      },
    },
    deviceId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "devices",
        key: "id",
      },
    },
    content: {
      type: Sequelize.STRING,
    },
    date: {
      type: Sequelize.DATE,
      defaultValue: Sequelize.NOW,
    },
    notRead: {
      type: Sequelize.ARRAY(Sequelize.UUID),
    },
  })

  Notification.associate = function(models) {
    models.Device.hasMany(Notification)
    Notification.belongsTo(models.Device)

    models.Environment.hasMany(Notification)
    Notification.belongsTo(models.Environment)
  }
  return Notification
}
