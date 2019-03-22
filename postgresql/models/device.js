module.exports = (queryInterface, Sequelize) => {
  const Device = queryInterface.define("device", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    environmentId: {
      type: Sequelize.UUID,
      references: {
        model: "environments",
        key: "id",
      },
    },
    producerId: {
      type: Sequelize.UUID,
      references: {
        model: "users",
        key: "id",
      },
    },
    starred: {
      type: Sequelize.ARRAY(Sequelize.UUID),
    },
    deviceType: {
      type: Sequelize.STRING,
    },
    name: {
      type: Sequelize.STRING,
    },
    index: {
      type: Sequelize.INTEGER,
    },
    online: {
      type: Sequelize.BOOLEAN,
    },
    batteryStatus: {
      type: Sequelize.FLOAT,
    },
    batteryCharging: {
      type: Sequelize.BOOLEAN,
    },
    signalStatus: {
      type: Sequelize.FLOAT,
    },
    firmware: {
      type: Sequelize.STRING,
    },
    muted: {
      type: Sequelize.BOOLEAN,
    },
    storageUsed: {
      type: Sequelize.INTEGER,
    },
  })
  Device.associate = function(models) {
    models.Environment.hasMany(Device)
  }
  return Device
}
