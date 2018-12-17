module.exports = (queryInterface, Sequelize) => {
  const Device = queryInterface.define("device", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
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
  })
  Device.associate = function(models) {
    models.Environment.hasMany(Device)
  }
  return Device
}
