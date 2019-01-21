module.exports = (queryInterface, Sequelize) => {
  const UnclaimedDevice = queryInterface.define("unclaimedDevice", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    deviceType: {
      type: Sequelize.STRING,
    },
    firmware: {
      type: Sequelize.STRING,
    },
  })

  return UnclaimedDevice
}
