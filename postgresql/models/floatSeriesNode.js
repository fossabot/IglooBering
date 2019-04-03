module.exports = (queryInterface, Sequelize) => {
  const FloatSeriesNode = queryInterface.define("floatSeriesNode", {
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
    deviceId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "devices",
        key: "id",
      },
    },
    seriesId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "floatSeriesValues",
        key: "id",
      },
    },
    value: {
      type: Sequelize.FLOAT,
      allowNull: false,
    },
    timestamp: {
      type: Sequelize.DATE,
      allowNull: false,
    },
  })

  FloatSeriesNode.associate = function(models) {
    FloatSeriesNode.belongsTo(models.FloatSeriesValue, { as: "series" })
    models.FloatSeriesValue.hasMany(FloatSeriesNode, { foreignKey: "seriesId" })
  }
  return FloatSeriesNode
}
