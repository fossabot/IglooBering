module.exports = (queryInterface, Sequelize) => {
  const StringPlotNode = queryInterface.define("stringPlotNode", {
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
    plotId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "stringPlotValues",
        key: "id",
      },
    },
    value: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    timestamp: {
      type: Sequelize.DATE,
      allowNull: false,
    },
  })

  StringPlotNode.associate = function(models) {
    StringPlotNode.belongsTo(models.StringPlotValue, { as: "plot" })
    models.StringPlotValue.hasMany(StringPlotNode, { foreignKey: "plotId" })
  }
  return StringPlotNode
}
