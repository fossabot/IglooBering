module.exports = (queryInterface, Sequelize) => {
  const PlotNode = queryInterface.define("plotNode", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
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

  PlotNode.associate = function(models) {
    PlotNode.belongsTo(models.PlotValue, { as: "plot" })
    models.PlotValue.hasMany(PlotNode, { foreignKey: "plotId" })
  }
  return PlotNode
}
