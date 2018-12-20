module.exports = (queryInterface, Sequelize) => {
  const CategoryPlotNode = queryInterface.define("categoryPlotNode", {
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
        model: "categoryPlotValues",
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

  CategoryPlotNode.associate = function(models) {
    CategoryPlotNode.belongsTo(models.CategoryPlotValue, { as: "plot" })
    models.CategoryPlotValue.hasMany(CategoryPlotNode, { foreignKey: "plotId" })
  }
  return CategoryPlotNode
}
