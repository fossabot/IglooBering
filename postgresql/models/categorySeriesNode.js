module.exports = (queryInterface, Sequelize) => {
  const CategorySeriesNode = queryInterface.define("categorySeriesNode", {
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
        model: "categorySeriesValues",
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

  CategorySeriesNode.associate = function(models) {
    CategorySeriesNode.belongsTo(models.CategorySeriesValue, { as: "series" })
    models.CategorySeriesValue.hasMany(CategorySeriesNode, {
      foreignKey: "seriesId",
    })
  }
  return CategorySeriesNode
}
