module.exports = (queryInterface, Sequelize) => {
  const EnvironmentAdmin = queryInterface.define("environmentAdmin", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    environmentId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "environments",
        key: "id",
      },
    },
    userId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "users",
        key: "id",
      },
    },
  })
  EnvironmentAdmin.associate = function(models) {}
  return EnvironmentAdmin
}
