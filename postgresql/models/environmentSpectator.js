module.exports = (queryInterface, Sequelize) => {
  const EnvironmentSpectator = queryInterface.define("environmentSpectator", {
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
  EnvironmentSpectator.associate = function(models) {}
  return EnvironmentSpectator
}
