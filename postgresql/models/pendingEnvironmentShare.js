module.exports = (queryInterface, Sequelize) => {
  const Role = Sequelize.ENUM("ADMIN", "EDITOR", "SPECTATOR")
  const PendingEnvironmentShare = queryInterface.define(
    "pendingEnvironmentShare",
    {
      id: {
        type: Sequelize.UUID,
        defaultValue: Sequelize.UUIDV4,
        primaryKey: true,
      },
      receiverId: {
        type: Sequelize.UUID,
        allowNull: false,
        references: {
          model: "users",
          key: "id",
        },
      },
      senderId: {
        type: Sequelize.UUID,
        allowNull: false,
        references: {
          model: "users",
          key: "id",
        },
      },
      role: {
        type: Role,
      },
      environmentId: {
        type: Sequelize.UUID,
        allowNull: false,
        references: {
          model: "environments",
          key: "id",
        },
      },
    }
  )
  PendingEnvironmentShare.associate = function(models) {}
  return PendingEnvironmentShare
}
