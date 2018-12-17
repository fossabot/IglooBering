module.exports = (queryInterface, Sequelize) => {
  const PendingOwnerChange = queryInterface.define("pendingOwnerChange", {
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
    environmentId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "environments",
        key: "id",
      },
    },
  })
  PendingOwnerChange.associate = function(models) {}
  return PendingOwnerChange
}
