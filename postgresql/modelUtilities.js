const ValueModel = (Sequelize, hasPermission = true) => {
  const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
  const ValueVisibility = Sequelize.ENUM("VISIBLE", "HIDDEN", "INVISIBLE")
  const cardSize = Sequelize.ENUM("NORMAL", "WIDE", "TALL", "LARGE")

  return {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    deviceId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "devices",
        key: "id",
      },
    },
    ...(hasPermission
      ? {
          permission: {
            type: ValuePermission,
          },
        }
      : {}),
    visibility: {
      type: ValueVisibility,
    },
    cardSize: {
      type: cardSize,
    },
    name: {
      type: Sequelize.STRING,
    },
    index: {
      type: Sequelize.INTEGER,
    },
  }
}

module.exports = {
  ValueModel,
}
