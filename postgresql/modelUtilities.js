const ValueModel = (Sequelize, hasPermission = true) => {
  const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
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
    hidden: {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
    },
    private: {
      type: Sequelize.BOOLEAN,
      defaultValue: false,
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
