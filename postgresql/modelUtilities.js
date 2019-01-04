const ValueModel = (Sequelize, hasPermission = true) => {
  const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
  const ValueVisibility = Sequelize.ENUM("VISIBLE", "HIDDEN", "INVISIBLE")
  const TileSize = Sequelize.ENUM("NORMAL", "WIDE", "TALL", "LARGE")

  return {
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
    deviceId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "devices",
        key: "id",
      },
    },
    unitOfMeasurement: {
      type: Sequelize.STRING,
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
    tileSize: {
      type: TileSize,
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
