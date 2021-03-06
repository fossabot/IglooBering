module.exports = (queryInterface, Sequelize) => {
  const Environment = queryInterface.define("environment", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    ownerId: {
      type: Sequelize.UUID,
      allowNull: false,
      references: {
        model: "users",
        key: "id",
      },
    },
    name: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    picture: {
      type: Sequelize.STRING,
    },
    index: {
      type: Sequelize.INTEGER,
    },
    muted: {
      type: Sequelize.BOOLEAN,
    },
  })
  Environment.associate = function(models) {
    Environment.Owner = "OwnEnvironments"
    Environment.belongsTo(models.User, { as: "owner" })
    models.User.OwnEnvironments = models.User.hasMany(Environment, {
      as: "OwnEnvironments",
      foreignKey: "ownerId",
    })

    Environment.Admins = "AdminEnvironments"
    Environment.belongsToMany(models.User, {
      as: "admin",
      through: "environmentAdmins",
    })
    models.User.AdminEnvironments = models.User.belongsToMany(Environment, {
      through: "environmentAdmins",
      as: "AdminEnvironments",
    })

    Environment.Editors = "EditorEnvironments"
    Environment.belongsToMany(models.User, {
      as: "editor",
      through: "environmentEditors",
    })
    models.User.EditorEnvironments = models.User.belongsToMany(Environment, {
      as: "EditorEnvironments",
      through: "environmentEditors",
    })

    Environment.Spectators = "SpectatorEnvironments"
    Environment.belongsToMany(models.User, {
      as: "spectator",
      through: "environmentSpectators",
    })
    models.User.SpectatorEnvironments = models.User.belongsToMany(Environment, {
      as: "SpectatorEnvironments",
      through: "environmentSpectators",
    })
  }
  return Environment
}
