module.exports = (queryInterface, Sequelize) => {
  const Environment = queryInterface.define("environment", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    name: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    avatar: {
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
      through: "EnvironmentAdmins",
    })
    models.User.AdminEnvironments = models.User.belongsToMany(Environment, {
      through: "EnvironmentAdmins",
      as: "AdminEnvironments",
    })

    Environment.Editors = "EditorEnvironments"
    Environment.belongsToMany(models.User, {
      as: "editor",
      through: "EnvironmentEditors",
    })
    models.User.EditorEnvironments = models.User.belongsToMany(Environment, {
      as: "EditorEnvironments",
      through: "EnvironmentEditors",
    })

    Environment.Spectators = "SpectatorEnvironments"
    Environment.belongsToMany(models.User, {
      as: "spectator",
      through: "EnvironmentSpectators",
    })
    models.User.SpectatorEnvironments = models.User.belongsToMany(Environment, {
      as: "SpectatorEnvironments",
      through: "EnvironmentSpectators",
    })
  }
  return Environment
}
