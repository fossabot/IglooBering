module.exports = {
  up: async (queryInterface, Sequelize) => {
    const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
    const ValueVisibility = Sequelize.ENUM("VISIBLE", "HIDDEN", "INVISIBLE")
    const TileSize = Sequelize.ENUM("NORMAL", "WIDE", "TALL", "LARGE")
    const Role = Sequelize.ENUM("ADMIN", "EDITOR", "SPECTATOR")
    const PaymentPlan = Sequelize.ENUM("FREE", "PAYING")
    const selfId = {
      id: {
        type: Sequelize.UUID,
        defaultValue: Sequelize.UUIDV4,
        primaryKey: true,
      },
    }
    const otherId = (fieldName, model, allowNull = false) => ({
      [fieldName]: {
        type: Sequelize.UUID,
        allowNull,
        references: {
          model,
          key: "id",
          deferrable: Sequelize.Deferrable.INITIALLY_IMMEDIATE,
        },
      },
    })

    const createdAt = {
      createdAt: {
        type: Sequelize.DATE,
      },
    }
    const updatedAt = {
      updatedAt: {
        type: Sequelize.DATE,
      },
    }

    await queryInterface.createTable("users", {
      ...selfId,
      ...createdAt,
      ...updatedAt,
      email: {
        type: Sequelize.STRING,
        allowNull: false,
        validate: {
          isEmail: true,
        },
      },
      password: {
        type: Sequelize.STRING,
        allowNull: false,
      },
      twoFactorSecret: {
        type: Sequelize.STRING,
      },
      quietMode: {
        type: Sequelize.BOOLEAN,
      },
      devMode: {
        type: Sequelize.BOOLEAN,
      },
      stripeCustomerId: {
        type: Sequelize.STRING,
      },
      monthUsage: {
        type: Sequelize.INTEGER,
      },
      paymentPlan: {
        type: PaymentPlan,
      },
      usageCap: {
        type: Sequelize.INTEGER,
      },
      name: {
        type: Sequelize.STRING,
      },
      profileIcon: {
        type: Sequelize.STRING,
      },
      profileIconColor: {
        type: Sequelize.STRING,
      },
      emailIsVerified: {
        type: Sequelize.BOOLEAN,
      },
      settings_language: {
        type: Sequelize.STRING,
      },
      settings_timeZone: {
        type: Sequelize.STRING,
      },
      settings_lengthAndMass: {
        type: Sequelize.STRING,
      },
      settings_temperature: {
        type: Sequelize.STRING,
      },
      settings_dateFormat: {
        type: Sequelize.STRING,
      },
      settings_timeFormat: {
        type: Sequelize.STRING,
      },
    })

    await queryInterface.createTable("environments", {
      ...selfId,
      ...otherId("ownerId", "users", false),
      ...createdAt,
      ...updatedAt,
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

    await queryInterface.createTable("permanentTokens", {
      ...selfId,
      ...otherId("userId", "users"),
      ...createdAt,
      ...updatedAt,
      name: {
        type: Sequelize.STRING,
        allowNull: false,
      },
      lastUsed: {
        type: Sequelize.DATE,
      },
    })

    await queryInterface.createTable("webPushNotifications", {
      ...selfId,
      ...otherId("userId", "users"),
      ...createdAt,
      ...updatedAt,
      endpoint: {
        type: Sequelize.STRING(2000),
      },
      expirationTime: {
        type: Sequelize.DATE,
      },
      p256dh: {
        type: Sequelize.STRING,
      },
      auth: {
        type: Sequelize.STRING,
      },
    })

    await queryInterface.createTable("devices", {
      ...selfId,
      ...otherId("environmentId", "environments", false),
      ...createdAt,
      ...updatedAt,
      deviceType: {
        type: Sequelize.STRING,
      },
      name: {
        type: Sequelize.STRING,
      },
      index: {
        type: Sequelize.INTEGER,
      },
      online: {
        type: Sequelize.BOOLEAN,
      },
      batteryStatus: {
        type: Sequelize.FLOAT,
      },
      batteryCharging: {
        type: Sequelize.BOOLEAN,
      },
      signalStatus: {
        type: Sequelize.FLOAT,
      },
      firmware: {
        type: Sequelize.STRING,
      },
      muted: {
        type: Sequelize.BOOLEAN,
      },
    })

    await queryInterface.createTable("notifications", {
      ...selfId,
      ...otherId("userId", "users", false),
      ...otherId("environmentId", "environments", false),
      ...otherId("deviceId", "devices", false),
      ...createdAt,
      ...updatedAt,
      content: {
        type: Sequelize.STRING,
      },
      date: {
        type: Sequelize.DATE,
        defaultValue: Sequelize.NOW,
      },
      visualized: {
        type: Sequelize.ARRAY(Sequelize.UUID),
      },
    })

    const Value = {
      ...selfId,
      ...otherId("environmentId", "environments", false),
      ...otherId("deviceId", "devices", false),
      ...createdAt,
      ...updatedAt,
      valueDetails: {
        type: Sequelize.STRING,
      },
      permission: {
        type: ValuePermission,
      },
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

    await queryInterface.createTable("booleanValues", {
      ...Value,
      value: {
        type: Sequelize.BOOLEAN,
        allowNull: false,
      },
    })
    await queryInterface.createTable("floatValues", {
      ...Value,
      value: {
        type: Sequelize.FLOAT,
        allowNull: false,
      },
      precision: {
        type: Sequelize.FLOAT,
      },
      boundaries: {
        type: Sequelize.ARRAY(Sequelize.FLOAT),
      },
    })
    await queryInterface.createTable("stringValues", {
      ...Value,
      value: {
        type: Sequelize.TEXT,
        allowNull: false,
      },
      maxChars: {
        type: Sequelize.INTEGER,
      },
      allowedValues: {
        type: Sequelize.ARRAY(Sequelize.STRING),
      },
    })
    await queryInterface.createTable("plotValues", {
      ...Value,
      precision: {
        type: Sequelize.FLOAT,
      },
      threshold: {
        type: Sequelize.FLOAT,
      },
      boundaries: {
        type: Sequelize.ARRAY(Sequelize.FLOAT),
      },
    })
    await queryInterface.createTable("plotNodes", {
      ...selfId,
      ...otherId("userId", "users"),
      ...otherId("deviceId", "devices"),
      ...otherId("plotId", "plotValues"),
      ...createdAt,
      ...updatedAt,
      value: {
        type: Sequelize.FLOAT,
        allowNull: false,
      },
      timestamp: {
        type: Sequelize.DATE,
        allowNull: false,
      },
    })
    await queryInterface.createTable("stringPlotValues", {
      ...Value,
      allowedValues: {
        type: Sequelize.ARRAY(Sequelize.STRING),
      },
    })
    await queryInterface.createTable("stringPlotNodes", {
      ...selfId,
      ...otherId("userId", "users"),
      ...otherId("deviceId", "devices"),
      ...otherId("plotId", "stringPlotValues"),
      ...createdAt,
      ...updatedAt,
      value: {
        type: Sequelize.STRING,
        allowNull: false,
      },
      timestamp: {
        type: Sequelize.DATE,
        allowNull: false,
      },
    })
    await queryInterface.createTable("mapValues", {
      ...Value,
      latitude: {
        type: Sequelize.FLOAT,
      },
      longitude: {
        type: Sequelize.FLOAT,
      },
      height: {
        type: Sequelize.FLOAT,
      },
      value: {
        type: Sequelize.TEXT,
      },
    })

    await queryInterface.createTable("pendingEnvironmentShares", {
      ...selfId,
      ...otherId("senderId", "users"),
      ...otherId("receiverId", "users"),
      ...otherId("environmentId", "environments"),
      ...createdAt,
      ...updatedAt,
      role: {
        type: Role,
      },
    })

    await queryInterface.createTable("pendingOwnerChanges", {
      ...selfId,
      ...otherId("senderId", "users"),
      ...otherId("receiverId", "users"),
      ...otherId("environmentId", "environments"),
      ...createdAt,
      ...updatedAt,
    })

    await queryInterface.createTable("EnvironmentAdmins", {
      ...selfId,
      ...createdAt,
      ...updatedAt,
      ...otherId("environmentId", "environments"),
      ...otherId("userId", "users"),
    })
    await queryInterface.createTable("EnvironmentEditors", {
      ...selfId,
      ...createdAt,
      ...updatedAt,
      ...otherId("environmentId", "environments"),
      ...otherId("userId", "users"),
    })
    await queryInterface.createTable("EnvironmentSpectators", {
      ...selfId,
      ...createdAt,
      ...updatedAt,
      ...otherId("environmentId", "environments"),
      ...otherId("userId", "users"),
    })
  },
  down: async (queryInterface, Sequelize) => {
    await queryInterface.dropTable("users")
    await queryInterface.dropTable("environments")
    await queryInterface.dropTable("devices")
    await queryInterface.dropTable("permanentTokens")
    await queryInterface.dropTable("webPushSubscriptions")
    await queryInterface.dropTable("notifications")
    await queryInterface.dropTable("floatValues")
    await queryInterface.dropTable("stringValues")
    await queryInterface.dropTable("booleanValues")
    await queryInterface.dropTable("mapValues")
    await queryInterface.dropTable("plotValues")
    await queryInterface.dropTable("stringPlotValues")
    await queryInterface.dropTable("plotNodes")
    await queryInterface.dropTable("stringPlotNodes")
    await queryInterface.dropTable("pendingEnvironmentShares")
    await queryInterface.dropTable("pendingOwnerChanges")
    await queryInterface.dropTable("EnvironmentAdmins")
    await queryInterface.dropTable("EnvironmentEditors")
    await queryInterface.dropTable("EnvironmentSpectators")
  },
}
