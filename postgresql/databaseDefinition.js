const Sequelize = require("sequelize")

const databaseDefinition = sequelize => {
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

  const User = sequelize.define("user", {
    ...selfId,
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

  const Environment = sequelize.define("environment", {
    ...selfId,
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

  const PermanentToken = sequelize.define("permanentToken", {
    ...selfId,
    ...otherId("userId", User),
    name: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    lastUsed: {
      type: Sequelize.DATE,
    },
  })

  const WebPushSubscription = sequelize.define("webPushNotification", {
    ...selfId,
    ...otherId("userId", User),
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

  const Device = sequelize.define("device", {
    ...selfId,
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

  const Notification = sequelize.define("notification", {
    ...selfId,
    ...otherId("userId", User),
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

  const BooleanValue = sequelize.define("booleanValue", {
    ...Value,
    value: {
      type: Sequelize.BOOLEAN,
      allowNull: false,
    },
  })
  const FloatValue = sequelize.define("floatValue", {
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
  const StringValue = sequelize.define("stringValue", {
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
  const PlotValue = sequelize.define("plotValue", {
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
  const PlotNode = sequelize.define("plotNode", {
    ...selfId,
    ...otherId("userId", User),
    ...otherId("deviceId", Device),
    ...otherId("plotId", PlotValue),
    value: {
      type: Sequelize.FLOAT,
      allowNull: false,
    },
    timestamp: {
      type: Sequelize.DATE,
      allowNull: false,
    },
  })
  const StringPlotValue = sequelize.define("stringPlotValue", {
    ...Value,
    allowedValues: {
      type: Sequelize.ARRAY(Sequelize.STRING),
    },
  })
  const StringPlotNode = sequelize.define("stringPlotNode", {
    ...selfId,
    ...otherId("userId", User),
    ...otherId("deviceId", Device),
    ...otherId("plotId", StringPlotValue),
    value: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    timestamp: {
      type: Sequelize.DATE,
      allowNull: false,
    },
  })
  const MapValue = sequelize.define("mapValue", {
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

  const PendingEnvironmentShare = sequelize.define("pendingEnvironmentShare", {
    ...selfId,
    ...otherId("senderId", User),
    ...otherId("receiverId", User),
    ...otherId("environmentId", Environment),
    role: {
      type: Role,
    },
  })

  const PendingOwnerChange = sequelize.define("pendingOwnerChange", {
    ...selfId,
    ...otherId("senderId", User),
    ...otherId("receiverId", User),
    ...otherId("environmentId", Environment),
  })

  Environment.hasMany(Device)
  Device.belongsTo(Environment)

  Device.hasMany(Notification)
  Notification.belongsTo(Device)

  Environment.hasMany(Notification)
  Notification.belongsTo(Environment)

  PlotValue.hasMany(PlotNode)
  PlotNode.belongsTo(PlotValue, { as: "plot" })

  StringPlotValue.hasMany(StringPlotNode)
  StringPlotNode.belongsTo(StringPlotValue, { as: "plot" })

  const values = [
    BooleanValue,
    FloatValue,
    StringValue,
    MapValue,
    PlotValue,
    StringPlotValue,
  ]
  values.forEach(Value => {
    Device.hasMany(Value)
    Environment.hasMany(Value)
  })

  Environment.Owner = "OwnEnvironments"
  Environment.belongsTo(User, { as: "owner" })
  User.OwnEnvironments = User.hasMany(Environment, {
    as: "OwnEnvironments",
  })

  const associations = []
  const joinTables = {}

  const adminAssociation = sequelize.define("EnvironmentAdmins", {})
  associations.push(adminAssociation)
  joinTables.EnvironmentAdmins = adminAssociation
  Environment.Admins = "AdminEnvironments"
  Environment.belongsToMany(User, {
    as: "admin",
    through: "EnvironmentAdmins",
  })
  User.AdminEnvironments = User.belongsToMany(Environment, {
    through: "EnvironmentAdmins",
    as: "AdminEnvironments",
  })

  const editorAssociation = sequelize.define("EnvironmentEditors", {})
  joinTables.EnvironmentEditors = editorAssociation
  associations.push(editorAssociation)
  Environment.Editors = "EditorEnvironments"
  Environment.belongsToMany(User, {
    as: "editor",
    through: "EnvironmentEditors",
  })
  User.EditorEnvironments = User.belongsToMany(Environment, {
    as: "EditorEnvironments",
    through: "EnvironmentEditors",
  })

  const spectatorAssociation = sequelize.define("EnvironmentSpectators", {})
  joinTables.EnvironmentSpectators = spectatorAssociation
  associations.push(spectatorAssociation)
  Environment.Spectators = "SpectatorEnvironments"
  Environment.belongsToMany(User, {
    as: "spectator",
    through: "EnvironmentSpectators",
  })
  User.SpectatorEnvironments = User.belongsToMany(Environment, {
    as: "SpectatorEnvironments",
    through: "EnvironmentSpectators",
  })

  return {
    User,
    Environment,
    PermanentToken,
    Device,
    BooleanValue,
    FloatValue,
    StringValue,
    PlotValue,
    PlotNode,
    MapValue,
    Notification,
    WebPushSubscription,
    StringPlotValue,
    StringPlotNode,
    associations,
    joinTables,
    PendingEnvironmentShare,
    PendingOwnerChange,
  }
}

module.exports = databaseDefinition
