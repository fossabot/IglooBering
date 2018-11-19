const Sequelize = require("sequelize")

const databaseDefinition = sequelize => {
  const ValuePermission = Sequelize.ENUM("READ_ONLY", "READ_WRITE")
  const ValueVisibility = Sequelize.ENUM("VISIBLE", "HIDDEN", "INVISIBLE")
  const TileSize = Sequelize.ENUM("NORMAL", "WIDE", "TALL", "LARGE")
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
    fullName: {
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

  const Board = sequelize.define("board", {
    ...selfId,
    customName: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    avatar: {
      type: Sequelize.STRING,
    },
    index: {
      type: Sequelize.INTEGER,
    },
    quietMode: {
      type: Sequelize.BOOLEAN,
    },
  })

  const PermanentToken = sequelize.define("permanentToken", {
    ...selfId,
    ...otherId("userId", User),
    customName: {
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
    customName: {
      type: Sequelize.STRING,
    },
    icon: {
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
    quietMode: {
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
    customName: {
      type: Sequelize.STRING,
    },
    index: {
      type: Sequelize.INTEGER,
    },
  }

  const BoolValue = sequelize.define("boolValue", {
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

  Board.hasMany(Device)
  Device.belongsTo(Board)

  Device.hasMany(Notification)
  Notification.belongsTo(Device)

  Board.hasMany(Notification)
  Notification.belongsTo(Board)

  PlotValue.hasMany(PlotNode)
  PlotNode.belongsTo(PlotValue, { as: "plot" })

  StringPlotValue.hasMany(StringPlotNode)
  StringPlotNode.belongsTo(StringPlotValue, { as: "plot" })

  const values = [
    BoolValue,
    FloatValue,
    StringValue,
    MapValue,
    PlotValue,
    StringPlotValue,
  ]
  values.forEach(Value => {
    Device.hasMany(Value)
    Board.hasMany(Value)
  })

  Board.Owner = "OwnBoards"
  Board.belongsTo(User, { as: "owner" })
  User.OwnBoards = User.hasMany(Board, {
    as: "OwnBoards",
  })

  const associations = []
  const joinTables = {}

  const adminAssociation = sequelize.define("BoardAdmins", {})
  associations.push(adminAssociation)
  joinTables.BoardAdmins = adminAssociation
  Board.Admins = "AdminBoards"
  Board.belongsToMany(User, {
    as: "admin",
    through: "BoardAdmins",
  })
  User.AdminBoards = User.belongsToMany(Board, {
    through: "BoardAdmins",
    as: "AdminBoards",
  })

  const editorAssociation = sequelize.define("BoardEditors", {})
  joinTables.BoardEditors = editorAssociation
  associations.push(editorAssociation)
  Board.Editors = "EditorBoards"
  Board.belongsToMany(User, {
    as: "editor",
    through: "BoardEditors",
  })
  User.EditorBoards = User.belongsToMany(Board, {
    as: "EditorBoards",
    through: "BoardEditors",
  })

  const spectatorAssociation = sequelize.define("BoardSpectators", {})
  joinTables.BoardSpectators = spectatorAssociation
  associations.push(spectatorAssociation)
  Board.Spectators = "SpectatorBoards"
  Board.belongsToMany(User, {
    as: "spectator",
    through: "BoardSpectators",
  })
  User.SpectatorBoards = User.belongsToMany(Board, {
    as: "SpectatorBoards",
    through: "BoardSpectators",
  })

  return {
    User,
    Board,
    PermanentToken,
    Device,
    BoolValue,
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
  }
}

module.exports = databaseDefinition
