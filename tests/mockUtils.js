import { Op } from "sequelize";

const arrayContains = (a, b) => a.filter(x => b.indexOf(x) !== -1).length !== 0;

module.exports = () => {
  const mockEnvironmentData = [
    {
      id: "mockEnvironmentId",
      ownerId: "mockUserId",
      name: "fake name",
      picture: "fake picture",
      index: 0,
      muted: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z"
    },
    {
      id: "mockEnvironmentId2",
      ownerId: "mockUserId2",
      name: "fake name",
      picture: "fake picture",
      index: 0,
      muted: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z"
    }
  ];

  const mockUserData = [
    {
      id: "mockUserId",
      quietMode: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      email: "mock@email.com",
      devMode: true,
      monthUsage: 10,
      paymentPlan: "FREE",
      usageCap: null,
      name: "MockName",
      profileIcon: "mockIcon",
      profileIconColor: "#00f",
      emailIsVerified: true,
      settings_language: "it-IT",
      settings_lengthAndMass: "SI",
      settings_temperature: "CELSIUS",
      settings_dateFormat: "DMY",
      settings_timeFormat: "H24",
      hasOwnEnvironment: ({ id }) => ["mockEnvironmentId"].includes(id),
      hasAdminEnvironment: () => false,
      hasEditorEnvironment: () => false,
      hasSpectatorEnvironment: () => false
    },
    {
      id: "mockUserId2",
      quietMode: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      email: "mock2@email.com",
      devMode: true,
      monthUsage: 10,
      paymentPlan: "FREE",
      usageCap: null,
      name: "MockName",
      profileIcon: "mockIcon",
      profileIconColor: "#00f",
      emailIsVerified: true,
      settings_language: "it-IT",
      settings_lengthAndMass: "SI",
      settings_temperature: "CELSIUS",
      settings_dateFormat: "DMY",
      settings_timeFormat: "H24",
      hasOwnEnvironment: () => false,
      hasAdminEnvironment: () => false,
      hasEditorEnvironment: () => false,
      hasSpectatorEnvironment: () => false
    },
    {
      id: "mockUserId3",
      quietMode: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      email: "mock3@email.com",
      devMode: true,
      monthUsage: 10,
      paymentPlan: "FREE",
      usageCap: null,
      name: "MockName",
      profileIcon: "mockIcon",
      profileIconColor: "#00f",
      emailIsVerified: true,
      settings_language: "it-IT",
      settings_lengthAndMass: "SI",
      settings_temperature: "CELSIUS",
      settings_dateFormat: "DMY",
      settings_timeFormat: "H24",
      hasOwnEnvironment: () => false,
      hasAdminEnvironment: ({ id }) => ["mockEnvironmentId"].includes(id),
      hasEditorEnvironment: () => false,
      hasSpectatorEnvironment: () => false
    },
    {
      id: "mockUserId4",
      quietMode: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      email: "mock4@email.com",
      devMode: true,
      monthUsage: 10,
      paymentPlan: "FREE",
      usageCap: null,
      name: "MockName",
      profileIcon: "mockIcon",
      profileIconColor: "#00f",
      emailIsVerified: true,
      settings_language: "it-IT",
      settings_lengthAndMass: "SI",
      settings_temperature: "CELSIUS",
      settings_dateFormat: "DMY",
      settings_timeFormat: "H24",
      hasOwnEnvironment: () => false,
      hasAdminEnvironment: () => false,
      hasEditorEnvironment: () => false,
      hasSpectatorEnvironment: () => false
    }
  ];

  const mockEnvironmentAdminData = [
    { userId: "mockUserId3", environmentId: "mockEnvironmentId", id: "mockEnvironmentAdminId" }
  ];
  const mockEnvironmentEditorData = [];
  const mockEnvironmentSpectatorData = [];

  const mockDeviceData = [
    {
      id: "mockDeviceId",
      environmentId: "mockEnvironmentId",
      deviceType: "mockDeviceType",
      name: "mockName",
      index: 0,
      online: null,
      batteryStatus: 30,
      batteryCharging: true,
      signalStatus: null,
      firmware: null,
      muted: false
    }
  ];

  const mockNotificationData = [
    {
      id: "mockNotificationId",
      userId: "mockUserId",
      deviceId: "mockDeviceId",
      environmentId: "mockEnvironmentId",
      content: "mockContent",
      date: "2018-11-27T22:09:44.183Z",
      notRead: ["mockUserId"]
    },
    {
      id: "mockNotificationId2",
      userId: "mockUserId",
      deviceId: "mockDeviceId",
      environmentId: "mockEnvironmentId",
      content: "mockContent2",
      date: "2018-11-27T22:09:44.183Z",
      notRead: ["mockUserId"]
    }
  ];

  const mockPendingEnvironmentShareData = [
    {
      id: "mockPendingEnvironmentShareId",
      senderId: "mockUserId",
      receiverId: "mockUserId2",
      environmentId: "mockEnvironmentId",
      role: "ADMIN"
    }
  ];

  const mockPendingOwnerChangeData = [
    {
      id: "mockPendingOwnerChangeId",
      senderId: "mockUserId",
      receiverId: "mockUserId2",
      environmentId: "mockEnvironmentId"
    }
  ];
  const mockPermanentTokenData = [
    {
      id: "mockPermanentTokenId",
      userId: "mockUserId",
      name: "mockPermanentTokenName",
      lastUsed: "2018-11-19T17:42:05.045Z"
    }
  ];
  const mockFloatValueData = [
    {
      id: "mockFloatValueId",
      environmentId: "mockEnvironmentId",
      deviceId: "mockDeviceId",
      visibility: "VISIBLE",
      cardSize: "NORMAL",
      name: "mockName",
      index: 0,
      permission: "READ_WRITE",
      value: 5,
      precision: 0.1,
      min: 1,
      max: 7,
      unitOfMeasurement: "C"
    }
  ];
  const mockStringValueData = [
    {
      id: "mockStringValueId",
      environmentId: "mockEnvironmentId",
      deviceId: "mockDeviceId",
      visibility: "VISIBLE",
      cardSize: "NORMAL",
      name: "mockName",
      index: 0,
      permission: "READ_WRITE",
      value: "test",
      maxChars: null,
      allowedValues: ["test", "test2"]
    }
  ];
  const mockBooleanValueData = [
    {
      id: "mockBooleanValueId",
      environmentId: "mockEnvironmentId",
      deviceId: "mockDeviceId",
      visibility: "VISIBLE",
      cardSize: "NORMAL",
      name: "mockName",
      index: 0,
      permission: "READ_WRITE",
      value: true
    }
  ];
  const mockMapValueData = [
    {
      id: "mockMapValueId",
      environmentId: "mockEnvironmentId",
      deviceId: "mockDeviceId",
      visibility: "VISIBLE",
      cardSize: "NORMAL",
      name: "mockName",
      index: 0,
      permission: "READ_WRITE",
      value: "",
      latitude: 4,
      longitude: 2,
      height: 2
    }
  ];
  const mockPlotValueData = [
    {
      id: "mockPlotValueId",
      environmentId: "mockEnvironmentId",
      deviceId: "mockDeviceId",
      visibility: "VISIBLE",
      cardSize: "NORMAL",
      name: "mockName",
      index: 0,
      precision: 1,
      threshold: 90,
      min: 0,
      max: 100,
      unitOfMeasurement: "C"
    }
  ];
  const mockCategoryPlotValueData = [
    {
      id: "mockCategoryPlotValueId",
      environmentId: "mockEnvironmentId",
      deviceId: "mockDeviceId",
      visibility: "VISIBLE",
      cardSize: "NORMAL",
      name: "mockName",
      index: 0,
      allowedValues: ["test", "test2"]
    }
  ];
  const mockPlotNodeData = [
    {
      id: "mockPlotNodeId",
      userId: "mockUserId",
      deviceId: "mockDeviceId",
      plotId: "mockPlotValueId",
      value: 1,
      timestamp: "2018-11-19T17:42:05.045Z"
    }
  ];
  const mockCategoryPlotNodeData = [
    {
      id: "mockCategoryPlotNodeId",
      userId: "mockUserId",
      deviceId: "mockDeviceId",
      plotId: "mockCategoryPlotValueId",
      value: 1,
      timestamp: "2018-11-19T17:42:05.045Z"
    }
  ];

  const queryAndItemMatch = whereQuery => item => {
    let isMatching = true;
    for (const key in whereQuery) {
      if (
        whereQuery[key] === item[key] ||
        (whereQuery[key][Op.in] !== undefined &&
          whereQuery[key][Op.in].indexOf(item[key]) !== -1) ||
        (whereQuery[key][Op.contains] !== undefined &&
          arrayContains(item[key], whereQuery[key][Op.contains]))
      ) {
        continue;
      } else {
        isMatching = false;
        break;
      }
    }

    return isMatching;
  };

  const mockFindAll = database => async ({ where: whereQuery }) =>
    database.filter(queryAndItemMatch(whereQuery)).map(item => ({
      ...item,
      dataValue: item,
      update: async newValues => {
        database = database.map(el => (el.id === item.id ? { ...item, ...newValues } : el));

        return { ...item, ...newValues };
      }
    }));

  const mockFind = database => {
    const hydratedMockFindAll = mockFindAll(database);

    return async (...args) => {
      const allResults = await hydratedMockFindAll(...args);
      return allResults.length === 0 ? null : allResults[0];
    };
  };

  const mockCount = database => {
    const hydratedMockFindAll = mockFindAll(database);

    return async (...args) => {
      const allResults = await hydratedMockFindAll(...args);
      return allResults.length;
    };
  };

  const mockCreate = database => async newInstance => {
    database.push(newInstance);

    return newInstance;
  };

  const mockUpdate = database => async (newValues, { where: whereQuery }) => {
    let count = 0;
    const itemMatch = queryAndItemMatch(whereQuery);
    database = database.map(el => (itemMatch(el) ? (count++, { ...el, ...newValues }) : el));

    return count;
  };

  const MockedModel = mockData => ({
    find: mockFind(mockData),
    findAll: mockFindAll(mockData),
    count: mockCount(mockData),
    create: mockCreate(mockData),
    update: mockUpdate(mockData)
  });

  const mocks = {
    MockedEnvironment: MockedModel(mockEnvironmentData),
    mockEnvironmentData,
    MockedEnvironmentAdmin: MockedModel(mockEnvironmentAdminData),
    mockEnvironmentAdminData,
    MockedEnvironmentEditor: MockedModel(mockEnvironmentEditorData),
    mockEnvironmentEditorData,
    MockedEnvironmentSpectator: MockedModel(mockEnvironmentSpectatorData),
    mockEnvironmentSpectatorData,
    MockedUser: MockedModel(mockUserData),
    mockUserData,
    MockedDevice: MockedModel(mockDeviceData),
    mockDeviceData,
    MockedNotification: MockedModel(mockNotificationData),
    mockNotificationData,
    MockedPendingEnvironmentShare: MockedModel(mockPendingEnvironmentShareData),
    mockPendingEnvironmentShareData,
    MockedPendingOwnerChange: MockedModel(mockPendingOwnerChangeData),
    mockPendingOwnerChangeData,
    MockedPermanentToken: MockedModel(mockPermanentTokenData),
    mockPermanentTokenData,
    MockedFloatValue: MockedModel(mockFloatValueData),
    mockFloatValueData,
    MockedStringValue: MockedModel(mockStringValueData),
    mockStringValueData,
    MockedBooleanValue: MockedModel(mockBooleanValueData),
    mockBooleanValueData,
    MockedMapValue: MockedModel(mockMapValueData),
    mockMapValueData,
    MockedPlotValue: MockedModel(mockPlotValueData),
    mockPlotValueData,
    MockedCategoryPlotValue: MockedModel(mockCategoryPlotValueData),
    mockCategoryPlotValueData,
    MockedPlotNode: MockedModel(mockPlotNodeData),
    mockPlotNodeData,
    MockedCategoryPlotNode: MockedModel(mockCategoryPlotNodeData),
    mockCategoryPlotNodeData
  };

  const mockDataLoader = MockedModel => ({
    load: id =>
      MockedModel.find({
        where: { id }
      })
  });

  const mockRolesDataLoader = MockedModel => ({
    load: tuple =>
      MockedModel.find({
        where: { userId: tuple.split("|")[0], environmentId: tuple.split("|")[1] }
      })
  });

  const mockContext = {
    dataLoaders: {
      userLoaderById: mockDataLoader(mocks.MockedUser),
      environmentLoaderById: mockDataLoader(mocks.MockedEnvironment),
      deviceLoaderById: mockDataLoader(mocks.MockedDevice),
      notificationLoaderById: mockDataLoader(mocks.MockedNotification),
      floatValueLoaderById: mockDataLoader(mocks.MockedFloatValue),
      stringValueLoaderById: mockDataLoader(mocks.MockedStringValue),
      booleanValueLoaderById: mockDataLoader(mocks.MockedBooleanValue),
      mapValueLoaderById: mockDataLoader(mocks.MockedMapValue),
      plotValueLoaderById: mockDataLoader(mocks.MockedPlotValue),
      plotNodeLoaderById: mockDataLoader(mocks.MockedPlotNode),
      categoryPlotNodeLoaderById: mockDataLoader(mocks.MockedCategoryPlotNode),
      categoryPlotValueLoaderById: mockDataLoader(mocks.MockedCategoryPlotValue),
      permanentTokenLoaderById: mockDataLoader(mocks.MockedPermanentToken),
      pendingEnvironmentShareLoaderById: mockDataLoader(mocks.MockedPendingEnvironmentShare),
      pendingOwnerChangeLoaderById: mockDataLoader(mocks.MockedPendingOwnerChange),
      environmentAdminLoaderByEnvironmentAndUserId: mockRolesDataLoader(
        mocks.MockedEnvironmentAdmin
      ),
      editorAdminLoaderByEnvironmentAndUserId: mockRolesDataLoader(mocks.MockedEnvironmentEditor),
      spectatorAdminLoaderByEnvironmentAndUserId: mockRolesDataLoader(
        mocks.MockedEnvironmentSpectator
      )
    },
    billingUpdater: { update: () => {} }
  };

  return { ...mocks, mockContext };
};
