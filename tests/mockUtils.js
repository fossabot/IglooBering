module.exports = () => {
  const mockEnvironmentData = [
    {
      id: "mockEnvironmentId",
      userId: "mockUserId",
      name: "fake name",
      avatar: "fake avatar",
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
      visualized: []
    },
    {
      id: "mockNotificationId2",
      userId: "mockUserId",
      deviceId: "mockDeviceId",
      environmentId: "mockEnvironmentId",
      content: "mockContent2",
      date: "2018-11-27T22:09:44.183Z",
      visualized: []
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

  const queryAndItemMatch = whereQuery => item => {
    let isMatching = true;
    for (const key in whereQuery) {
      if (whereQuery[key] !== item[key]) {
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

  return {
    MockedEnvironment: MockedModel(mockEnvironmentData),
    mockEnvironmentData,
    MockedUser: MockedModel(mockUserData),
    mockUserData,
    MockedDevice: MockedModel(mockDeviceData),
    mockDeviceData,
    MockedNotification: MockedModel(mockNotificationData),
    mockNotificationData,
    MockedPendingEnvironmentShare: MockedModel(mockPendingEnvironmentShareData),
    mockPendingEnvironmentShareData
  };
};
