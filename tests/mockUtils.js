module.exports = () => {
  const mockBoardData = [
    {
      id: "mockBoardId",
      userId: "mockUserId",
      customName: "fake customName",
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
      muted: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      hasOwnBoard: ({ id }) => ["mockBoardId"].includes(id),
      hasAdminBoard: () => false,
      hasEditorBoard: () => false,
      hasSpectatorBoard: () => false
    },
    {
      id: "mockUserId2",
      muted: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      hasOwnBoard: () => false,
      hasAdminBoard: () => false,
      hasEditorBoard: () => false,
      hasSpectatorBoard: () => false
    },
    {
      id: "mockUserId3",
      muted: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      hasOwnBoard: () => false,
      hasAdminBoard: ({ id }) => ["mockBoardId"].includes(id),
      hasEditorBoard: () => false,
      hasSpectatorBoard: () => false
    },
    {
      id: "mockUserId4",
      muted: false,
      createdAt: "2018-11-19T17:42:05.045Z",
      updateAt: "2018-11-19T17:42:05.045Z",
      hasOwnBoard: () => false,
      hasAdminBoard: () => false,
      hasEditorBoard: () => false,
      hasSpectatorBoard: () => false
    }
  ];

  const mockDeviceData = [
    {
      id: "mockDeviceId",
      boardId: "mockBoardId",
      deviceType: "mockDeviceType",
      customName: "mockCustomName",
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
      boardId: "mockBoardId",
      content: "mockContent",
      date: "2018-11-27T22:09:44.183Z",
      visualized: []
    }
  ];

  const mockPendingBoardShareData = [
    {
      id: "mockPendingBoardShareId",
      senderId: "mockUserId",
      receiverId: "mockUserId2",
      boardId: "mockBoardId",
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
    create: mockCreate(mockData),
    update: mockUpdate(mockData)
  });

  return {
    MockedBoard: MockedModel(mockBoardData),
    mockBoardData,
    MockedUser: MockedModel(mockUserData),
    mockUserData,
    MockedDevice: MockedModel(mockDeviceData),
    mockDeviceData,
    MockedNotification: MockedModel(mockNotificationData),
    mockNotificationData,
    MockedPendingBoardShare: MockedModel(mockPendingBoardShareData),
    mockPendingBoardShareData
  };
};
