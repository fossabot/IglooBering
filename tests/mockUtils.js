module.exports = () => {
  const mockBoardData = [
    {
      id: "mockBoardId",
      userId: "mockUserId",
      customName: "fake customName",
      avatar: "fake avatar",
      index: 0,
      quietMode: false,
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
      hasOwnBoard: () => true,
      hasAdminBoard: () => true,
      hasEditorBoard: () => true,
      hasSpectatorBoard: () => true
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
      quietMode: false
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
    mockNotificationData
  };
};
