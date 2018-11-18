module.exports = () => {
  const mockBoardData = [
    {
      id: 'mockBoardId',
      userId: 'mockUserId',
      customName: 'fake customName',
      avatar: 'fake avatar',
      index: 0,
      quietMode: false,
    },
  ]

  const mockUserData = [
    {
      id: 'mockUserId',
      hasOwnBoard: () => true,
      hasAdminBoard: () => true,
      hasEditorBoard: () => true,
      hasSpectatorBoard: () => true,
    },
  ]

  const queryAndItemMatch = whereQuery => (item) => {
    let isMatching = true
    for (const key in whereQuery) {
      if (whereQuery[key] !== item[key]) {
        isMatching = false
        break
      }
    }

    return isMatching
  }

  const mockFindAll = database => async ({ where: whereQuery }) =>
    database.filter(queryAndItemMatch(whereQuery)).map(item => ({
      ...item,
      dataValue: item,
      update: async (newValues) => {
        database = database.map(el => (el.id === item.id ? { ...item, ...newValues } : el))

        return { ...item, ...newValues }
      },
    }))

  const mockFind = (database) => {
    const hydratedMockFindAll = mockFindAll(database)

    return async (...args) => {
      const allResults = await hydratedMockFindAll(...args)
      return allResults.length === 0 ? null : allResults[0]
    }
  }

  const mockCreate = database => async (newInstance) => {
    database.push(newInstance)

    return newInstance
  }

  const mockUpdate = database => async (newValues, { where: whereQuery }) => {
    let count = 0
    const itemMatch = queryAndItemMatch(whereQuery)
    database = database.map(el => (itemMatch(el) ? (count++, { ...el, ...newValues }) : el))

    return count
  }

  const MockedModel = mockData => ({
    find: mockFind(mockData),
    findAll: mockFindAll(mockData),
    create: mockCreate(mockData),
    update: mockUpdate(mockData),
  })

  return {
    MockedBoard: MockedModel(mockBoardData),
    mockBoardData,
    MockedUser: MockedModel(mockUserData),
    mockUserData,
  }
}
