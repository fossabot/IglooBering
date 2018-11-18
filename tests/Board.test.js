import BoardResolverFactory from '../graphql/resolvers/Board'
import MocksGenerator from './mockUtils'

const {
  MockedBoard,
  MockedUser,
  mockBoardData,
  mockUserData,
} = MocksGenerator()

const BoardResolver = BoardResolverFactory({
  User: MockedUser,
  Board: MockedBoard,
})

describe('Board', () => {
  test('customName is resolved correctly', async (done) => {
    const customNameFound = await BoardResolver.customName(
      { id: 'mockBoardId' },
      {},
      { auth: { userId: 'mockUserId', tokenType: 'TEMPORARY' } },
    )

    expect(customNameFound).toBe(mockBoardData[0].customName)

    done()
  })

  test('updating a mockedValue works', async (done) => {
    const boardFound = await MockedBoard.find({ where: { id: 'mockBoardId' } })
    const boardReturned = await boardFound.update({ quietMode: true })
    const boardFound2 = await MockedBoard.find({ where: { id: 'mockBoardId' } })

    expect(boardFound2.quietMode).toBe(true)
    done()
  })

  test('create works', async (done) => {
    const boardsFound = await MockedBoard.findAll({ where: {} })
    await MockedBoard.create({
      id: 'mockedBoardId2',
      userId: 'mockedUserId',
      quietMode: true,
    })
    const boardsFound2 = await MockedBoard.findAll({ where: {} })

    expect(boardsFound2.length).toBe(boardsFound.length + 1)
    done()
  })

  test('update works', async (done) => {
    const boardsFound = await MockedBoard.findAll({ where: {} })
    const count = await MockedBoard.update({ quietMode: true }, {})

    expect(count).toBe(boardsFound.length)
    done()
  })
})
