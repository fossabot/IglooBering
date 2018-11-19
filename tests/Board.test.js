import BoardResolverFactory from "../graphql/resolvers/Board";
import MocksGenerator from "./mockUtils";
import { testScalarProp, unauthenticatedShouldFail } from "./testUtils";

const { MockedBoard, MockedUser, mockBoardData, mockUserData } = MocksGenerator();

const BoardResolver = BoardResolverFactory({
  User: MockedUser,
  Board: MockedBoard
});

describe("Board", () => {
  const testBoardScalarProp = testScalarProp(
    BoardResolver,
    { id: "mockBoardId" },
    mockBoardData[0]
  );
  const testUnauthenticated = unauthenticatedShouldFail(BoardResolver, {
    id: "mockBoardId"
  });

  // not using a for loop because this syntax integrates better with the IDE
  test("customName is resolved correctly", testBoardScalarProp("customName"));
  test("avatar is resolved correctly", testBoardScalarProp("avatar"));
  test("index is resolved correctly", testBoardScalarProp("index"));
  test("createdAt is resolved correctly", testBoardScalarProp("createdAt"));
  test("updatedAt is resolved correctly", testBoardScalarProp("updatedAt"));

  test("quietMode is resolved correctly", async done => {
    const quietModeFound = await BoardResolver.quietMode(
      { id: "mockBoardId" },
      {},
      { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
    );

    const correctQuietMode = mockBoardData[0].quietMode || mockUserData[0].quietMode;
    expect(quietModeFound).toBe(correctQuietMode);

    done();
  });

  test("customName fails if unauthenticated", testUnauthenticated("customName"));
  test("avatar fails if unauthenticated", testUnauthenticated("avatar"));
  test("index fails if unauthenticated", testUnauthenticated("index"));
  test("createdAt fails if unauthenticated", testUnauthenticated("createdAt"));
  test("updatedAt fails if unauthenticated", testUnauthenticated("updatedAt"));
  test("quietMode fails if unauthenticated", testUnauthenticated("quietMode"));
  test("devices fails if unauthenticated", testUnauthenticated("devices"));
  test("owner fails if unauthenticated", testUnauthenticated("owner"));
  test("admins fails if unauthenticated", testUnauthenticated("admins"));
  test("editors fails if unauthenticated", testUnauthenticated("editors"));
  test("spectators fails if unauthenticated", testUnauthenticated("spectators"));
  test("notificationsCount fails if unauthenticated", testUnauthenticated("notificationsCount"));
  test("myRole fails if unauthenticated", testUnauthenticated("myRole"));
});
