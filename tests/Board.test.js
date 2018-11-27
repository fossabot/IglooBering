import BoardResolverFactory from "../graphql/resolvers/Board";
import MocksGenerator from "./mockUtils";
import { testScalarProp, unauthenticatedShouldFail, wrongIdShouldFail } from "./testUtils";

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
  const testWrongId = wrongIdShouldFail(
    BoardResolver,
    { id: "wrongBoardId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

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
  test("notificationCount fails if unauthenticated", testUnauthenticated("notificationCount"));
  test("myRole fails if unauthenticated", testUnauthenticated("myRole"));
  test("pendingBoardShares fails if unauthenticated", testUnauthenticated("pendingBoardShares"));

  test("customName fails if unauthenticated", testWrongId("customName"));
  test("avatar fails if unauthenticated", testWrongId("avatar"));
  test("index fails if unauthenticated", testWrongId("index"));
  test("createdAt fails if unauthenticated", testWrongId("createdAt"));
  test("updatedAt fails if unauthenticated", testWrongId("updatedAt"));
  test("quietMode fails if unauthenticated", testWrongId("quietMode"));
  test("devices fails if unauthenticated", testWrongId("devices"));
  test("owner fails if unauthenticated", testWrongId("owner"));
  test("admins fails if unauthenticated", testWrongId("admins"));
  test("editors fails if unauthenticated", testWrongId("editors"));
  test("spectators fails if unauthenticated", testWrongId("spectators"));
  test("notificationCount fails if unauthenticated", testWrongId("notificationCount"));
  test("myRole fails if unauthenticated", testWrongId("myRole"));
  test("pendingBoardShares fails if unauthenticated", testWrongId("pendingBoardShares"));
});
