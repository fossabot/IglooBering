import BoardResolverFactory from "../graphql/resolvers/Board";
import MocksGenerator from "./mockUtils";
import {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
} from "./testUtils";

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
  const testNotAuthorized = notAuthorizedShouldFail(
    BoardResolver,
    { id: "mockBoardId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" } }
  );
  const testWrongId = wrongIdShouldFail(
    BoardResolver,
    { id: "wrongBoardId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

  let scalarProps = ["name", "avatar", "index", "createdAt", "updatedAt"];

  for (let prop of scalarProps) {
    test(`${prop} is resolved correctly`, testBoardScalarProp(prop));
  }

  test("muted is resolved correctly", async done => {
    const mutedFound = await new Promise((resolve, reject) => {
      BoardResolver.muted(
        { id: "mockBoardId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
      )(resolve, reject);
    });

    const correctQuietMode = mockBoardData[0].muted || mockUserData[0].quietMode;
    expect(mutedFound).toBe(correctQuietMode);

    done();
  });

  let authorizedProps = [
    "avatar",
    "index",
    "createdAt",
    "updatedAt",
    "muted",
    "devices",
    "owner",
    "admins",
    "editors",
    "spectators",
    "notificationCount",
    "myRole",
    "pendingBoardShares"
  ];

  for (let prop of authorizedProps) {
    test(`${prop} fails if not authorized`, testNotAuthorized(prop));
  }

  let allProps = [...authorizedProps, "name"];

  for (let prop of allProps) {
    test(`${prop} fails if unauthenticated`, testUnauthenticated(prop));
    test(`${prop} fails if id is wrong`, testWrongId(prop));
  }
});
