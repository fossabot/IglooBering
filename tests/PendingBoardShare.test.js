import PendingBoardShareResolverFactory from "../graphql/resolvers/PendingBoardShare";
import MocksGenerator from "./mockUtils";
import {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
} from "./testUtils";

const {
  MockedBoard,
  MockedUser,
  MockedPendingBoardShare,
  mockBoardData,
  mockUserData,
  mockPendingBoardShareData
} = MocksGenerator();

const PendingBoardShareResolver = PendingBoardShareResolverFactory({
  User: MockedUser,
  Board: MockedBoard,
  PendingBoardShare: MockedPendingBoardShare
});

describe("PendingBoardShare", () => {
  const testPendingBoardShareScalarProp = testScalarProp(
    PendingBoardShareResolver,
    { id: "mockPendingBoardShareId" },
    mockPendingBoardShareData[0]
  );
  const testUnauthenticated = unauthenticatedShouldFail(PendingBoardShareResolver, {
    id: "mockPendingBoardShareId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    PendingBoardShareResolver,
    { id: "mockPendingBoardShareId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" } }
  );
  const testWrongId = wrongIdShouldFail(
    PendingBoardShareResolver,
    { id: "wrongPendingBoardShareId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

  // not using a for loop because this syntax integrates better with the IDE
  test("id is resolved correctly", testPendingBoardShareScalarProp("id"));
  test("role is resolved correctly", testPendingBoardShareScalarProp("role"));
  test("receiver is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2", "mockUserId3"];

    for (let userId of usersWithAccesIds) {
      const receiverFound = await new Promise((resolve, reject) => {
        PendingBoardShareResolver.receiver(
          { id: "mockPendingBoardShareId" },
          {},
          { auth: { userId, tokenType: "TEMPORARY" } }
        )(resolve, reject);
      });

      expect(receiverFound).toMatchObject({ id: mockPendingBoardShareData[0].receiverId });
    }
    done();
  });
  test("sender is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2", "mockUserId3"];

    for (let userId of usersWithAccesIds) {
      const senderFound = await new Promise((resolve, reject) => {
        PendingBoardShareResolver.sender(
          { id: "mockPendingBoardShareId" },
          {},
          { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
        )(resolve, reject);
      });

      expect(senderFound).toMatchObject({ id: mockPendingBoardShareData[0].senderId });
    }
    done();
  });
  test("board is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2", "mockUserId3"];

    for (let userId of usersWithAccesIds) {
      const boardFound = await new Promise((resolve, reject) => {
        PendingBoardShareResolver.board(
          { id: "mockPendingBoardShareId" },
          {},
          { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
        )(resolve, reject);
      });

      expect(boardFound).toMatchObject({ id: mockPendingBoardShareData[0].boardId });
    }
    done();
  });

  test("id fails if unauthenticated", testUnauthenticated("id"));
  test("sender fails if unauthenticated", testUnauthenticated("sender"));
  test("receiver fails if unauthenticated", testUnauthenticated("receiver"));
  test("role fails if unauthenticated", testUnauthenticated("role"));
  test("board fails if unauthenticated", testUnauthenticated("board"));

  test("id fails if unauthenticated", testNotAuthorized("id"));
  test("sender fails if unauthenticated", testNotAuthorized("sender"));
  test("receiver fails if unauthenticated", testNotAuthorized("receiver"));
  test("role fails if unauthenticated", testNotAuthorized("role"));
  test("board fails if unauthenticated", testNotAuthorized("board"));

  test("id fails if unauthenticated", testWrongId("id"));
  test("sender fails if unauthenticated", testWrongId("sender"));
  test("receiver fails if unauthenticated", testWrongId("receiver"));
  test("role fails if unauthenticated", testWrongId("role"));
  test("board fails if unauthenticated", testWrongId("board"));
});
