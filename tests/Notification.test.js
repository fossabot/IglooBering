import NotificationResolverFactory from "../graphql/resolvers/Notification";
import MocksGenerator from "./mockUtils";
import { testScalarProp, unauthenticatedShouldFail, wrongIdShouldFail } from "./testUtils";

const {
  MockedNotification,
  MockedBoard,
  MockedUser,
  MockedDevice,
  mockBoardData,
  mockUserData,
  mockDeviceData,
  mockNotificationData
} = MocksGenerator();

const NotificationResolver = NotificationResolverFactory({
  User: MockedUser,
  Board: MockedBoard,
  Device: MockedDevice,
  Notification: MockedNotification
});

describe("Notification", () => {
  const testNotificationScalarProp = testScalarProp(
    NotificationResolver,
    { id: "mockNotificationId" },
    { ...mockNotificationData[0], visualized: false }
  );
  const testUnauthenticated = unauthenticatedShouldFail(NotificationResolver, {
    id: "mockNotificationId"
  });
  const testWrongId = wrongIdShouldFail(
    NotificationResolver,
    { id: "wrongNotificationId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

  // not using a for loop because this syntax integrates better with the IDE
  test("content is resolved correctly", testNotificationScalarProp("content"));
  test("date is resolved correctly", testNotificationScalarProp("date"));
  test("visualized is resolved correctly", testNotificationScalarProp("visualized"));
  test("user is resolved correctly", async done => {
    const userFound = await new Promise((resolve, reject) => {
      NotificationResolver.user(
        { id: "mockNotificationId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
      )(resolve, reject);
    });

    expect(userFound).toMatchObject({ id: mockNotificationData[0].userId });

    done();
  });
  test("device is resolved correctly", async done => {
    const deviceFound = await new Promise((resolve, reject) => {
      NotificationResolver.device(
        { id: "mockNotificationId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockNotificationData[0].deviceId });

    done();
  });
  test("board is resolved correctly", async done => {
    const boardFound = await new Promise((resolve, reject) => {
      NotificationResolver.board(
        { id: "mockNotificationId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
      )(resolve, reject);
    });

    expect(boardFound).toMatchObject({ id: mockNotificationData[0].boardId });

    done();
  });

  test("content fails if unauthenticated", testWrongId("content"));
  test("date fails if unauthenticated", testWrongId("date"));
  test("visualized fails if unauthenticated", testWrongId("visualized"));
  test("user fails if unauthenticated", testWrongId("user"));
  test("board fails if unauthenticated", testWrongId("board"));
  test("device fails if unauthenticated", testWrongId("device"));

  test("content fails if unauthenticated", testUnauthenticated("content"));
  test("date fails if unauthenticated", testUnauthenticated("date"));
  test("visualized fails if unauthenticated", testUnauthenticated("visualized"));
  test("user fails if unauthenticated", testUnauthenticated("user"));
  test("board fails if unauthenticated", testUnauthenticated("board"));
  test("device fails if unauthenticated", testUnauthenticated("device"));
});
