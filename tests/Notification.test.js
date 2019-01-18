import NotificationResolverFactory from "../graphql/resolvers/Notification";
import MocksGenerator from "./mockUtils";
import { testScalarProp, unauthenticatedShouldFail, wrongIdShouldFail } from "./testUtils";

const {
  MockedNotification,
  MockedEnvironment,
  MockedUser,
  MockedDevice,
  mockEnvironmentData,
  mockUserData,
  mockDeviceData,
  mockNotificationData,
  mockContext
} = MocksGenerator();

const NotificationResolver = NotificationResolverFactory({
  User: MockedUser,
  Environment: MockedEnvironment,
  Device: MockedDevice,
  Notification: MockedNotification
});

describe("Notification", () => {
  const testNotificationScalarProp = testScalarProp(
    NotificationResolver,
    { id: "mockNotificationId" },
    { ...mockNotificationData[0], read: false }
  );
  const testUnauthenticated = unauthenticatedShouldFail(NotificationResolver, {
    id: "mockNotificationId"
  });
  const testWrongId = wrongIdShouldFail(
    NotificationResolver,
    { id: "wrongNotificationId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  // not using a for loop because this syntax integrates better with the IDE
  test("content is resolved correctly", testNotificationScalarProp("content"));
  test("date is resolved correctly", testNotificationScalarProp("date"));
  test("read is resolved correctly", testNotificationScalarProp("read"));
  test("user is resolved correctly", async done => {
    const userFound = await new Promise((resolve, reject) => {
      NotificationResolver.user(
        { id: "mockNotificationId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
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
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockNotificationData[0].deviceId });

    done();
  });
  test("environment is resolved correctly", async done => {
    const environmentFound = await new Promise((resolve, reject) => {
      NotificationResolver.environment(
        { id: "mockNotificationId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(environmentFound).toMatchObject({ id: mockNotificationData[0].environmentId });

    done();
  });

  test("content fails if unauthenticated", testWrongId("content"));
  test("date fails if unauthenticated", testWrongId("date"));
  test("read fails if unauthenticated", testWrongId("read"));
  test("user fails if unauthenticated", testWrongId("user"));
  test("environment fails if unauthenticated", testWrongId("environment"));
  test("device fails if unauthenticated", testWrongId("device"));

  test("content fails if unauthenticated", testUnauthenticated("content"));
  test("date fails if unauthenticated", testUnauthenticated("date"));
  test("read fails if unauthenticated", testUnauthenticated("read"));
  test("user fails if unauthenticated", testUnauthenticated("user"));
  test("environment fails if unauthenticated", testUnauthenticated("environment"));
  test("device fails if unauthenticated", testUnauthenticated("device"));
});
