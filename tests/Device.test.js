import DeviceResolverFactory from "../graphql/resolvers/Device";
import MocksGenerator from "./mockUtils";
import {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
} from "./testUtils";

const {
  MockedEnvironment,
  MockedUser,
  MockedDevice,
  mockEnvironmentData,
  mockUserData,
  mockDeviceData,
  MockedNotification,
  mockNotificationData,
  mockContext
} = MocksGenerator();

const DeviceResolver = DeviceResolverFactory({
  User: MockedUser,
  Environment: MockedEnvironment,
  Device: MockedDevice,
  Notification: MockedNotification
});

describe("Device", () => {
  const testDeviceScalarProp = testScalarProp(
    DeviceResolver,
    { id: "mockDeviceId" },
    mockDeviceData[0]
  );
  const testProducerHasAccess = testScalarProp(
    DeviceResolver,
    { id: "mockDeviceId" },
    mockDeviceData[0],
    "mockUserId5"
  );
  const testUnauthenticated = unauthenticatedShouldFail(DeviceResolver, {
    id: "mockDeviceId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    DeviceResolver,
    { id: "mockDeviceId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testWrongId = wrongIdShouldFail(
    DeviceResolver,
    { id: "wrongDeviceId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  const scalarProps = [
    "name",
    "deviceType",
    "index",
    "online",
    "batteryStatus",
    "batteryCharging",
    "signalStatus",
    "firmware"
  ];

  for (let prop of scalarProps) {
    test(`${prop} is resolved correctly`, testDeviceScalarProp(prop));
  }
  const producerAccessibleProp = [
    "deviceType",
    "online",
    "batteryStatus",
    "batteryCharging",
    "signalStatus",
    "firmware"
  ];

  for (let prop of producerAccessibleProp) {
    test(`${prop} is accessible for producer`, testProducerHasAccess(prop));
  }

  test("muted is resolved correctly", async done => {
    const mutedFound = await new Promise((resolve, reject) => {
      DeviceResolver.muted(
        { id: "mockDeviceId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    const correctQuietMode =
      mockDeviceData[0].muted || mockEnvironmentData[0].muted || mockUserData[0].quietMode;
    expect(mutedFound).toEqual(correctQuietMode);

    done();
  });
  test("environment is resolved correctly", async done => {
    const environmentFound = await new Promise((resolve, reject) => {
      DeviceResolver.environment(
        { id: "mockDeviceId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(environmentFound).toMatchObject({ id: mockDeviceData[0].environmentId });

    done();
  });
  test.skip("values is resolved correctly", async done => {
    const valuesFound = await new Promise((resolve, reject) => {
      DeviceResolver.values(
        { id: "mockDeviceId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(mutedFound).toEqual(correctQuietMode);

    done();
  });
  test("notifications is resolved correctly", async done => {
    const notificationsFound = await new Promise((resolve, reject) => {
      DeviceResolver.notifications(
        { id: "mockDeviceId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(notificationsFound.length).toBeDefined();
    expect(notificationsFound.length).toEqual(2);
    expect(notificationsFound[0]).toMatchObject({ id: mockNotificationData[0].id });
    done();
  });
  test("notificationCount is resolved correctly", async done => {
    const notificationCount = await new Promise((resolve, reject) => {
      DeviceResolver.notificationCount(
        { id: "mockDeviceId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(notificationCount).toEqual(2);

    done();
  });

  const allProps = [
    "name",
    "deviceType",
    "index",
    "online",
    "batteryStatus",
    "batteryCharging",
    "signalStatus",
    "firmware",
    "createdAt",
    "updatedAt",
    "values",
    "notifications",
    "notificationCount",
    "environment",
    "muted"
  ];

  for (let prop of allProps) {
    test(`${prop} fails if unauthenticated`, testUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testNotAuthorized(prop));
    test(`${prop} fails if id is wrong`, testWrongId(prop));
  }
});
