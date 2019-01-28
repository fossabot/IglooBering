import EnvironmentResolverFactory from "../graphql/resolvers/Environment";
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
  mockEnvironmentData,
  mockUserData,
  mockDeviceData,
  MockedDevice,
  MockedNotification,
  mockNotificationData,
  mockContext
} = MocksGenerator();

const EnvironmentResolver = EnvironmentResolverFactory({
  User: MockedUser,
  Environment: MockedEnvironment,
  Device: MockedDevice,
  Notification: MockedNotification
});

describe("Environment", () => {
  const testEnvironmentScalarProp = testScalarProp(
    EnvironmentResolver,
    { id: "mockEnvironmentId" },
    mockEnvironmentData[0]
  );
  const testUnauthenticated = unauthenticatedShouldFail(EnvironmentResolver, {
    id: "mockEnvironmentId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    EnvironmentResolver,
    { id: "mockEnvironmentId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testWrongId = wrongIdShouldFail(
    EnvironmentResolver,
    { id: "wrongEnvironmentId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  let scalarProps = ["name", "picture", "index", "createdAt", "updatedAt"];

  for (let prop of scalarProps) {
    test(`${prop} is resolved correctly`, testEnvironmentScalarProp(prop));
  }

  test("muted is resolved correctly", async done => {
    const mutedFound = await new Promise((resolve, reject) => {
      EnvironmentResolver.muted(
        { id: "mockEnvironmentId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    const correctQuietMode = mockEnvironmentData[0].muted || mockUserData[0].quietMode;
    expect(mutedFound).toEqual(correctQuietMode);

    done();
  });

  test("devices is resolved correctly", async done => {
    const devicesFound = await new Promise((resolve, reject) => {
      EnvironmentResolver.devices(
        { id: "mockEnvironmentId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(devicesFound.length).toBeDefined();
    expect(devicesFound.length).toEqual(1);
    expect(devicesFound[0]).toMatchObject({ id: mockDeviceData[0].id });
    done();
  });

  test("deviceCount is resolved correctly", async done => {
    const deviceCount = await new Promise((resolve, reject) => {
      EnvironmentResolver.deviceCount(
        { id: "mockEnvironmentId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(deviceCount).toEqual(1);
    done();
  });

  test("notificationCount is resolved correctly", async done => {
    const notificationCount = await new Promise((resolve, reject) => {
      EnvironmentResolver.notificationCount(
        { id: "mockEnvironmentId" },
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

  let authorizedProps = [
    "picture",
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
    "pendingEnvironmentShares"
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
