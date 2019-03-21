import UserResolverFactory from "../graphql/resolvers/User";
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
  MockedPendingEnvironmentShare,
  mockPendingEnvironmentShareData,
  MockedPendingOwnerChange,
  mockedPendingOwnerChangeData,
  mockContext
} = MocksGenerator();

const UserResolver = UserResolverFactory({
  User: MockedUser,
  Environment: MockedEnvironment,
  Device: MockedDevice,
  Notification: MockedNotification,
  PendingEnvironmentShare: MockedPendingEnvironmentShare,
  PendingOwnerChange: MockedPendingOwnerChange
});

describe("User", () => {
  const testUserScalarProp = testScalarProp(UserResolver, { id: "mockUserId" }, mockUserData[0]);
  const testUnauthenticated = unauthenticatedShouldFail(UserResolver, {
    id: "mockUserId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    UserResolver,
    { id: "mockUserId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );

  let privateScalarProps = ["quietMode", "paymentPlan", "devMode"];
  let publicScalarProps = ["email", "name", "profileIcon", "profileIconColor", "emailIsVerified"];

  const scalarProps = [...privateScalarProps, ...publicScalarProps];
  for (let prop of scalarProps) {
    test(`${prop} is resolved correctly`, testUserScalarProp(prop));
  }

  test("settings is resolved correctly", async done => {
    const settingsFound = await new Promise((resolve, reject) => {
      UserResolver.settings(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(settingsFound).toMatchObject({
      language: mockUserData[0].settings_language,
      timeFormat: mockUserData[0].settings_timeFormat,
      dateFormat: mockUserData[0].settings_dateFormat,
      temperature: mockUserData[0].settings_temperature,
      lengthAndMass: mockUserData[0].settings_lengthAndMass
    });

    done();
  });
  test.skip("deviceCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("environments is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("environmentCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("notificationCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("valueCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("valueCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test("pendingEnvironmentShares is resolved correctly", async done => {
    // sender user should see the pendingEnvironmentShare in the user prop
    const pendingEnvironmentSharesFound = await new Promise((resolve, reject) => {
      UserResolver.pendingEnvironmentShares(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingEnvironmentSharesFound.length).toEqual(0);

    // receiver user should see the pendingEnvironmentShares
    const pendingEnvironmentSharesFound2 = await new Promise((resolve, reject) => {
      UserResolver.pendingEnvironmentShares(
        { id: "mockUserId2" },
        {},
        {
          auth: { userId: "mockUserId2", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingEnvironmentSharesFound2.length).toEqual(1);
    expect(pendingEnvironmentSharesFound2[0]).toMatchObject({
      id: "mockPendingEnvironmentShareId"
    });

    done();
  });
  test("pendingEnvironmentShareCount is resolved correctly", async done => {
    // sender user should see the pendingEnvironmentShare in the user prop
    const pendingEnvironmentSharesFound = await new Promise((resolve, reject) => {
      UserResolver.pendingEnvironmentShareCount(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingEnvironmentSharesFound).toEqual(0);

    // receiver user should see the pendingEnvironmentShares
    const pendingEnvironmentSharesFound2 = await new Promise((resolve, reject) => {
      UserResolver.pendingEnvironmentShareCount(
        { id: "mockUserId2" },
        {},
        {
          auth: { userId: "mockUserId2", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingEnvironmentSharesFound2).toEqual(1);

    done();
  });
  test("pendingOwnerChanges is resolved correctly", async done => {
    // receiver user should see the pendingEnvironmentShare in the user prop
    const pendingOwnerChangesFound = await new Promise((resolve, reject) => {
      UserResolver.pendingOwnerChanges(
        { id: "mockUserId2" },
        {},
        {
          auth: { userId: "mockUserId2", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingOwnerChangesFound.length).toEqual(1);
    expect(pendingOwnerChangesFound[0]).toMatchObject({
      id: "mockPendingOwnerChangeId"
    });

    // sender user shouldn't see the pendingEnvironmentShares in the user prop
    const pendingOwnerChangesFound2 = await new Promise((resolve, reject) => {
      UserResolver.pendingOwnerChanges(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingOwnerChangesFound2.length).toEqual(0);

    done();
  });
  test("pendingOwnerChangeCount is resolved correctly", async done => {
    const pendingOwnerChangesFound = await new Promise((resolve, reject) => {
      UserResolver.pendingOwnerChangeCount(
        { id: "mockUserId2" },
        {},
        {
          auth: { userId: "mockUserId2", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingOwnerChangesFound).toEqual(1);

    const pendingOwnerChangesFound2 = await new Promise((resolve, reject) => {
      UserResolver.pendingOwnerChangeCount(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          ...mockContext
        }
      )(resolve, reject);
    });

    expect(pendingOwnerChangesFound2).toEqual(0);

    done();
  });
  test.skip("permanentTokens is resolved correctly", async done => {
    //TODO: need mocks of PendingOwnerChange
  });
  test.skip("permanentTokenCount is resolved correctly", async done => {
    //TODO: need mocks of PendingOwnerChange
  });

  const authorizedProps = [
    ...privateScalarProps,
    "settings",
    "deviceCount",
    "pendingEnvironmentShares",
    "pendingEnvironmentShareCount",
    "pendingOwnerChanges",
    "pendingOwnerChangeCount",
    "environmentCount",
    "environments",
    "notificationCount",
    "valueCount",
    "permanentTokens",
    "permanentTokenCount"
  ];
  for (let prop of authorizedProps) {
    test(`${prop} fails if not authorized`, testNotAuthorized(prop));
  }

  for (let prop of authorizedProps) {
    test(`${prop} fails if unauthenticated`, testUnauthenticated(prop));
  }
});
