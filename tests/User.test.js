import UserResolverFactory from "../graphql/resolvers/User";
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
  mockBoardData,
  mockUserData,
  mockDeviceData,
  MockedDevice,
  MockedNotification,
  mockNotificationData,
  MockedPendingBoardShare,
  mockPendingBoardShareData
} = MocksGenerator();

const UserResolver = UserResolverFactory({
  User: MockedUser,
  Board: MockedBoard,
  Device: MockedDevice,
  Notification: MockedNotification,
  PendingBoardShare: MockedPendingBoardShare
});

describe("User", () => {
  const testUserScalarProp = testScalarProp(UserResolver, { id: "mockUserId" }, mockUserData[0]);
  const testUnauthenticated = unauthenticatedShouldFail(UserResolver, {
    id: "mockUserId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    UserResolver,
    { id: "mockUserId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" } }
  );

  let privateScalarProps = [
    "quietMode",
    "devMode",
    "monthUsage",
    "emailIsVerified",
    "paymentPlan",
    "usageCap"
  ];
  let publicScalarProps = ["email", "name", "profileIcon", "profileIconColor"];

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
          billingUpdater: { update: () => {} }
        }
      )(resolve, reject);
    });

    expect(settingsFound).toMatchObject({
      language: mockUserData[0].settings_language,
      timeFormat: mockUserData[0].settings_timeFormat,
      timeZone: mockUserData[0].settings_timeZone,
      dateFormat: mockUserData[0].settings_dateFormat,
      temperature: mockUserData[0].settings_temperature,
      lengthAndMass: mockUserData[0].settings_lengthAndMass
    });

    done();
  });
  test.skip("devices is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
    const devicesFound = await new Promise((resolve, reject) => {
      UserResolver.devices(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          billingUpdater: { update: () => {} }
        }
      )(resolve, reject);
    });

    expect(devicesFound.length).toBe(1);
    expect(devicesFound[0]).toMatchObject({ id: "mockDeviceId" });

    done();
  });
  test.skip("deviceCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("boards is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("boardCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("notifications is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("notificationCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("values is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("valueCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("values is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test.skip("valueCount is resolved correctly", async done => {
    //TODO: need implementation of includes in the mock search
  });
  test("pendingBoardShares is resolved correctly", async done => {
    // sender user should see the pendingBoardShare in the user prop
    const pendingBoardSharesFound = await new Promise((resolve, reject) => {
      UserResolver.pendingBoardShares(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          billingUpdater: { update: () => {} }
        }
      )(resolve, reject);
    });

    expect(pendingBoardSharesFound.length).toBe(0);

    // receiver user should see the pendingBoardShares
    const pendingBoardSharesFound2 = await new Promise((resolve, reject) => {
      UserResolver.pendingBoardShares(
        { id: "mockUserId2" },
        {},
        {
          auth: { userId: "mockUserId2", tokenType: "TEMPORARY" },
          billingUpdater: { update: () => {} }
        }
      )(resolve, reject);
    });

    expect(pendingBoardSharesFound2.length).toBe(1);
    expect(pendingBoardSharesFound2[0]).toMatchObject({ id: "mockPendingBoardShareId" });

    done();
  });
  test("pendingBoardShareCount is resolved correctly", async done => {
    // sender user should see the pendingBoardShare in the user prop
    const pendingBoardSharesFound = await new Promise((resolve, reject) => {
      UserResolver.pendingBoardShareCount(
        { id: "mockUserId" },
        {},
        {
          auth: { userId: "mockUserId", tokenType: "TEMPORARY" },
          billingUpdater: { update: () => {} }
        }
      )(resolve, reject);
    });

    expect(pendingBoardSharesFound).toBe(0);

    // receiver user should see the pendingBoardShares
    const pendingBoardSharesFound2 = await new Promise((resolve, reject) => {
      UserResolver.pendingBoardShareCount(
        { id: "mockUserId2" },
        {},
        {
          auth: { userId: "mockUserId2", tokenType: "TEMPORARY" },
          billingUpdater: { update: () => {} }
        }
      )(resolve, reject);
    });

    expect(pendingBoardSharesFound2).toBe(1);

    done();
  });
  test.skip("pendingOwnerChanges is resolved correctly", async done => {
    //TODO: need mocks of PendingOwnerChange
  });
  test.skip("pendingOwnerChangeCount is resolved correctly", async done => {
    //TODO: need mocks of PendingOwnerChange
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
    "devices",
    "deviceCount",
    "pendingBoardShares",
    "pendingBoardShareCount",
    "pendingOwnerChanges",
    "pendingOwnerChangeCount",
    "boardCount",
    "boards",
    "notificationCount",
    "notifications",
    "values",
    "valueCount",
    "permanentTokens",
    "permanentTokenCount"
  ];
  for (let prop of authorizedProps) {
    test(`${prop} fails if not authorized`, testNotAuthorized(prop));
  }

  let authenticatedProps = [...authorizedProps, ...publicScalarProps];

  for (let prop of authenticatedProps) {
    test(`${prop} fails if unauthenticated`, testUnauthenticated(prop));
  }
});
