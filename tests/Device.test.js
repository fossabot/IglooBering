import DeviceResolverFactory from "../graphql/resolvers/Device";
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
  MockedDevice,
  mockBoardData,
  mockUserData,
  mockDeviceData
} = MocksGenerator();

const DeviceResolver = DeviceResolverFactory({
  User: MockedUser,
  Board: MockedBoard,
  Device: MockedDevice
});

describe("Device", () => {
  const testDeviceScalarProp = testScalarProp(
    DeviceResolver,
    { id: "mockDeviceId" },
    mockDeviceData[0]
  );
  const testUnauthenticated = unauthenticatedShouldFail(DeviceResolver, {
    id: "mockDeviceId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    DeviceResolver,
    { id: "mockDeviceId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" } }
  );
  const testWrongId = wrongIdShouldFail(
    DeviceResolver,
    { id: "wrongDeviceId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

  // not using a for loop because this syntax integrates better with the IDE
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

  test("muted is resolved correctly", async done => {
    const mutedFound = await DeviceResolver.muted(
      { id: "mockDeviceId" },
      {},
      { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
    );

    const correctQuietMode =
      mockDeviceData[0].muted || mockBoardData[0].muted || mockUserData[0].muted;
    expect(mutedFound).toBe(correctQuietMode);

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
    "board",
    "muted"
  ];

  for (let prop of allProps) {
    test(`${prop} fails if unauthenticated`, testUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testNotAuthorized(prop));
    test(`${prop} fails if id is wrong`, testWrongId(prop));
  }
});
