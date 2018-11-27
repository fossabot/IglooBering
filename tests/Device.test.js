import DeviceResolverFactory from "../graphql/resolvers/Device";
import MocksGenerator from "./mockUtils";
import { testScalarProp, unauthenticatedShouldFail, wrongIdShouldFail } from "./testUtils";

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

  const testWrongId = wrongIdShouldFail(
    DeviceResolver,
    { id: "wrongDeviceId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

  // not using a for loop because this syntax integrates better with the IDE
  test("customName is resolved correctly", testDeviceScalarProp("customName"));
  test("deviceType is resolved correctly", testDeviceScalarProp("deviceType"));
  test("index is resolved correctly", testDeviceScalarProp("index"));
  test("online is resolved correctly", testDeviceScalarProp("online"));
  test("batteryStatus is resolved correctly", testDeviceScalarProp("batteryStatus"));
  test("batteryCharging is resolved correctly", testDeviceScalarProp("batteryCharging"));
  test("signalStatus is resolved correctly", testDeviceScalarProp("signalStatus"));
  test("firmware is resolved correctly", testDeviceScalarProp("firmware"));

  test("quietMode is resolved correctly", async done => {
    const quietModeFound = await DeviceResolver.quietMode(
      { id: "mockDeviceId" },
      {},
      { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
    );

    const correctQuietMode =
      mockDeviceData[0].quietMode || mockBoardData[0].quietMode || mockUserData[0].quietMode;
    expect(quietModeFound).toBe(correctQuietMode);

    done();
  });

  test("customName fails if unauthenticated", testUnauthenticated("customName"));
  test("deviceType fails if unauthenticated", testUnauthenticated("deviceType"));
  test("index fails if unauthenticated", testUnauthenticated("index"));
  test("createdAt fails if unauthenticated", testUnauthenticated("createdAt"));
  test("updatedAt fails if unauthenticated", testUnauthenticated("updatedAt"));
  test("quietMode fails if unauthenticated", testUnauthenticated("quietMode"));
  test("batteryStatus fails if unauthenticated", testUnauthenticated("batteryStatus"));
  test("batteryCharging fails if unauthenticated", testUnauthenticated("batteryCharging"));
  test("signalStatus fails if unauthenticated", testUnauthenticated("signalStatus"));
  test("firmware fails if unauthenticated", testUnauthenticated("firmware"));
  test("values fails if unauthenticated", testUnauthenticated("values"));
  test("notifications fails if unauthenticated", testUnauthenticated("notifications"));
  test("notificationCount fails if unauthenticated", testUnauthenticated("notificationCount"));
  test("board fails if unauthenticated", testUnauthenticated("board"));

  test("customName fails if unauthenticated", testWrongId("customName"));
  test("deviceType fails if unauthenticated", testWrongId("deviceType"));
  test("index fails if unauthenticated", testWrongId("index"));
  test("createdAt fails if unauthenticated", testWrongId("createdAt"));
  test("updatedAt fails if unauthenticated", testWrongId("updatedAt"));
  test("quietMode fails if unauthenticated", testWrongId("quietMode"));
  test("batteryStatus fails if unauthenticated", testWrongId("batteryStatus"));
  test("batteryCharging fails if unauthenticated", testWrongId("batteryCharging"));
  test("signalStatus fails if unauthenticated", testWrongId("signalStatus"));
  test("firmware fails if unauthenticated", testWrongId("firmware"));
  test("values fails if unauthenticated", testWrongId("values"));
  test("notifications fails if unauthenticated", testWrongId("notifications"));
  test("notificationCount fails if unauthenticated", testWrongId("notificationCount"));
  test("board fails if unauthenticated", testWrongId("board"));
});
