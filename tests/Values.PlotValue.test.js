import ValuesResolverFactory from "../graphql/resolvers/Values";
import MocksGenerator from "./mockUtils";
import {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
} from "./testUtils";

const {
  MockedEnvironment,
  MockedDevice,
  MockedUser,
  MockedFloatSeriesValue,
  MockedFloatSeriesNode,
  mockSeriesValueData,
  mockContext
} = MocksGenerator();

const { FloatSeriesValue: FloatSeriesValueResolver } = ValuesResolverFactory(
  {
    FloatSeriesValue: MockedFloatSeriesValue,
    FloatSeriesNode: MockedFloatSeriesNode
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("FloatSeriesValue", () => {
  const testSeriesValueScalarProp = testScalarProp(
    FloatSeriesValueResolver,
    { id: "mockFloatSeriesValueId" },
    mockSeriesValueData[0]
  );
  const testSeriesValueUnauthenticated = unauthenticatedShouldFail(FloatSeriesValueResolver, {
    id: "mockFloatSeriesValueId"
  });
  const testSeriesValueNotAuthorized = notAuthorizedShouldFail(
    FloatSeriesValueResolver,
    { id: "mockFloatSeriesValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testSeriesValueWrongId = wrongIdShouldFail(
    FloatSeriesValueResolver,
    { id: "wrongSeriesValueId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  const scalarProps = [
    "createdAt",
    "updatedAt",
    "private",
    "hidden",
    "cardSize",
    "name",
    "index",
    "unitOfMeasurement",
    "precision",
    "min",
    "max",
    "threshold"
  ];

  scalarProps.forEach(prop =>
    test(`${prop} is resolved correctly by sender`, testSeriesValueScalarProp(prop))
  );
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      FloatSeriesValueResolver.device(
        { id: "mockFloatSeriesValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockSeriesValueData[0].deviceId });
  });
  test("value is resolved correctly", async () => {
    const valueFound = await new Promise((resolve, reject) => {
      FloatSeriesValueResolver.value(
        { id: "mockFloatSeriesValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(valueFound.length).toEqual(3);
    const valueIds = valueFound.map(value => value.id);
    expect(valueIds).toContain("mockFloatSeriesNodeId");
    expect(valueIds).toContain("mockFloatSeriesNodeId2");
    expect(valueIds).toContain("mockFloatSeriesNodeId3");
  });
  // TODO: implement order in mocks
  test.skip("lastNode is resolved correctly", async () => {
    const nodeFound = await new Promise((resolve, reject) => {
      FloatSeriesValueResolver.lastNode(
        { id: "mockFloatSeriesValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(nodeFound.id).toEqual("mockFloatSeriesNodeId2");
  });

  const allProps = [...scalarProps, "device", "value", "lastNode"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testSeriesValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testSeriesValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testSeriesValueWrongId(prop));
  });
});
