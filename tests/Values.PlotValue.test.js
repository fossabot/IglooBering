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
  MockedPlotValue,
  MockedPlotNode,
  mockPlotValueData,
  mockContext
} = MocksGenerator();

const { PlotValue: PlotValueResolver } = ValuesResolverFactory(
  {
    PlotValue: MockedPlotValue,
    PlotNode: MockedPlotNode
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("PlotValue", () => {
  const testPlotValueScalarProp = testScalarProp(
    PlotValueResolver,
    { id: "mockPlotValueId" },
    mockPlotValueData[0]
  );
  const testPlotValueUnauthenticated = unauthenticatedShouldFail(PlotValueResolver, {
    id: "mockPlotValueId"
  });
  const testPlotValueNotAuthorized = notAuthorizedShouldFail(
    PlotValueResolver,
    { id: "mockPlotValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testPlotValueWrongId = wrongIdShouldFail(
    PlotValueResolver,
    { id: "wrongPlotValueId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  const scalarProps = [
    "createdAt",
    "updatedAt",
    "visibility",
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
    test(`${prop} is resolved correctly by sender`, testPlotValueScalarProp(prop))
  );
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      PlotValueResolver.device(
        { id: "mockPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockPlotValueData[0].deviceId });
  });
  test("value is resolved correctly", async () => {
    const valueFound = await new Promise((resolve, reject) => {
      PlotValueResolver.value(
        { id: "mockPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(valueFound.length).toEqual(3);
    const valueIds = valueFound.map(value => value.id);
    expect(valueIds).toContain("mockPlotNodeId");
    expect(valueIds).toContain("mockPlotNodeId2");
    expect(valueIds).toContain("mockPlotNodeId3");
  });
  // TODO: implement order in mocks
  test.skip("lastNode is resolved correctly", async () => {
    const nodeFound = await new Promise((resolve, reject) => {
      PlotValueResolver.lastNode(
        { id: "mockPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(nodeFound.id).toEqual("mockPlotNodeId2");
  });

  const allProps = [...scalarProps, "device", "value", "lastNode"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testPlotValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testPlotValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testPlotValueWrongId(prop));
  });
});
