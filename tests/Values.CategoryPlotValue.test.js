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
  MockedCategoryPlotValue,
  MockedCategoryPlotNode,
  mockCategoryPlotValueData,
  mockContext
} = MocksGenerator();

const { CategoryPlotValue: CategoryPlotValueResolver } = ValuesResolverFactory(
  {
    CategoryPlotValue: MockedCategoryPlotValue,
    CategoryPlotNode: MockedCategoryPlotNode
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("CategoryPlotValue", () => {
  const testCategoryPlotValueScalarProp = testScalarProp(
    CategoryPlotValueResolver,
    { id: "mockCategoryPlotValueId" },
    mockCategoryPlotValueData[0]
  );
  const testCategoryPlotValueUnauthenticated = unauthenticatedShouldFail(
    CategoryPlotValueResolver,
    {
      id: "mockCategoryPlotValueId"
    }
  );
  const testCategoryPlotValueNotAuthorized = notAuthorizedShouldFail(
    CategoryPlotValueResolver,
    { id: "mockCategoryPlotValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testCategoryPlotValueWrongId = wrongIdShouldFail(
    CategoryPlotValueResolver,
    { id: "wrongCategoryPlotValueId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  const scalarProps = [
    "createdAt",
    "updatedAt",
    "visibility",
    "cardSize",
    "name",
    "index",
    "allowedValues"
  ];

  scalarProps.forEach(prop =>
    test(`${prop} is resolved correctly by sender`, testCategoryPlotValueScalarProp(prop))
  );
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      CategoryPlotValueResolver.device(
        { id: "mockCategoryPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockCategoryPlotValueData[0].deviceId });
  });
  test("environment is resolved correctly", async () => {
    const environmentFound = await new Promise((resolve, reject) => {
      CategoryPlotValueResolver.environment(
        { id: "mockCategoryPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(environmentFound).toMatchObject({ id: "mockEnvironmentId" });
  });
  test("value is resolved correctly", async () => {
    const valueFound = await new Promise((resolve, reject) => {
      CategoryPlotValueResolver.value(
        { id: "mockCategoryPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(valueFound.length).toEqual(3);
    const valueIds = valueFound.map(value => value.id);
    expect(valueIds).toContain("mockCategoryPlotNodeId");
    expect(valueIds).toContain("mockCategoryPlotNodeId2");
    expect(valueIds).toContain("mockCategoryPlotNodeId3");
  });
  // TODO: implement order in mocks
  test.skip("lastNode is resolved correctly", async () => {
    const nodeFound = await new Promise((resolve, reject) => {
      CategoryPlotValueResolver.lastNode(
        { id: "mockPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });
    expect(nodeFound.id).toEqual("mockCategoryPlotNodeId2");
  });
  test("myRole is resolved correctly", async () => {
    const roleFound = await new Promise((resolve, reject) => {
      CategoryPlotValueResolver.myRole(
        { id: "mockCategoryPlotValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(roleFound).toEqual("OWNER");
    const roleFound2 = await new Promise((resolve, reject) => {
      CategoryPlotValueResolver.myRole(
        { id: "mockCategoryPlotValueId" },
        {},
        { auth: { userId: "mockUserId3", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(roleFound2).toEqual("ADMIN");
  });

  const allProps = [...scalarProps, "device", "environment", "myRole", "value", "lastNode"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testCategoryPlotValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testCategoryPlotValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testCategoryPlotValueWrongId(prop));
  });
});
