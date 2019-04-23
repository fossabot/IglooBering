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
  MockedCategorySeriesValue,
  MockedCategorySeriesNode,
  mockCategorySeriesValueData,
  mockContext
} = MocksGenerator();

const { CategorySeriesValue: CategorySeriesValueResolver } = ValuesResolverFactory(
  {
    CategorySeriesValue: MockedCategorySeriesValue,
    CategorySeriesNode: MockedCategorySeriesNode
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("CategorySeriesValue", () => {
  const testCategorySeriesValueScalarProp = testScalarProp(
    CategorySeriesValueResolver,
    { id: "mockCategorySeriesValueId" },
    mockCategorySeriesValueData[0]
  );
  const testCategorySeriesValueUnauthenticated = unauthenticatedShouldFail(
    CategorySeriesValueResolver,
    {
      id: "mockCategorySeriesValueId"
    }
  );
  const testCategorySeriesValueNotAuthorized = notAuthorizedShouldFail(
    CategorySeriesValueResolver,
    { id: "mockCategorySeriesValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testCategorySeriesValueWrongId = wrongIdShouldFail(
    CategorySeriesValueResolver,
    { id: "wrongCategorySeriesValueId" },
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
    "allowedValues"
  ];

  scalarProps.forEach(prop =>
    test(`${prop} is resolved correctly by sender`, testCategorySeriesValueScalarProp(prop))
  );
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      CategorySeriesValueResolver.device(
        { id: "mockCategorySeriesValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockCategorySeriesValueData[0].deviceId });
  });
  test("value is resolved correctly", async () => {
    const valueFound = await new Promise((resolve, reject) => {
      CategorySeriesValueResolver.nodes(
        { id: "mockCategorySeriesValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(valueFound.length).toEqual(3);
    const valueIds = valueFound.map(value => value.id);
    expect(valueIds).toContain("mockCategorySeriesNodeId");
    expect(valueIds).toContain("mockCategorySeriesNodeId2");
    expect(valueIds).toContain("mockCategorySeriesNodeId3");
  });
  // TODO: implement order in mocks
  test.skip("lastNode is resolved correctly", async () => {
    const nodeFound = await new Promise((resolve, reject) => {
      CategorySeriesValueResolver.lastNode(
        { id: "mockFloatSeriesValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });
    expect(nodeFound.id).toEqual("mockCategorySeriesNodeId2");
  });

  const allProps = [...scalarProps, "device", "nodes", "lastNode"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testCategorySeriesValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testCategorySeriesValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testCategorySeriesValueWrongId(prop));
  });
});
