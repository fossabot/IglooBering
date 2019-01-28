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
  MockedBooleanValue,
  mockBooleanValueData,
  mockContext
} = MocksGenerator();

const { BooleanValue: BooleanValueResolver } = ValuesResolverFactory(
  {
    BooleanValue: MockedBooleanValue
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("BooleanValue", () => {
  const testBooleanValueScalarProp = testScalarProp(
    BooleanValueResolver,
    { id: "mockBooleanValueId" },
    mockBooleanValueData[0]
  );
  const testBooleanValueUnauthenticated = unauthenticatedShouldFail(BooleanValueResolver, {
    id: "mockBooleanValueId"
  });
  const testBooleanValueNotAuthorized = notAuthorizedShouldFail(
    BooleanValueResolver,
    { id: "mockBooleanValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testBooleanValueWrongId = wrongIdShouldFail(
    BooleanValueResolver,
    { id: "wrongBooleanValueId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  const scalarProps = [
    "createdAt",
    "updatedAt",
    "visibility",
    "cardSize",
    "name",
    "value",
    "index",
    "permission"
  ];

  scalarProps.forEach(prop =>
    test(`${prop} is resolved correctly by sender`, testBooleanValueScalarProp(prop))
  );
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      BooleanValueResolver.device(
        { id: "mockBooleanValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockBooleanValueData[0].deviceId });
  });
  test("environment is resolved correctly", async () => {
    const environmentFound = await new Promise((resolve, reject) => {
      BooleanValueResolver.environment(
        { id: "mockBooleanValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(environmentFound).toMatchObject({ id: "mockEnvironmentId" });
  });
  test("myRole is resolved correctly", async () => {
    const roleFound = await new Promise((resolve, reject) => {
      BooleanValueResolver.myRole(
        { id: "mockBooleanValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(roleFound).toEqual("OWNER");
    const roleFound2 = await new Promise((resolve, reject) => {
      BooleanValueResolver.myRole(
        { id: "mockBooleanValueId" },
        {},
        { auth: { userId: "mockUserId3", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(roleFound2).toEqual("ADMIN");
  });

  const allProps = [...scalarProps, "device", "environment", "myRole"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testBooleanValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testBooleanValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testBooleanValueWrongId(prop));
  });
});
