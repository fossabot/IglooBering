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
  MockedStringValue,
  mockStringValueData,
  mockContext
} = MocksGenerator();

const { StringValue: StringValueResolver } = ValuesResolverFactory(
  {
    StringValue: MockedStringValue
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("StringValue", () => {
  const testStringValueScalarProp = testScalarProp(
    StringValueResolver,
    { id: "mockStringValueId" },
    mockStringValueData[0]
  );
  const testStringValueUnauthenticated = unauthenticatedShouldFail(StringValueResolver, {
    id: "mockStringValueId"
  });
  const testStringValueNotAuthorized = notAuthorizedShouldFail(
    StringValueResolver,
    { id: "mockStringValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testStringValueWrongId = wrongIdShouldFail(
    StringValueResolver,
    { id: "wrongStringValueId" },
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
    "permission",
    "maxChars",
    "allowedValues"
  ];

  scalarProps.forEach(prop =>
    test(`${prop} is resolved correctly by sender`, testStringValueScalarProp(prop))
  );
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      StringValueResolver.device(
        { id: "mockStringValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockStringValueData[0].deviceId });
  });
  test("environment is resolved correctly", async () => {
    const environmentFound = await new Promise((resolve, reject) => {
      StringValueResolver.environment(
        { id: "mockStringValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(environmentFound).toMatchObject({ id: "mockEnvironmentId" });
  });
  test("myRole is resolved correctly", async () => {
    const roleFound = await new Promise((resolve, reject) => {
      StringValueResolver.myRole(
        { id: "mockStringValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(roleFound).toEqual("OWNER");
    const roleFound2 = await new Promise((resolve, reject) => {
      StringValueResolver.myRole(
        { id: "mockStringValueId" },
        {},
        { auth: { userId: "mockUserId3", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(roleFound2).toEqual("ADMIN");
  });
  const allProps = [...scalarProps, "device", "environment", "myRole"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testStringValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testStringValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testStringValueWrongId(prop));
  });
});
