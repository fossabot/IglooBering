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
  MockedFloatValue,
  mockFloatValueData,
  mockContext
} = MocksGenerator();

const { FloatValue: FloatValueResolver } = ValuesResolverFactory(
  {
    FloatValue: MockedFloatValue
  },
  MockedUser,
  MockedDevice,
  MockedEnvironment
);

describe("FloatValue", () => {
  const testFloatValueScalarProp = testScalarProp(
    FloatValueResolver,
    { id: "mockFloatValueId" },
    mockFloatValueData[0]
  );
  const producerHasAccess = testScalarProp(
    FloatValueResolver,
    { id: "mockFloatValueId" },
    mockFloatValueData[0],
    "mockUserId5"
  );
  const testPrivate = notAuthorizedShouldFail(
    FloatValueResolver,
    { id: "mockPrivateFloatValueId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testFloatValueUnauthenticated = unauthenticatedShouldFail(FloatValueResolver, {
    id: "mockFloatValueId"
  });
  const testFloatValueNotAuthorized = notAuthorizedShouldFail(
    FloatValueResolver,
    { id: "mockFloatValueId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testFloatValueWrongId = wrongIdShouldFail(
    FloatValueResolver,
    { id: "wrongFloatValueId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  const scalarProps = [
    "createdAt",
    "updatedAt",
    "private",
    "hidden",
    "cardSize",
    "name",
    "value",
    "index",
    "unitOfMeasurement",
    "precision",
    "min",
    "max",
    "permission"
  ];

  scalarProps.forEach(prop => {
    test(`${prop} is resolved correctly by sender`, testFloatValueScalarProp(prop));
    test(`${prop} is accessible by producer`, producerHasAccess(prop));
    test(`${prop} of private value is not accessible`, testPrivate(prop));
  });
  test("device is resolved correctly", async () => {
    const deviceFound = await new Promise((resolve, reject) => {
      FloatValueResolver.device(
        { id: "mockFloatValueId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject);
    });

    expect(deviceFound).toMatchObject({ id: mockFloatValueData[0].deviceId });
  });

  const allProps = [...scalarProps, "device"];

  allProps.forEach(prop => {
    test(`${prop} fails if unauthenticated`, testFloatValueUnauthenticated(prop));
    test(`${prop} fails if not authorized`, testFloatValueNotAuthorized(prop));
    test(`${prop} fails if wrong id`, testFloatValueWrongId(prop));
  });
});
