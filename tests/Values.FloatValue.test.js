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

  test("createdAt is resolved correctly by sender", testFloatValueScalarProp("createdAt"));
  test("updatedAt is resolved correctly by sender", testFloatValueScalarProp("updatedAt"));
  test("visibility is resolved correctly by sender", testFloatValueScalarProp("visibility"));
  test("cardSize is resolved correctly by sender", testFloatValueScalarProp("cardSize"));
  test("name is resolved correctly by sender", testFloatValueScalarProp("name"));
  test("value is resolved correctly by sender", testFloatValueScalarProp("value"));
  test("index is resolved correctly by sender", testFloatValueScalarProp("index"));
  //   test("sender is resolved correctly", async done => {
  //     const usersWithAccesIds = ["mockUserId", "mockUserId2"];

  //     for (let userId of usersWithAccesIds) {
  //       const senderFound = await new Promise((resolve, reject) => {
  //         PendingOwnerChangeResolver.sender(
  //           { id: "mockPendingOwnerChangeId" },
  //           {},
  //           { auth: { userId, tokenType: "TEMPORARY" }, ...mockContext }
  //         )(resolve, reject);
  //       });

  //       expect(senderFound).toMatchObject({ id: mockPendingOwnerChangeData[0].senderId });
  //     }
  //     done();
  //   });
  test("createdAt fails if unauthenticated", testFloatValueUnauthenticated("createdAt"));
  test("updatedAt fails if unauthenticated", testFloatValueUnauthenticated("updatedAt"));
  test("visibility fails if unauthenticated", testFloatValueUnauthenticated("visibility"));
  test("cardSize fails if unauthenticated", testFloatValueUnauthenticated("cardSize"));
  test("name fails if unauthenticated", testFloatValueUnauthenticated("name"));
  test("value fails if unauthenticated", testFloatValueUnauthenticated("value"));
  test("index fails if unauthenticated", testFloatValueUnauthenticated("index"));

  test("createdAt fails if not authorized", testFloatValueNotAuthorized("createdAt"));
  test("updatedAt fails if not authorized", testFloatValueNotAuthorized("updatedAt"));
  test("visibility fails if not authorized", testFloatValueNotAuthorized("visibility"));
  test("cardSize fails if not authorized", testFloatValueNotAuthorized("cardSize"));
  test("name fails if not authorized", testFloatValueNotAuthorized("name"));
  test("value fails if not authorized", testFloatValueNotAuthorized("value"));
  test("index fails if not authorized", testFloatValueNotAuthorized("index"));

  test("createdAt fails if wrong id", testFloatValueWrongId("createdAt"));
  test("updatedAt fails if wrong id", testFloatValueWrongId("updatedAt"));
  test("visibility fails if wrong id", testFloatValueWrongId("visibility"));
  test("cardSize fails if wrong id", testFloatValueWrongId("cardSize"));
  test("name fails if wrong id", testFloatValueWrongId("name"));
  test("value fails if wrong id", testFloatValueWrongId("value"));
  test("index fails if wrong id", testFloatValueWrongId("index"));
});
