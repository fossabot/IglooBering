import PermanentTokenResolver from "../graphql/resolvers/PermanentToken";
import MocksGenerator from "./mockUtils";
import {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
} from "./testUtils";

const { mockContext, mockPermanentTokenData } = MocksGenerator();

describe("PendingEnvironmentShare", () => {
  const testPermanentTokenScalarProp = testScalarProp(
    PermanentTokenResolver,
    { id: "mockPermanentTokenId" },
    mockPermanentTokenData[0]
  );
  const testUnauthenticated = unauthenticatedShouldFail(PermanentTokenResolver, {
    id: "mockPermanentTokenId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    PermanentTokenResolver,
    { id: "mockPermanentTokenId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testWrongId = wrongIdShouldFail(
    PermanentTokenResolver,
    { id: "wrongPermanentTokenId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  // not using a for loop because this syntax integrates better with the IDE
  test("name is resolved correctly", testPermanentTokenScalarProp("name"));
  test("lastUsed is resolved correctly", testPermanentTokenScalarProp("lastUsed"));
  test("user is resolved correctly", async done => {
    const userFound = await new Promise((resolve, reject) =>
      PermanentTokenResolver.user(
        { id: "mockPermanentTokenId" },
        {},
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(userFound).toMatchObject({ id: mockPermanentTokenData[0].userId });
    done();
  });

  test("lastUsed fails if unauthenticated", testUnauthenticated("lastUsed"));
  test("user fails if unauthenticated", testUnauthenticated("user"));
  test("name fails if unauthenticated", testUnauthenticated("name"));

  test("lastUsed fails if not authorized", testNotAuthorized("lastUsed"));
  test("user fails if not authorized", testNotAuthorized("user"));
  test("name fails if not authorized", testNotAuthorized("name"));

  test("lastUsed fails if wrong id", testWrongId("lastUsed"));
  test("user fails if wrong id", testWrongId("user"));
  test("name fails if wrong id", testWrongId("name"));
});
