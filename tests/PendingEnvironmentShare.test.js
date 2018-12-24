import PendingEnvironmentShareResolverFactory from "../graphql/resolvers/PendingEnvironmentShare";
import MocksGenerator from "./mockUtils";
import {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
} from "./testUtils";

const {
  MockedEnvironment,
  MockedUser,
  MockedPendingEnvironmentShare,
  mockEnvironmentData,
  mockUserData,
  mockPendingEnvironmentShareData,
  mockContext
} = MocksGenerator();

const PendingEnvironmentShareResolver = PendingEnvironmentShareResolverFactory({
  User: MockedUser,
  Environment: MockedEnvironment,
  PendingEnvironmentShare: MockedPendingEnvironmentShare
});

describe("PendingEnvironmentShare", () => {
  const testPendingEnvironmentShareScalarProp = testScalarProp(
    PendingEnvironmentShareResolver,
    { id: "mockPendingEnvironmentShareId" },
    mockPendingEnvironmentShareData[0]
  );
  const testUnauthenticated = unauthenticatedShouldFail(PendingEnvironmentShareResolver, {
    id: "mockPendingEnvironmentShareId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    PendingEnvironmentShareResolver,
    { id: "mockPendingEnvironmentShareId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testWrongId = wrongIdShouldFail(
    PendingEnvironmentShareResolver,
    { id: "wrongPendingEnvironmentShareId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  // not using a for loop because this syntax integrates better with the IDE
  test("id is resolved correctly", testPendingEnvironmentShareScalarProp("id"));
  test("role is resolved correctly", testPendingEnvironmentShareScalarProp("role"));
  test("receiver is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2", "mockUserId3"];

    for (let userId of usersWithAccesIds) {
      const receiverFound = await new Promise((resolve, reject) => {
        PendingEnvironmentShareResolver.receiver(
          { id: "mockPendingEnvironmentShareId" },
          {},
          { auth: { userId, tokenType: "TEMPORARY" }, ...mockContext }
        )(resolve, reject);
      });

      expect(receiverFound).toMatchObject({ id: mockPendingEnvironmentShareData[0].receiverId });
    }
    done();
  });
  test("sender is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2", "mockUserId3"];

    for (let userId of usersWithAccesIds) {
      const senderFound = await new Promise((resolve, reject) => {
        PendingEnvironmentShareResolver.sender(
          { id: "mockPendingEnvironmentShareId" },
          {},
          { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
        )(resolve, reject);
      });

      expect(senderFound).toMatchObject({ id: mockPendingEnvironmentShareData[0].senderId });
    }
    done();
  });
  test("environment is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2", "mockUserId3"];

    for (let userId of usersWithAccesIds) {
      const environmentFound = await new Promise((resolve, reject) => {
        PendingEnvironmentShareResolver.environment(
          { id: "mockPendingEnvironmentShareId" },
          {},
          { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
        )(resolve, reject);
      });

      expect(environmentFound).toMatchObject({
        id: mockPendingEnvironmentShareData[0].environmentId
      });
    }
    done();
  });

  test("id fails if unauthenticated", testUnauthenticated("id"));
  test("sender fails if unauthenticated", testUnauthenticated("sender"));
  test("receiver fails if unauthenticated", testUnauthenticated("receiver"));
  test("role fails if unauthenticated", testUnauthenticated("role"));
  test("environment fails if unauthenticated", testUnauthenticated("environment"));

  test("id fails if unauthenticated", testNotAuthorized("id"));
  test("sender fails if unauthenticated", testNotAuthorized("sender"));
  test("receiver fails if unauthenticated", testNotAuthorized("receiver"));
  test("role fails if unauthenticated", testNotAuthorized("role"));
  test("environment fails if unauthenticated", testNotAuthorized("environment"));

  test("id fails if unauthenticated", testWrongId("id"));
  test("sender fails if unauthenticated", testWrongId("sender"));
  test("receiver fails if unauthenticated", testWrongId("receiver"));
  test("role fails if unauthenticated", testWrongId("role"));
  test("environment fails if unauthenticated", testWrongId("environment"));
});
