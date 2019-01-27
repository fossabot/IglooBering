import PendingOwnerChangeResolverFactory from "../graphql/resolvers/PendingOwnerChange";
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
  MockedPendingOwnerChange,
  mockEnvironmentData,
  mockUserData,
  mockPendingOwnerChangeData,
  mockContext
} = MocksGenerator();

const PendingOwnerChangeResolver = PendingOwnerChangeResolverFactory({
  User: MockedUser,
  Environment: MockedEnvironment,
  PendingOwnerChange: MockedPendingOwnerChange
});

describe("PendingOwnerChange", () => {
  const testPendingOwnerChangeScalarPropSender = testScalarProp(
    PendingOwnerChangeResolver,
    { id: "mockPendingOwnerChangeId" },
    mockPendingOwnerChangeData[0]
  );
  const testPendingOwnerChangeScalarPropReceiver = testScalarProp(
    PendingOwnerChangeResolver,
    { id: "mockPendingOwnerChangeId" },
    mockPendingOwnerChangeData[0],
    "mockUserId2"
  );
  const testUnauthenticated = unauthenticatedShouldFail(PendingOwnerChangeResolver, {
    id: "mockPendingOwnerChangeId"
  });
  const testNotAuthorized = notAuthorizedShouldFail(
    PendingOwnerChangeResolver,
    { id: "mockPendingOwnerChangeId" },
    { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
  );
  const testWrongId = wrongIdShouldFail(
    PendingOwnerChangeResolver,
    { id: "wrongPendingOwnerChangeId" },
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
  );

  test("id is resolved correctly by sender", testPendingOwnerChangeScalarPropSender("id"));
  test("id is resolved correctly by receiver", testPendingOwnerChangeScalarPropReceiver("id"));
  test("receiver is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2"];

    for (let userId of usersWithAccesIds) {
      const receiverFound = await new Promise((resolve, reject) => {
        PendingOwnerChangeResolver.receiver(
          { id: "mockPendingOwnerChangeId" },
          {},
          { auth: { userId, tokenType: "TEMPORARY" }, ...mockContext }
        )(resolve, reject);
      });

      expect(receiverFound).toMatchObject({ id: mockPendingOwnerChangeData[0].receiverId });
    }
    done();
  });
  test("sender is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2"];

    for (let userId of usersWithAccesIds) {
      const senderFound = await new Promise((resolve, reject) => {
        PendingOwnerChangeResolver.sender(
          { id: "mockPendingOwnerChangeId" },
          {},
          { auth: { userId, tokenType: "TEMPORARY" }, ...mockContext }
        )(resolve, reject);
      });

      expect(senderFound).toMatchObject({ id: mockPendingOwnerChangeData[0].senderId });
    }
    done();
  });
  test("environment is resolved correctly", async done => {
    const usersWithAccesIds = ["mockUserId", "mockUserId2"];

    for (let userId of usersWithAccesIds) {
      const environmentFound = await new Promise((resolve, reject) => {
        PendingOwnerChangeResolver.environment(
          { id: "mockPendingOwnerChangeId" },
          {},
          { auth: { userId, tokenType: "TEMPORARY" }, ...mockContext }
        )(resolve, reject);
      });

      expect(environmentFound).toMatchObject({
        id: mockPendingOwnerChangeData[0].environmentId
      });
    }
    done();
  });

  test("id fails if unauthenticated", testUnauthenticated("id"));
  test("sender fails if unauthenticated", testUnauthenticated("sender"));
  test("receiver fails if unauthenticated", testUnauthenticated("receiver"));
  test("environment fails if unauthenticated", testUnauthenticated("environment"));

  test("id fails if not authorized", testNotAuthorized("id"));
  test("sender fails if not authorized", testNotAuthorized("sender"));
  test("receiver fails if not authorized", testNotAuthorized("receiver"));
  test("environment fails if not authorized", testNotAuthorized("environment"));

  test("id fails if wrong id", testWrongId("id"));
  test("sender fails if wrong id", testWrongId("sender"));
  test("receiver fails if wrong id", testWrongId("receiver"));
  test("environment fails if wrong id", testWrongId("environment"));
});
