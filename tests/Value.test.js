import ValueResolver from "../graphql/resolvers/Value";
import MocksGenerator from "./mockUtils";

const { mockContext } = MocksGenerator();

describe("Value", () => {
  test("__resolveType is resolved correctly for FloatValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockFloatValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toEqual("FloatValue");
    done();
  });
  test("__resolveType is resolved correctly for StringValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockStringValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toEqual("StringValue");
    done();
  });
  test("__resolveType is resolved correctly for BooleanValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockBooleanValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toEqual("BooleanValue");
    done();
  });
  test("__resolveType is resolved correctly for FloatSeriesValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockFloatSeriesValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toEqual("FloatSeriesValue");
    done();
  });
  test("__resolveType is resolved correctly for CategorySeriesValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockCategorySeriesValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toEqual("CategorySeriesValue");
    done();
  });

  // not using a for loop because this syntax integrates better with the IDE
  test("__resolveType fails if unauthenticated", async done => {
    const resolveTypeFound = new Promise((resolve, reject) =>
      ValueResolver.__resolveType({ id: "mockFloatValueId" }, { ...mockContext })(resolve, reject)
    );

    expect(resolveTypeFound).rejects.toMatch("You are not authenticated");
    done();
  });

  // not using a for loop because this syntax integrates better with the IDE
  test("__resolveType fails if not authorized", async done => {
    const resolveTypeFound = new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockFloatValueId" },
        { auth: { userId: "mockUserId4", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).rejects.toMatch("You are not allowed to perform this operation");
    done();
  });

  // not using a for loop because this syntax integrates better with the IDE
  test("__resolveType fails if wrong id", async done => {
    const resolveTypeFound = new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "wrongValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).rejects.toMatch("The requested resource does not exist");
    done();
  });
});
