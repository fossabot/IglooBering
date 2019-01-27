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

    expect(resolveTypeFound).toBe("FloatValue");
    done();
  });
  test("__resolveType is resolved correctly for StringValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockStringValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toBe("StringValue");
    done();
  });
  test("__resolveType is resolved correctly for BooleanValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockBooleanValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toBe("BooleanValue");
    done();
  });
  test("__resolveType is resolved correctly for MapValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockMapValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toBe("MapValue");
    done();
  });
  test("__resolveType is resolved correctly for PlotValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockPlotValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toBe("PlotValue");
    done();
  });
  test("__resolveType is resolved correctly for CategoryPlotValue", async done => {
    const resolveTypeFound = await new Promise((resolve, reject) =>
      ValueResolver.__resolveType(
        { id: "mockCategoryPlotValueId" },
        { auth: { userId: "mockUserId", tokenType: "TEMPORARY" }, ...mockContext }
      )(resolve, reject)
    );

    expect(resolveTypeFound).toBe("CategoryPlotValue");
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
