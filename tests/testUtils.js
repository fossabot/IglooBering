const testScalarProp = (Resolver, root, mockData) => prop => async () => {
  const propFound = await Resolver[prop](
    root,
    {},
    { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } }
  );

  expect(propFound).toBe(mockData[prop]);
};

const unauthenticatedShouldFail = (Resolver, root) => prop => async () => {
  const promise = Resolver[prop](root, {}, {});

  await expect(promise).rejects.toMatch("You are not authenticated");
};

const wrongIdShouldFail = (Resolver, root, context) => prop => async () => {
  const promise = Resolver[prop](root, {}, context);

  await expect(promise).rejects.toMatch("The requested resource does not exist");
};

module.exports = {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail
};
