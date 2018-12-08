const testScalarProp = (Resolver, root, mockData) => prop => async () => {
  const propFound = await new Promise((resolve, reject) => {
    Resolver[prop](root, {}, { auth: { userId: "mockUserId", tokenType: "TEMPORARY" } })(
      resolve,
      reject
    );
  });

  expect(propFound).toBe(mockData[prop]);
};

const unauthenticatedShouldFail = (Resolver, root) => prop => async () => {
  const promise = new Promise((resolve, reject) => {
    Resolver[prop](root, {}, {})(resolve, reject);
  });

  await expect(promise).rejects.toMatch("You are not authenticated");
};

const notAuthorizedShouldFail = (Resolver, root, context) => prop => async () => {
  const promise = new Promise((resolve, reject) => {
    Resolver[prop](root, {}, context)(resolve, reject);
  });

  await expect(promise).rejects.toMatch("You are not allowed to perform this operation");
};

const wrongIdShouldFail = (Resolver, root, context) => prop => async () => {
  const promise = new Promise((resolve, reject) => {
    Resolver[prop](root, {}, context)(resolve, reject);
  });

  await expect(promise).rejects.toMatch("The requested resource does not exist");
};

module.exports = {
  testScalarProp,
  unauthenticatedShouldFail,
  wrongIdShouldFail,
  notAuthorizedShouldFail
};
