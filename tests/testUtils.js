const testScalarProp = (Resolver, query, mockData) => prop => async () => {
  const propFound = await Resolver[prop](
    query,
    {},
    { auth: { userId: 'mockUserId', tokenType: 'TEMPORARY' } },
  )

  expect(propFound).toBe(mockData[prop])
}

const unauthenticatedShouldFail = (Resolver, query) => prop => async () => {
  const promise = Resolver[prop](query, {}, {})

  await expect(promise).rejects.toMatch('You are not authenticated')
}

module.exports = {
  testScalarProp,
  unauthenticatedShouldFail,
}
