import * as GraphQL from "graphql"
const validator = require("is-my-uuid-valid")
const validateUUID = validator({})

export default {
  __serialize: value => {
    return value
  },

  __parseValue: value => {
    return value
  },

  __parseLiteral: ast => {
    const value = GraphQL.GraphQLString.parseLiteral(ast)
    if (!validateUUID(value)) {
      throw new Error() // graphql will automatically build the error message
    } else {
      return value
    }
  },
}
