import { GraphQLInt } from "graphql"
const validator = require("is-my-uuid-valid")

export default {
  __serialize: value => {
    return value
  },

  __parseValue: value => {
    return value
  },

  __parseLiteral: ast => {
    const value = GraphQLInt.parseLiteral(ast)
    if (value < 0) {
      throw new Error("should be positive") // graphql will automatically build the error message
    } else {
      return value
    }
  },
}
