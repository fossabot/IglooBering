import { makeExecutableSchema } from "graphql-tools"
import fs from "fs"
import { DirectiveLocation } from "graphql"
import {
  GraphQLCustomDirective,
  applySchemaCustomDirectives,
} from "graphql-custom-directive"
import resolvers from "./resolvers"

const typeDefs = fs.readFileSync("./graphql/types.graphql").toString()
const schema = makeExecutableSchema({ typeDefs, resolvers })

const ToUpperDirective = new GraphQLCustomDirective({
  name: "toUpperCase",
  description: "change the case of a string to uppercase",
  locations: [DirectiveLocation.FIELD],
  resolve(resolve, root) {
    return resolve().then(result => {
      if (!result.toUpperCase)
        throw new Error("toUpperCase directive is only available on strings")
      return result.toUpperCase ? result.toUpperCase() : result
    })
  },
})

schema._directives.push.apply(schema._directives, [ToUpperDirective])
applySchemaCustomDirectives(schema)

export default schema
