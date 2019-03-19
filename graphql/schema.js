import { makeExecutableSchema } from "graphql-tools"
import fs from "fs"
import {
  DirectiveLocation,
  GraphQLString,
  GraphQLInt,
  GraphQLList,
} from "graphql"
import resolvers from "./resolvers"
import {
  GraphQLCustomDirective,
  applySchemaCustomDirectives,
} from "graphql-custom-directive"
import { isNumber } from "util"
import { convert } from "./utilities"

const GraphQLCustomDuplicateDirective = new GraphQLCustomDirective({
  name: "convertTo",
  description: "convert to chosen unit",
  args: {
    unit: { type: GraphQLString, description: "desired output unit" },
  },
  locations: [DirectiveLocation.FIELD],
  resolve(resolve, root, args, context, info) {
    if (info.fieldName !== "value") {
      throw new Error(
        "@convertTo directive can only be used on the value field"
      )
    }
    if (!args.unit) {
      throw new Error("unit field is required on @convertTo directive")
    }
    if (!root.unitOfMeasurement) {
      throw new Error("this resource does not have a unit")
    }

    return resolve().then(result => {
      if (result === null) return null
      if (!isNumber(result)) {
        throw new Error(
          "@convertTo directive can only be used on numeric fields"
        )
      }

      return convert(result, root.unitOfMeasurement, args.unit)
    })
  },
})

const typeDefs = fs.readFileSync("./graphql/types.graphql").toString()
const schema = makeExecutableSchema({
  typeDefs,
  resolvers,
})

schema._directives.push.apply(schema._directives, [
  GraphQLCustomDuplicateDirective,
])
applySchemaCustomDirectives(schema)

module.exports = schema
