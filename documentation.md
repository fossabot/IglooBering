# Documentation

We use GraphQL syntax to handle requests, if you are not familiar with it yet we suggest you read [this quick introduction](https://graphql.org/learn/)

## Sending requests

The GraphQL endpoint is `https://bering.igloo.ooo/graphql`, to send a request you need to send a **POST** request to the endpoint and the body should contain a valid GraphQL query, like this one:

```graphql
mutation {
  signUp(email: "showcase@igloo.io", password: "showcase", name: "Showcase") {
    id
    token
  }
}
```

To authenticate users we use JWT tokens, using tokens we can allow access to the account without needing to store the password, furthermore the token remains valid if the password changes.

You can obtain a temporary one using the `signUp` or the `logIn` mutation, if you need a permanent access token you can use the `createPermanentToken` mutation.

Every authenticated requests should have an `Authorization` header containing the token in the format `Bearer token` (for example `Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiJ9.eyJleHAiOjE1Mjc2ODkzNDQsInVzZXJJZCI6IjdhYjJmMjc4LTNkYWItNDA3Ni05ZDVmLWFlYzc1ZGM4ZDA5NiIsImFjY2Vzc0xldmVsIjoiT1dORVIiLCJ0b2tlblR5cGUiOiJURU1QT1JBUlkifQ.5RaYhrVRnTgByhQMoFvARRQWZWoy3nXWiuTnuu0klsYWJEOV36wVv_4X4bZI9biDhn-gzaCPmscIbSmMdYV_XQ`).
The only unauthenticated methods are `signUp` and `logIn`

## GraphQL API

You can interactively explore our api using the [graphiql docs tool](https://bering.igloo.ooo/graphiql) or you can read [our graphql schema](https://github.com/hellowitlab/iglooHouston/blob/master/graphql/types.graphql) (it's full of comments 😉)

## Database structure

Igloo's database is structured in a hierarchical way: the top-level element is **user**, which contains various **devices**, each device contains various **values** and **notifications**.
