import request from "supertest"
import GraphQLServer from "../app.js"
import {generateAuthenticationToken} from "../graphql/resolvers/utilities"

require("dotenv").config()
if (!process.env.JWT_SECRET) {
    throw new Error("Could not load .env")
}

const {JWT_SECRET} = process.env
let self = {}

describe("Device", function() {
    beforeAll(async () => {
        // creating an account to perform all the operations
        const res = await request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .send({
                query: `mutation SignupUser($email: String!, $password: String!) {
                                SignupUser(email: $email, password: $password) {
                                    id
                                    token
                                }
                            }
                        `,
                variables: {
                    email: "userTest2@email.com",
                    password: "password",
                },
            })
        const parsedRes = JSON.parse(res.text)
        expect(parsedRes.data.SignupUser.id).toBeDefined()
        expect(parsedRes.data.SignupUser.token).toBeDefined()
        expect(parsedRes.errors).toBeUndefined()
        self.userId = parsedRes.data.SignupUser.id
        self.token = parsedRes.data.SignupUser.token

        const res2 = await request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .send({
                query: `mutation SignupUser($email: String!, $password: String!) {
                            SignupUser(email: $email, password: $password) {
                                id
                                token
                            }
                        }
                    `,
                variables: {
                    email: "userTest3@email.com",
                    password: "password",
                },
            })
        const parsedRes2 = JSON.parse(res2.text)
        expect(parsedRes2.data.SignupUser.id).toBeDefined()
        expect(parsedRes2.data.SignupUser.token).toBeDefined()
        expect(parsedRes2.errors).toBeUndefined()
        self.userId2 = parsedRes2.data.SignupUser.id
        self.token2 = parsedRes2.data.SignupUser.token
    })

    it("should be able to create a device", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .set("Authorization", "Bearer " + self.token)
            .send({
                query: `mutation CreateDevice($deviceType: String!, $customName: String!, $tags:[String!]!){
                    CreateDevice(deviceType: $deviceType, customName: $customName, tags: $tags){
                        tags,
                        values{
                            id
                        },
                        id,
                        customName,
                        updatedAt,
                        createdAt,
                        deviceType,
                        user{
                            id
                            email
                        }
                    }
                }
                `,
                variables: {
                    deviceType: "Lamp",
                    customName: "Lampada",
                    tags: ["yellow"],
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeUndefined()
                expect(parsedRes.data.CreateDevice.id).toBeDefined()
                expect(parsedRes.data.CreateDevice.updatedAt).toBeDefined()
                expect(parsedRes.data.CreateDevice.createdAt).toBeDefined()
                expect(parsedRes.data.CreateDevice.tags).toEqual(["yellow"])
                expect(parsedRes.data.CreateDevice.values).toEqual([])
                expect(parsedRes.data.CreateDevice.customName).toBe("Lampada")
                expect(parsedRes.data.CreateDevice.deviceType).toBe("Lamp")
                expect(parsedRes.data.CreateDevice.user).toEqual({
                    id: self.userId,
                    email: "userTest2@email.com",
                })
                self.deviceId = parsedRes.data.CreateDevice.id
                done()
            })
    })

    it("should be able to create a device without tags", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .set("Authorization", "Bearer " + self.token2)
            .send({
                query: `mutation CreateDevice($deviceType: String!, $customName: String!, $tags:[String!]!){
                    CreateDevice(deviceType: $deviceType, customName: $customName, tags: $tags){
                        tags,
                        values{
                            id
                        },
                        id,
                        customName,
                        updatedAt,
                        createdAt,
                        deviceType,
                        user{
                            id
                            email
                        }
                    }
                }
                `,
                variables: {
                    deviceType: "Lamp",
                    customName: "Lampada",
                    tags: [],
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeUndefined()
                expect(parsedRes.data.CreateDevice.id).toBeDefined()
                expect(parsedRes.data.CreateDevice.updatedAt).toBeDefined()
                expect(parsedRes.data.CreateDevice.createdAt).toBeDefined()
                expect(parsedRes.data.CreateDevice.tags).toEqual([])
                expect(parsedRes.data.CreateDevice.values).toEqual([])
                expect(parsedRes.data.CreateDevice.customName).toBe("Lampada")
                expect(parsedRes.data.CreateDevice.deviceType).toBe("Lamp")
                expect(parsedRes.data.CreateDevice.user).toEqual({
                    id: self.userId2,
                    email: "userTest3@email.com",
                })
                self.deviceId2 = parsedRes.data.CreateDevice.id
                done()
            })
    })

    it("should not be able to create a device without a token", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .send({
                query: `mutation CreateDevice($deviceType: String!, $customName: String!, $tags:[String!]!){
                    CreateDevice(deviceType: $deviceType, customName: $customName, tags: $tags){
                        id
                    }
                }
                `,
                variables: {
                    deviceType: "Lamp",
                    customName: "Lampada",
                    tags: [],
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeDefined()
                expect(parsedRes.errors[0].message).toBe(
                    "You are not authenticated. Use `AuthenticateUser` to obtain an authentication token"
                )
                done()
            })
    })

    it("should be able to load data about a device", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .set("Authorization", "Bearer " + self.token)
            .send({
                query: `query device($id:ID!){
                            device(id:$id){
                                id
                                updatedAt
                                createdAt
                                customName
                                tags
                                deviceType
                                user{
                                    id
                                    email
                                }
                            }
                        }
                `,
                variables: {
                    id: self.deviceId,
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeUndefined()
                expect(parsedRes.data.device.id).toBe(self.deviceId)
                expect(parsedRes.data.device.updatedAt).toBeDefined()
                expect(parsedRes.data.device.createdAt).toBeDefined()
                expect(parsedRes.data.device.customName).toBe("Lampada")
                expect(parsedRes.data.device.deviceType).toBe("Lamp")
                expect(parsedRes.data.device.tags).toEqual(["yellow"])
                expect(parsedRes.data.device.user).toEqual({
                    id: self.userId,
                    email: "userTest2@email.com",
                })
                done()
            })
    })

    it("should not be able to load a device that doesn't exist", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .set("Authorization", "Bearer " + self.token)
            .send({
                query: `query device($id:ID!){
                            device(id:$id){
                                id
                                updatedAt
                                createdAt
                                customName
                                tags
                                deviceType
                                user{
                                    id
                                    email
                                }
                            }
                        }
                `,
                variables: {
                    id: "aaf5480f-b804-424d-bec8-3f7b363b5519", // wrong ID
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeDefined()
                expect(parsedRes.errors[0].message).toBe(
                    "The requested resource does not exist"
                )
                done()
            })
    })

    it("should not be able to load a device without a token", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .send({
                query: `query device($id:ID!){
                            device(id:$id){
                                id
                                updatedAt
                                createdAt
                                customName
                                tags
                                deviceType
                                user{
                                    id
                                    email
                                }
                            }
                        }
                `,
                variables: {
                    id: self.deviceId, // wrong ID
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeDefined()
                expect(parsedRes.errors[0].message).toBe(
                    "You are not authenticated. Use `AuthenticateUser` to obtain an authentication token"
                )
                done()
            })
    })

    it("should not be able to load a device owned by someone else", done => {
        request(GraphQLServer)
            .post("/graphql")
            .set("content-type", "application/json")
            .set("accept", "application/json")
            .set("Authorization", "Bearer " + self.token) // token of user 1
            .send({
                query: `query device($id:ID!){
                            device(id:$id){
                                id
                                updatedAt
                                createdAt
                                customName
                                tags
                                deviceType
                                user{
                                    id
                                    email
                                }
                            }
                        }
                `,
                variables: {
                    id: self.deviceId2, // device owned by user 2
                },
            })
            .then(res => {
                const parsedRes = JSON.parse(res.text)
                expect(parsedRes.errors).toBeDefined()
                expect(parsedRes.errors[0].message).toBe(
                    "You are not allowed to access details about this resource"
                )
                done()
            })
    })
})
