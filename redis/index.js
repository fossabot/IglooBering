const redis = require("redis")
const { promisify } = require("util")
const fs = require("fs")
const { join } = require("path")
require("dotenv").config()

const client = redis.createClient(process.env.REDIS_URL)

const hincrby = promisify(client.hincrby).bind(client)
const script = promisify(client.script).bind(client)
const evalsha = promisify(client.evalsha).bind(client)

const leakyBucketScript = fs.readFileSync("./redis/leakyBucket.lua", "utf8")

client.on("error", function(err) {
  console.log("Error " + err)
})

const BUCKET_SIZE = 5000
const FLUSH_PER_SECOND = 100

let hash

async function isUserBlocked(userId) {
  if (!hash) {
    hash = await script("LOAD", leakyBucketScript)
  }
  const currVal = await evalsha(hash, 1, userId, FLUSH_PER_SECOND)
  console.log(currVal)

  if (currVal >= BUCKET_SIZE) {
    return true
  } else {
    return false
  }
}

function increaseUserAccessCount(userId, delta = 1) {
  return hincrby("leaky_bucket_user", userId, delta)
}

module.exports = {
  isUserBlocked,
  increaseUserAccessCount,
}
