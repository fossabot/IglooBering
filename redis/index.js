const redis = require("redis")
const { promisify } = require("util")
const fs = require("fs")
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
  const currVal = await evalsha(
    hash,
    3,
    userId,
    "leaky_bucket_user_time",
    "leaky_bucket_user",
    FLUSH_PER_SECOND
  )

  if (currVal >= BUCKET_SIZE) {
    return true
  } else {
    return false
  }
}

function increaseUserAccessCount(userId, delta = 1) {
  return hincrby("leaky_bucket_user", userId, delta)
}

async function isIpBlocked(ip) {
  if (!hash) {
    hash = await script("LOAD", leakyBucketScript)
  }
  const currVal = await evalsha(
    hash,
    3,
    ip,
    "leaky_bucket_ip_time",
    "leaky_bucket_ip",
    FLUSH_PER_SECOND
  )

  if (currVal >= BUCKET_SIZE) {
    return true
  } else {
    return false
  }
}

function increaseIpAccessCount(ip, delta = 1) {
  return hincrby("leaky_bucket_ip", ip, delta)
}

async function isDeviceBlocked(deviceId) {
  if (!hash) {
    hash = await script("LOAD", leakyBucketScript)
  }
  const currVal = await evalsha(
    hash,
    3,
    deviceId,
    "leaky_bucket_device_time",
    "leaky_bucket_device",
    FLUSH_PER_SECOND
  )

  if (currVal >= BUCKET_SIZE) {
    return true
  } else {
    return false
  }
}

function increaseDeviceAccessCount(deviceId, delta = 1) {
  return hincrby("leaky_bucket_device", deviceId, delta)
}

module.exports = {
  isUserBlocked,
  increaseUserAccessCount,
  isIpBlocked,
  increaseIpAccessCount,
  isDeviceBlocked,
  increaseDeviceAccessCount,
}
