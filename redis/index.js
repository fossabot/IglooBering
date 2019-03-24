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

let hash

async function isUserBlocked(userId) {
  const USER_BUCKET_SIZE = 13200
  const USER_FLUSH_PER_HOUR = 600 * 100 // FIXME: this is temporary
  if (!hash) {
    hash = await script("LOAD", leakyBucketScript)
  }
  const currVal = await evalsha(
    hash,
    3,
    userId,
    "leaky_bucket_user_time",
    "leaky_bucket_user",
    USER_FLUSH_PER_HOUR
  )

  if (currVal >= USER_BUCKET_SIZE) {
    return true
  } else {
    return false
  }
}

function increaseUserAccessCount(userId, delta = 1) {
  return hincrby("leaky_bucket_user", userId, delta)
}

async function isIpBlocked(ip) {
  const IP_BUCKET_SIZE = 13200
  const IP_FLUSH_PER_HOUR = 600 * 100 // FIXME: this is temporary
  if (!hash) {
    hash = await script("LOAD", leakyBucketScript)
  }
  const currVal = await evalsha(
    hash,
    3,
    ip,
    "leaky_bucket_ip_time",
    "leaky_bucket_ip",
    IP_FLUSH_PER_HOUR
  )

  if (currVal >= IP_BUCKET_SIZE) {
    return true
  } else {
    return false
  }
}

function increaseIpAccessCount(ip, delta = 1) {
  return hincrby("leaky_bucket_ip", ip, delta)
}

async function isDeviceBlocked(deviceId) {
  const DEVICE_BUCKET_SIZE = 13200
  const DEVICE_FLUSH_PER_HOUR = 600 * 100 // FIXME: this is temporary
  if (!hash) {
    hash = await script("LOAD", leakyBucketScript)
  }
  const currVal = await evalsha(
    hash,
    3,
    deviceId,
    "leaky_bucket_device_time",
    "leaky_bucket_device",
    DEVICE_FLUSH_PER_HOUR
  )

  if (currVal >= DEVICE_BUCKET_SIZE) {
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
