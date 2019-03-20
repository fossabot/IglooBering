redis.replicate_commands();
local time = tonumber(redis.call('TIME')[1]);
local oldTime = redis.call('hget', KEYS[2], KEYS[1]);
if (oldTime == false)
then
    redis.call('hset', KEYS[3], KEYS[1], 0);
    redis.call('hset', KEYS[2], KEYS[1], time);
    return 0
end

local countReduction = math.floor(((time - oldTime) * ARGV[1] ) / 3600)

local count = 0

if(countReduction > 0)
then
    count = redis.call('hincrby', KEYS[3], KEYS[1], - countReduction);
    if( count < 0)
    then
        redis.call('hset', KEYS[3], KEYS[1], 0);
        count = 0
    end
    redis.call('hset', KEYS[2], KEYS[1], time);
else
    count = redis.call('hget',KEYS[3], KEYS[1])
end

return count

