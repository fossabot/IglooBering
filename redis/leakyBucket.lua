redis.replicate_commands();
local time = redis.call('TIME')[1];
local oldTime = redis.call('hget', KEYS[2], KEYS[1]);
if (oldTime == false)
then
    redis.call('hset', KEYS[3], KEYS[1], 0);
    redis.call('hset', KEYS[2], KEYS[1], time);
    return 0
end

local countReduction = ( (time - oldTime) * ARGV[1])
local count = 0

if(countReduction ~= 0)
then
    count = redis.call('hincrby', KEYS[3], KEYS[1], - countReduction);
    if( count < 0)
    then
        redis.call('hset', KEYS[3], KEYS[1], 0);
        count = 0
    end
else
    count = redis.call('hget',KEYS[3], KEYS[1])
end

redis.call('hset', KEYS[2], KEYS[1], time);
return count

