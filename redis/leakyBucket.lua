redis.replicate_commands();
local time = redis.call('TIME')[1];
local oldTime = redis.call('hget', 'leaky_bucket_user_time', KEYS[1]);
if (oldTime == false)
then
    redis.call('hset', 'leaky_bucket_user', KEYS[1], 0);
    redis.call('hset', 'leaky_bucket_user_time', KEYS[1], time);
    return 0
end

local countReduction = ( (time - oldTime) * ARGV[1])
local count = 0

if(countReduction ~= 0)
then
    count = redis.call('hincrby', 'leaky_bucket_user', KEYS[1], - countReduction);
    if( count < 0)
    then
        redis.call('hset', 'leaky_bucket_user', KEYS[1], 0);
        count = 0
    end
else
    count = redis.call('hget','leaky_bucket_user', KEYS[1])
end

redis.call('hset', 'leaky_bucket_user_time', KEYS[1], time);
return count

