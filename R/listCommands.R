##### list commands #####
redisCMD <- function(cmd,ocmds=NULL,value=NULL,rc=FALSE,bl=FALSE){
  if(!is.null(ocmds) && !is.list(ocmds)) ocmds <- as.list(ocmds) 
  redis$cmd(cmd,ocmds,value,rc,bl)
}

redisRPush <- function(key, value, rc=FALSE, bl=FALSE)
{
  redisCMD("RPUSH",key, value, rc, bl)
}

redisLPush <- function(key, value, rc=FALSE, bl=FALSE)
{
  redisCMD("LPUSH",key,value,rc,bl)
}

redisRPop <- function(key,rc=FALSE)
{
   redisCMD("RPOP",key,NULL,rc,FALSE)
}

redisLPop <- function(key,rc=FALSE)
{
  redisCMD("LPOP",key,rc=rc)
}

redisLLen <- function(key)
{
  redisCMD("LLEN",key)
}

redisLRange <- function(key, start, end, rc=FALSE)
{
  if(missing(start) || missing(end))
    stop(" start and end must be provided\n")
  redisCMD("LRANGE",list(key,start,end),rc=rc)
}

redisLTrim <- function(key,start,end)
{ if(missing(start) || missing(end))
    stop(" start and end must be provided\n")
  redisCMD("LTRIM",list(key,start,end))
}

redisLIndex <- function(key, index, rc=FALSE)
{
  redisCMD("LINDEX",list(key,index),rc=rc)
}

redisLSet <- function(key, index, value,rc=FALSE)
{
  redisCMD("LSET",list(key,index),value,rc=rc)
}

redisLRem <- function(key, count, value)
{
  redisCMD("LREM",list(key,list(count,value)))
}

redisRPopLPush <- function(src, dest, value)
{
  redisCMD("RPOPLPUSH",list(src,dest),value)
}

redisBRPop <- function(keys, timeout=0, rc=FALSE)
{
  redisCMD("BRPOP",c(as.list(keys),timeout),rc=rc)
}

redisBLPop <- function(keys, timeout=0, rc=FALSE)
{
  redisCMD("BLPOP",c(as.list(keys),timeout),rc=rc)
}

redisBRPopLPush <- function(src, dest, timeout=0,rc=FALSE)
{
  redisCMD("BRPOPLPUSH",list(src,dest,timeout))
}

redisLInsert <- function(key, BoF="BEFORE", value)
{
  redisCMD("LINSERT",list(key,BoF),value)
}

redisRPushX <- function(key, value)
{
  redisCMD("RPUSHX",list(key),value)
}

redisLPushX <- function(key, value)
{
  redisCMD("LPUSHX",list(key),value)
}

