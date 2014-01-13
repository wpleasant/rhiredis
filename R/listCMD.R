
rawCheck <- function(...)
{
  x <- list(...) 
  if("raw" %in% names(x))
    return(!x$raw)
  else return(TRUE)
}

toChar   <- function(x)
{
  rawToChar(serialize(x,NULL,ascii=TRUE)) 
}

.fromChar <- function(x)
{
  unserialize(charToRaw(x))
}

fromChar <- function(x)
{ 
  if(is.list(x)) {
    #for(i in seq_along(x)) x[[i]] <- unserialize(charToRaw(x[[i]]))
      lapply(x,function(i) unserialize(charToRaw(i)))
  } else {
      unserialize(charToRaw(x))
  } 
}

##### list commands #####

redisRPush <- function(key, value)
{
  redis$cmd("RPUSH",key,value,FALSE)
}

redisLPush <- function(key, value)
{
  redis$cmd("LPUSH",key,value,FALSE)
}

redisRPop <- function(key,fromChar=FALSE)
{
  redis$cmd("RPOP",key,NULL,fromChar)
}


redisLPop <- function(key,...)
{
  redis$cmd("LPOP",key,NULL,fromChar)
}

redisLLen <- function(key)
{
  redis$cmd("LLEN",as.character(key),NULL,TRUE)
}

redisLRange <- function(key, start, end, ...)
{
  if(missing(start) || missing(end))
    stop(" start and end must be provided\n")
    redis$exec(paste("LRANGE",key,start,end),FALSE)
}

redisLTrim <- function(key,start,end)
{ if(missing(start) || missing(end))
    stop(" start and end must be provided\n")
  redis$exec(paste("LTRIM",key,start,end))
}

redisLIndex <- function(key, index, ...)
{
  redis$exec(paste("LINDEX",key,index),FALSE)
}

redisLSet <- function(key, index, value)
{
  redis$exec(paste("LSET",key,index,toChar(value)))
}

redisLRem <- function(key, count, value)
{
  redis$exec(paste("LREM",key,count,toChar(value)))
}

redisRPopLPush <- function(src, dest, ...)
{
  x <- redis$exec(paste("RPOPLPUSH",src,dest))
  if(rawCheck(...)) { 
    x <- fromChar(x)
  }
  x
}

redisBRPop <- function(keys, timeout=0, ...)
{
  if(length(keys)>1) keys <- paste0(keys,collapse=" ")
  x <- redis$exec(paste("BRPOP",keys,timeout))
  if(rawCheck(...)) { 
    x[[2]] <- fromChar(x[[2]])
  }
  x
}

redisBLPop <- function(keys, timeout=0, ...)
{
  if(length(keys)>1) keys <- paste0(keys,collapse=" ")
  x <- redis$exec(paste("BLPOP",keys,timeout))
  if(rawCheck(...)) { 
    x[[2]] <- fromChar(x[[2]])
  }
  x
}

redisBRPopLPush <- function(src, dest, timeout=0, ...)
{
  x <- redis$exec(paste("BRPOPLPUSH",src,dest,timeout))
  if(rawCheck(...)) { 
    x <- fromChar(x)
  }
  x
}

redisLInsert <- function(key, BoF="BEFORE", value)
{
  redis$exec(paste("LINSERT",key,BoF,toChar(value)))
}

redisRPushX <- function(key, value)
{
  redis$exec(paste("RPUSHX",key,toChar(value)))
}

redisLPushX <- function(key, value)
{
  redis$exec(paste("LPUSHX",key,toChar(value)))
}

####### General Commands

redisMulti <- function()
{ 
  redis$exec("MULTI")
}

redisExec <- function()
{
  redis$exec("EXEC")
}

redisDiscard <- function()
{
  redis$exec("DISCARD")
}

redisWatch <- function(keys)
{
  if(length(keys)>1){
    keys <- as.list(keys)
    for(i in keys) redis$exec(paste("WATCH",i))
  } else {
    redis$exec(paste("WATCH",keys))
  }
}

redisUnwatch <- function(keys)
{
  if(length(keys)>1){
    keys <- as.list(keys)
    for(i in keys) redis$exec(paste("UNWATCH",i))
  } else {
    redis$exec(paste("UNWATCH",keys))
  }
}

redisExists <- function(key) 
{
   redis$exec(paste("EXISTS",key))
}

redisDelete <- function(keys) 
{
  if(length(keys)>1) {
    if(is.list(keys)) keys <- unlist(keys)
    keys <- paste0(keys,collapse=" ")
  }
  redis$exec(paste("DEL",keys))
}

redisType <- function(key) 
{
  redis$exec(paste("TYPE",key))
}

redisKeys <- function(pattern="*") 
{
  res <- redis$exec(paste("KEYS",pattern))
  unlist(res)
}

redisRandomKey <- function() 
{
  redis$exec("RANDOMKEY")
}

redisRename <- function(old, new) 
{
  redis$exec(paste("RENAME",old,new))
}
redisRenameX <- function(old, new) 
{
  redis$exec(paste("RENAMEX",old,new))
}

redisPexpire <- function(key, milliseconds)
{
   redis$exec(paste("PEXPIRE",key,milliseconds))
}

redisPexpireAt <- function(key, time)
{
   if(missing(time)) stop("Please provide time")
   redis$exec(paste("PEXPIREAT",key,as.character(time)))
}

redisPTTL <- function(key)
{
   redis$exec(paste("PTTL",key))
}

redisPersist <- function(key)
{
   redis$exec(paste("PERSIST",key))
}

redisExpire <- function(key, seconds) 
{
   redis$exec(paste("EXPIRE",key,seconds))
}

redisExpireAt <- function(key, time) 
{ 
  if(missing(time)) stop("Please provide time")
  if(is.character(time)) time <- as.POSIXct(time)
  redis$exec(paste("EXPIREAT",key,as.numeric(time)))
}

redisTTL <- function(key) 
{
  redis$exec(paste("TTL",key))
}

redisMove <- function(key, dbindex) 
{
  redis$exec(paste("MOVE",key,dbindex))
}

#####  String setting functions ####


redisGet <- function(key, ...)
{
  x <- redis$exec(paste("GET",key))
  if(rawCheck(...)) { 
    x <- fromChar(x)
  }
  x
}

redisSet <- function(key, value)
{
  redis$exec(paste("SET",key,toChar(value)))
}

redisSetNX <- function(key, value)
{
  redis$exec(paste("SETNX",key,toChar(value)))
}


redisGetSet <- function(key, value, ...)
{
  x <- redis$exec(paste("GETSET",key,toChar(value)))
  if(rawCheck(...)) { 
    x <- fromChar(x)
  }
  x
}

redisMGet <- function(keys, ...)
{
  .keys <- if(is.list(keys)) unlist(keys) else keys
  if(length(keys)>1) {
    if(is.list(keys)) keys <- unlist(keys)
    keys <- paste0(keys,collapse=" ")
  }
  x <- redis$exec(paste("MGET",keys))
  if(rawCheck(...)) { 
    x <- fromChar(x)
  }
  if(length(x) == length(.keys)) names(x) <- .keys
  x
}

#### This may change to true set value but I beleve there is a limit on
#       the number of arguments one can place in a hiredis' stardard command
redisMSet <- function(keyvalues)
{ 
  na <- names(keyvalues)
  if(is.null(na)) stop("keyvalues must be a named list\n")
  x <- lapply(na, function(i){
      redis$exec(paste("SET",i,toChar(keyvalues[i])))
  })
  if(all(x=="OK")) 1 else 0
}

redisMSetNX <- function(keyvalues)
{ 
  na <- names(keyvalues)
  if(is.null(na)) stop("keyvalues must be a named list\n")
  x <- lapply(na, function(i){
      redis$exec(paste("SETNX",i,toChar(keyvalues[i])))
  })
  if(all(x)) 1 else 0
}
redisGetRange <- function(key,start,end,...){
    if(missing(start)|| missing(end)) {
        stop("plese provide start and/or end")
    }
    x <- redis$exec(paste("GETRANGE",key,start,end))
    if(rawCheck(...)) { 
      x <- fromChar(x)
    }
    x
}

redisStrlen <- function(key)
{
  redis$exec(paste("STRLEN",key))
}

redisIncr <- function(key)
{
  redis$exec(paste("INCR",key))
}

redisIncrBy <- function(key, value)
{
  redis$exec(paste("INCRBY",value))
}

redisIncrByFloat <- function(key, value)
{
  redis$exec(paste("INCRBYFLOAT",key,value))
}

redisDecrBy <- function(key, value)
{
  redis$exec(paste("DECRBY",key,value))
}

redisDecr <- function(key)
{
  redis$exec(paste("DECR",key))
}

redisAppend <- function(key, value){
  redis$exec(paste("APPEND",key,value))
}

redisQuit <- redisClose <- function()
{
  redis$exec("QUIT")
}

redisAuth <- function(pwd)
{
  redis$exec(paste("AUTH", pwd))
}

redisSave <- function()
{
  redis$exec("SAVE")
}

redisBgSave <- function()
{
  redis$exec('BGSAVE')
}

redisBgRewriteAOF <- function()
{
  redis$exec('BGREWRITEAOF')
}

redisShutDown <- function(save=TRUE)
{  s <- if(save) "SAVE" else "NOSAVE"
  redis$exec(paste('SHUTDOWN',s))
}

### POSIX == TRUE returns the POSIXct time stamp with microseconds
#   FALSE returns the default list cast to numerics
redisTime <- function(POSIX=TRUE)
{
  x <- redis$exec('TIME')
  if(POSIX) {
    .POSIXct(as.numeric(x[[1]])+as.numeric(x[[2]])*.00001)
  } else {
    x[[1]] <- as.numeric(x[[1]])
    x[[2]] <- as.numeric(x[[2]])
    x
  }
}

redisInfo <- function()
{
  x <- .redis$exec('INFO')
  z <- strsplit(x,'\r\n')[[1]]
  rj <- c(grep("^$",z), grep("^#",z))
  if(length(rj)>0) z <- z[-rj]
  w <- unlist(lapply(z,strsplit,':'))
  n <- length(w)
  e <- seq.int(from=2,to=n,by=2)
  o <- seq.int(from=1,to=n,by=2)
  z <- as.list(w[e])
  names(z) <- w[o]
  z
}

redisSlaveOf <- function(host,port)
{
# Use host="no" port="one" to disable slave replication # from b.w.lewis
  redis$exec(paste('SLAVEOF',host,port))
}

redisFlushDB <- function() {
  redis$exec('FLUSHDB')
}

redisFlushAll <- function() {
  redis$exec('FLUSHALL')
}

redisSelect <- function(index) {
  if(missing(index)) stop("Which index should we select?")
  redis$exec(paste('SELECT',index))
}

redisPing <- function() {
  redis$exec('PING')
}

redisDBSize <- function() {
  redis$exec('DBSIZE')
}


