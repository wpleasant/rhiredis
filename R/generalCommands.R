#### General Commands ####

redisMulti <- function()
{ 
  redis$exec("MULTI",TRUE)
}

redisExec <- function()
{
  redis$exec("EXEC",TRUE)
}

redisDiscard <- function()
{
  redis$exec("DISCARD",TRUE)
}

redisWatch <- function(keys)
{
  if(length(keys)>1){
    keys <- as.list(keys)
    for(i in keys) redis$exec(paste("WATCH",i),TRUE)
  } else {
    redis$exec(paste("WATCH",keys),TRUE)
  }
}

redisUnwatch <- function(keys)
{
  if(length(keys)>1){
    keys <- as.list(keys)
    for(i in keys) redis$exec(paste("UNWATCH",i),TRUE)
  } else {
    redis$exec(paste("UNWATCH",keys),TRUE)
  }
}

redisExists <- function(key) 
{
   redis$exec(paste("EXISTS",key),TRUE)
}

redisDelete <- function(keys) 
{
  if(length(keys)>1) {
    if(is.list(keys)) keys <- unlist(keys)
    keys <- paste0(keys,collapse=" ")
  }
  redis$exec(paste("DEL",keys),TRUE)
}

redisType <- function(key) 
{
  redis$exec(paste("TYPE",key),TRUE)
}

redisKeys <- function(pattern="*") 
{
  res <- redis$exec(paste("KEYS",pattern),TRUE)
  unlist(res)
}

redisRandomKey <- function() 
{
  redis$exec("RANDOMKEY")
}

redisRename <- function(old, new) 
{
  redis$exec(paste("RENAME",old,new),TRUE)
}

redisRenameX <- function(old, new) 
{
  redis$exec(paste("RENAMEX",old,new),TRUE)
}

redisPexpire <- function(key, milliseconds)
{
   redis$exec(paste("PEXPIRE",key,milliseconds),TRUE)
}

redisPexpireAt <- function(key, time)
{
   if(missing(time)) stop("Please provide time")
   redis$exec(paste("PEXPIREAT",key,as.character(time)),TRUE)
}

redisPTTL <- function(key)
{
   redis$exec(paste("PTTL",key),TRUE)
}

redisPersist <- function(key)
{
   redis$exec(paste("PERSIST",key),TRUE)
}

redisExpire <- function(key, seconds) 
{
   redis$exec(paste("EXPIRE",key,seconds),TRUE)
}

redisExpireAt <- function(key, time) 
{ 
  if(missing(time)) stop("Please provide time")
  if(is.character(time)) time <- as.POSIXct(time)
  redis$exec(paste("EXPIREAT",key,as.numeric(time)),TRUE)
}

redisTTL <- function(key) 
{
  redis$exec(paste("TTL",key),TRUE)
}

redisMove <- function(key, dbindex) 
{
  redis$exec(paste("MOVE",key,dbindex),TRUE)
}

redisQuit <- redisClose <- function()
{
  redis$exec("QUIT",TRUE)
}

redisAuth <- function(pwd)
{
  redis$exec(paste("AUTH", pwd),TRUE)
}

redisSave <- function()
{
  redis$exec("SAVE",TRUE)
}

redisBgSave <- function()
{
  redis$exec('BGSAVE',TRUE)
}

redisBgRewriteAOF <- function()
{
  redis$exec('BGREWRITEAOF',TRUE)
}

redisShutDown <- function(save=TRUE)
{  
  s <- if(save) "SAVE" else "NOSAVE"
  redis$exec(paste('SHUTDOWN',s),TRUE)
}

### POSIX == TRUE returns the POSIXct time stamp with microseconds
#   FALSE returns the default list cast to numerics
redisTime <- function(POSIX=TRUE)
{
  x <- redis$exec('TIME',TRUE)
  if(POSIX) {
    .POSIXct(as.numeric(x[[1]])+as.numeric(x[[2]])*.00001)
  } else {
    x[[1]] <- as.numeric(x[[1]])
    x[[2]] <- as.numeric(x[[2]])
    x
  }
}
# copied directly from rredis  
redisInfo <- function()
{
  x <- .redis$exec('INFO',TRUE)
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
  redis$exec(paste('SLAVEOF',host,port),TRUE)
}

redisFlushDB <- function() {
  redis$exec('FLUSHDB',TRUE)
}

redisFlushAll <- function() {
  redis$exec('FLUSHALL',TRUE)
}

redisSelect <- function(index) {
  if(missing(index)) stop("Which index should we select?")
  redis$exec(paste('SELECT',index),TRUE)
}

redisPing <- function() {
  redis$exec('PING',TRUE)
}

redisDBSize <- function() {
  redis$exec('DBSIZE',TRUE)
}


