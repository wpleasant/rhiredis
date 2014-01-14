#### General Commands ####

redisMulti <- function()
{ 
  redisCMD("MULTI",rc=TRUE)
}

redisExec <- function()
{
  redisCMD("EXEC",rc=TRUE)
}

redisDiscard <- function()
{
  redisCMD("DISCARD",rc=TRUE)
}

redisWatch <- function(keys)
{
  redisCMD("WATCH",as.list(keys),rc=TRUE)
}

redisUnwatch <- function(keys)
{
    redisCMD("UNWATCH",as.list(keys),rc=TRUE)
}

redisExists <- function(key) 
{
   redisCMD("EXISTS",list(key),rc=TRUE)
}

redisDelete <- function(keys) 
{
  redisCMD("DEL",as.list(keys),rc=TRUE)
}

redisType <- function(key) 
{
  redisCMD("TYPE",list(key),rc=TRUE)
}

redisKeys <- function(pattern="*") 
{
  res <- redisCMD("KEYS",list(pattern),rc=TRUE)
  unlist(res)
}

redisRandomKey <- function() 
{
  redisCMD("RANDOMKEY",rc=TRUE)
}

redisRename <- function(old, new) 
{
  redisCMD("RENAME",list(old,new),rc=TRUE)
}

redisRenameX <- function(old, new) 
{
  redisCMD("RENAMEX",list(old,new),rc=TRUE)
}

redisPexpire <- function(key, milliseconds)
{
   redis$exec("PEXPIRE",list(key,milliseconds),rc=TRUE)
}

redisPexpireAt <- function(key, time)
{
   if(missing(time)) stop("Please provide time")
   redisCMD("PEXPIREAT",list(key,as.character(time)),rc=TRUE)
}

redisPTTL <- function(key)
{
   redisCMD("PTTL",list(key),rc=TRUE)
}

redisPersist <- function(key)
{
   redisCMD("PERSIST",list(key),rc=TRUE)
}

redisExpire <- function(key, seconds) 
{
   redisCMD("EXPIRE",list(key,seconds),rc=TRUE)
}

redisExpireAt <- function(key, time) 
{ 
  if(missing(time)) stop("Please provide time")
  if(is.character(time)) time <- as.POSIXct(time)
  redisCMD("EXPIREAT",list(key,as.numeric(time)),rc=TRUE)
}

redisTTL <- function(key) 
{
  redisCMD("TTL",list(key),rc=TRUE)
}

redisMove <- function(key, dbindex) 
{
  redisCMD("MOVE",list(key,dbindex),rc=TRUE)
}

redisQuit <- redisClose <- function()
{
  redisCMD("QUIT",rc=TRUE)
}

redisAuth <- function(pwd)
{
  redisCMD("AUTH", list(pwd),rc=TRUE)
}

redisSave <- function()
{
  redisCMD("SAVE",rc=TRUE)
}

redisBgSave <- function()
{
  redisCMD('BGSAVE',rc=TRUE)
}

redisBgRewriteAOF <- function()
{
  redisCMD('BGREWRITEAOF',rc=TRUE)
}

redisShutDown <- function(save=TRUE)
{  
  s <- if(save) "SAVE" else "NOSAVE"
  redisCMD('SHUTDOWN',list(s),rc=TRUE)
}

### POSIX == TRUE returns the POSIXct time stamp with microseconds
#   FALSE returns the default list cast to numerics
redisTime <- function(POSIX=TRUE)
{
  x <- redisCMD('TIME',rc=TRUE)
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
  x <- redisCMD('INFO',rc=TRUE)
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
  redisCMD('SLAVEOF',list(host,port),rc=TRUE)
}

redisFlushDB <- function() {
  redisCMD('FLUSHDB',rc=TRUE)
}

redisFlushAll <- function() {
  redisCMD('FLUSHALL',rc=TRUE)
}

redisSelect <- function(index) {
  if(missing(index)) stop("Which index should we select?")
  redisCMD('SELECT',list(index),rc=TRUE)
}

redisPing <- function() {
  redisCMD('PING',rc=TRUE)
}

redisDBSize <- function() {
  redisCMD('DBSIZE',rc=TRUE)
}


