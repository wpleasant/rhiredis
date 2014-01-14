#####  String Commands ####

redisGet <- function(key,rc=FALSE)
{
  redisCMD("GET",list(key),rc=rc)
}

redisSet <- function(key, value)
{
  redisCMD("SET",list(key),value)
}

redisSetNX <- function(key, value)
{
  redisCMD("SETNX",list(key),value)
}


redisGetSet <- function(key, value, rc=FALSE)
{
  redisCMD("GETSET", list(key),value,rc=rc)
}

redisMGet <- function(keys, rc=FALSE)
{
  if(is.list(keys)) .keys <- as.character(keys)
  else .keys <- keys
  x <- redisCMD("MGET",keys,rc=rc)
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
      redisCMD("SET",list(i),keyvalues[i])
  })
  if(all(x=="OK")) 1 else 0
}

redisMSetNX <- function(keyvalues)
{ 
  na <- names(keyvalues)
  if(is.null(na)) stop("keyvalues must be a named list\n")
  x <- lapply(na, function(i){
      redisCMD("SETNX",i,keyvalues[i])
  })
  if(all(x)) 1 else 0
}
redisGetRange <- function(key,start,end,rc=FALSE){
    if(missing(start)|| missing(end)) {
        stop("plese provide start and/or end")
    }
    redisCMD("GETRANGE",list(key,start,end),rc=rc)
}

redisStrlen <- function(key)
{
  redisCMD("STRLEN",list(key),rc=TRUE)
}

redisIncr <- function(key)
{
  redisCMD("INCR",key,rc=TRUE)
}

redisIncrBy <- function(key, value)
{
  redisCMD("INCRBY",list(value),rc=TRUE)
}

redisIncrByFloat <- function(key, value)
{
  redisCMD("INCRBYFLOAT",list(key,value),rc=TRUE)
}

redisDecrBy <- function(key, value)
{
  redisCMD("DECRBY",list(key,value),rc=TRUE)
}

redisDecr <- function(key)
{
  redisCMD("DECR",list(key),rc=T)
}

redisAppend <- function(key, value){
  redisCMD("APPEND",list(key,value),rc=T)
}
