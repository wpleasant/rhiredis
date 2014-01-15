## general redisCommand
redisCMD <- function(cmd,ocmd=NULL,value=NULL,rc=FALSE,bl=FALSE)
{
  if(!is.null(ocmd) && !is.list(ocmd)) ocmd <- as.list(ocmd) 
  redis$cmd(cmd,ocmd,value,rc,bl)
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

### FIXME !!! This can't be the best way to do this...!!!  
redisConnect <- function(host="127.0.0.1",port=6379)
{
    host <- as.character(host)
    port <- as.integer(port)
    if("redis" %in% ls(envir=.GlobalEnv))
       stop("redis was predifined in the global env")
    assign("redis",new(Redis,host,port),envir=.GlobalEnv)
    invisible()
}

redisClose <- function() 
{
  x <- redisQuit()
  if(x=="OK")  cat("redisQuit was a success\n")
  if("redis" %in% ls(envir=.GlobalEnv)){
    rm(redis,envir=.GlobalEnv)
    cat("redis connection was closed\n")
  } else {
    cat("redis was not in the GlobalEnv\n")
  }
  invisible()
}


