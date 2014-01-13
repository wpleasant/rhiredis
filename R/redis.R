## minimal R interface to redis using the hiredis library

## ensure module gets loaded
loadModule("Redis", TRUE)

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
