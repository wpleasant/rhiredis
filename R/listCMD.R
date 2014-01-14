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


