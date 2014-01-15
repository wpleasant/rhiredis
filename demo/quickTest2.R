suppressMessages(library(rhiredis))
suppressMessages(library(rredis))

data(trees)
fit <- lm(log(Volume) ~ log(Girth) + log(Height), data=trees)

rhiredis::redisConnect()
rredis::redisConnect()      # new rredis option to mimich networking behavior of hiredis

## set a serialized object
key <- "foo"
redis$set(key, serialize(fit,NULL,ascii=TRUE))


## retrieve with rredis
fit2 <- rredis::redisGet(key)

## check
all.equal(fit, fit2)


## or serialize an object internally 
key <- "foo2"
redis$set(key, fit)

## retrieve with rredis
fit3 <- rredis::redisGet(key)

## check
all.equal(fit, fit3)

## retrieve with rredis
fit4 <- redis$get(key)

## check
all.equal(fit, fit4)

## set from redisCMD
rhiredis::redisCMD("SET","fit2",fit)


## get from redisCMD 
fit5 <- rhiredis::redisCMD("Get","fit2")

##check
all.equal(fit, fit5)

## check keys
all.equal(rhiredis::redisKeys(),rredis::redisKeys())


## set a character string rhiredis
rhiredis::redisCMD("SET",list("teststring1","myteststring"))

## set a serialized string rhiredis

rhiredis::redisSet(list("teststring2"),"myteststring")

## get a character string from the CLI
system("redis-cli get teststring1 ")


## get a serialized string from the CLI
system("redis-cli get teststring2 ")

## get teststring 1 
ts1 <- rhiredis::redisGet("teststring1",rc=T)

## get teststring 1 
ts2 <- rhiredis::redisGet("teststring2",rc=F)

## check

all.equal(ts1,ts2)







