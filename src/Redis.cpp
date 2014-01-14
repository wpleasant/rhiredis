// -*- indent-tabs-mode: nil; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; -*-
//
// simple C++ class to host a stateful connection to redis
//
// uses hiredis library which provides a basic C API to redis
//
// (for now) forked from Wush Wu's Rhiredis
// slowly adding some more Redis functions
//
// Dirk Eddelbuettel, 2013 - 2014

#include <Rcpp.h>
#include <hiredis/hiredis.h>         // on Ubuntu file /usr/include/hiredis/hiredis.h

extern "C" SEXP serializeToRaw(SEXP object);
extern "C" SEXP unserializeFromRaw(SEXP object);


// A simple and lightweight class -- with just a simple private member variable 
// We could add some more member variables to cache the last call, status, ...
//

class Redis {

private: 
   
    redisContext *prc_;                // private pointer to redis context

    void init(std::string host="127.0.0.1", int port=6379)  { 
        prc_ = redisConnect(host.c_str(), port);
        if (prc_->err) 
            Rcpp::stop(std::string("Redis connection error: ") + std::string(prc_->errstr));
    }
	
    SEXP extract_reply(redisReply *reply, bool rc){
        switch(reply->type) {
        case REDIS_REPLY_STRING: {
          if(rc) {
            std::string res(reply->str);
            return(Rcpp::wrap(res));
          } else {
            int nc = reply->len;
            SEXP res = Rf_allocVector(RAWSXP, nc);
            memcpy(RAW(res), reply->str, nc);
            SEXP obj = unserializeFromRaw(res);
            return(obj);
          }
        }
        case REDIS_REPLY_STATUS: {
            std::string res(reply->str);
            return(Rcpp::wrap(res));
        }
        case REDIS_REPLY_INTEGER: {
            return(Rcpp::wrap((double)reply->integer));
        }
        case REDIS_REPLY_ERROR: {
            std::string res(reply->str);
            return(Rcpp::wrap(res));
        }
        case REDIS_REPLY_NIL: {
            return(R_NilValue);
        }
        case REDIS_REPLY_ARRAY: {
            Rcpp::List retval(reply->elements);
            extract_array(reply, retval, rc);
            return(retval);
        }
        default:
            throw std::logic_error("Unknown type");
        }
    }
    
    void extract_array(redisReply *node, Rcpp::List& retval, bool rc) {
        for(unsigned int i = 0;i < node->elements;i++) {
            retval[i] = extract_reply(node->element[i], rc);
        }
    }

public:
   
    Redis(std::string host, int port)  { init(host, port); }
    Redis(std::string host)            { init(host);       }
    Redis()                            { init();           }

    ~Redis() { 
        redisFree(prc_);
        prc_ = NULL;                // just to be on the safe side
    }

    // execute given string
    SEXP exec(std::string cmd, bool rc=false) {
        redisReply *reply = static_cast<redisReply*>(redisCommand(prc_, cmd.c_str()));
        SEXP rep = extract_reply(reply, rc);
        freeReplyObject(reply);
        return(rep);
    }


    // redis set
    std::string set(std::string key, SEXP s) {

        // if raw, use as is else serialize to raw
        Rcpp::RawVector x = (TYPEOF(s) == RAWSXP) ? s : serializeToRaw(s);

        // uses binary protocol, see hiredis doc at github
        redisReply *reply = 
            static_cast<redisReply*>(redisCommand(prc_, "SET %s %b", 
                                                  key.c_str(), x.begin(), x.size()));
        std::string res(reply->str);                                                
        freeReplyObject(reply);
        return(res);
    }

    // redis get
    SEXP get(std::string key) {

        // uses binary protocol, see hiredis doc at github
        redisReply *reply = 
            static_cast<redisReply*>(redisCommand(prc_, "GET %s", key.c_str()));

        int nc = reply->len;
        SEXP res = Rf_allocVector(RAWSXP, nc);
        memcpy(RAW(res), reply->str, nc);
                                               
        freeReplyObject(reply);
        SEXP obj = unserializeFromRaw(res);
        return(obj);
    }
    // any redis Command
    SEXP cmd(std::string cmd, SEXP gc, SEXP s, bool rc =false, bool bl=false)
    {
      
      int na = 1, j=0, i;
      R_len_t slen = 0, gclen=0;
      int islist    = Rf_isNewList(s);
      if(gc != R_NilValue) {
        if(!Rf_isNewList(gc)) Rcpp::stop("gc in cmd must be a list"); 
        gclen = Rf_length(gc);
        na +=  gclen;
      }
      if(s != R_NilValue) {
        na++;
        slen = Rf_length(s);
        if(bl && islist && slen>1)
            na += slen-1;
      }
      std::vector<const char *> argv( na );
      std::vector<size_t> argvlen( na );
     
      argv[j]    = cmd.c_str();
      argvlen[j] = cmd.size();
      j++;
      /* coerce each object gc to a characterVector and attach to argv*/
      if(gclen) {
        for(i = 0;i < gclen ;i++) {
            argv[i+1]   = CHAR(STRING_ELT(Rf_coerceVector(VECTOR_ELT(gc,i),STRSXP),0));
            argvlen[i+1] = strlen(argv[i+1]);
            j++;
        }
      }
      if(na == gclen+2 ) {
        Rcpp::RawVector x = (TYPEOF(s) == RAWSXP) ? s : serializeToRaw(s);
        argv[j]    = reinterpret_cast<const char*>(x.begin());
        argvlen[j] = x.size();
      } else 
      if(na > gclen+2) {
        /* loop for for bulk inserts */
        SEXP ss;
        for( i=0; i<slen; i++) {
            ss = VECTOR_ELT(s,i);
            Rcpp::RawVector x = (TYPEOF(ss) == RAWSXP) ? ss : serializeToRaw(ss);
            argv[j]    = reinterpret_cast<const char*>(x.begin());
            argvlen[j] = x.size();
          ++j;
        }
      }
      redisReply *reply = static_cast<redisReply*>(redisCommandArgv(prc_
            , argv.size()
            , &(argv[0])
            , &(argvlen[0])));
      SEXP rep = extract_reply(reply, rc);
      freeReplyObject(reply);
      return(rep);
    }

    // could create new functions to (re-)connect with given host and port etc pp
};


RCPP_MODULE(Redis) {
    Rcpp::class_<Redis>("Redis")   
        
        .constructor("default constructor")  
        .constructor<std::string>("constructor with host port")  
        .constructor<std::string, int>("constructor with host and port")  

        .method("exec", &Redis::exec,  "execute given redis command")

        .method("set", &Redis::set,  "runs 'SET key serializedObject'")
        .method("get", &Redis::get,  "runs 'GET key'")
        
        .method("cmd", &Redis::cmd,  "execute given redis command")
    ;
}
