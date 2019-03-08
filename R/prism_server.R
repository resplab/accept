#Last update: 2019.02.12
#Remember to run redis otherwise it will get stuck on redisConnect


thisSession<-new.env()


thisSession$REDIS_ADDRESS="prism.resp.core.ubc.ca"
thisSession$REDIS_PORT <- 3001

thisSession$MODE_REQUIRE_API_KEY=TRUE;
thisSession$MODE_REQUIRE_SESSION=TRUE;
thisSession$MODE_REQUIRE_SESSION_DATA=TRUE;

thisSession$LONG_RUN_STATUS_READY<-0
thisSession$LONG_RUN_STATUS_DONE<-1
thisSession$LONG_RUN_STATUS_ERROR<- -1

thisSession$MODEL_DESCRIPTION<-"This is ACCEPT - PRISM enabled!"
thisSession$MODEL_VERSION<- packageVersion('accept')

connect_redis_prism <- function (){
  rredis::redisConnect(host = thisSession$REDIS_ADDRESS, port = thisSession$REDIS_PORT, password = "H1Uf5o6326n6C2276m727cU82O")
}



#' @export
test<-function(func,...)
{
  return(jsonlite::toJSON("Hi"))
}



#DEPRECATED
#' @export
gateway_json<-function(func,...)
{
  f<-get(func)
  out<-f(...)

  return(jsonlite::toJSON(out))
}



#' @export
prism_model_run<-function(model_input)
{
  return(model_run(model_input))
}



#' @export
gateway_json0<-function(func)
{
  check_access(func=func)
  f<-get(func)
  out<-f()

  return(jsonlite::toJSON(out))
}

#' @export
gateway_json1<-function(func,parms1)
{
  check_access(func=func)
  f<-get(func)
  out<-f(parms1)

  return(jsonlite::toJSON(out))
}

#' @export
gateway_json2<-function(func,parms1,parms2)
{
  check_access(func=func)
  f<-get(func)
  out<-f(parms1,parms2)

  return(jsonlite::toJSON(out))
}

#' @export
gateway_json3<-function(func,parms1,parms2,parms3)
{
  check_access(func=func)
  f<-get(func)
  out<-f(parms1,parms2,parms3)

  return(jsonlite::toJSON(out))
}







#' @export
gateway_json0_s<-function(session_id,func)
{
  check_access(session_id,func)
  session_id<<-session_id
  restore_session(session_id)
  f<-get(func)
  out<-f()
  save_session(session_id)
  return(jsonlite::toJSON(out))
}

#' @export
gateway_json1_s<-function(session_id,func,parms1)
{
  check_access(session_id,func)
  session_id<<-session_id
  restore_session(session_id)
  f<-get(func)
  out<-f(parms1)
  save_session(session_id)
  return(jsonlite::toJSON(out))
}

#' @export
gateway_json2_s<-function(session_id,func,parms1,parms2)
{
  check_access(session_id,func)
  session_id<<-session_id
  restore_session(session_id)
  f<-get(func)
  out<-f(parms1,parms2)
  save_session(session_id)
  return(jsonlite::toJSON(out))
}

#' @export
gateway_json3_s<-function(session_id,func,parms1,parms2,parms3)
{
  check_access(session_id,func)
  session_id<<-session_id
  restore_session(session_id)
  f<-get(func)
  out<-f(parms1,parms2,parms3)
  save_session(session_id)
  return(jsonlite::toJSON(out))
}










save_session<-function(session_id)
{
  if(!thisSession$MODE_REQUIRE_SESSION_DATA) return()
  connect_redis_prism()
  e<-new.env()
  for(nm in names(globalenv()))
  {
    if(typeof(globalenv()[[nm]])!='closure')
    {
      e[[nm]]<-globalenv()[[nm]]
    }
  }
  rredis::redisSet(paste(session_id,"env",sep=":"),e)
}






restore_session<-function(session_id)
{
  if(!thisSession$MODE_REQUIRE_SESSION_DATA) return()
  connect_redis_prism()
  e<-rredis::redisGet(paste(session_id,"env",sep=":"))
  for(nm in names(e))
  {
    if(typeof(e[[nm]])!='closure')
    {
      .GlobalEnv[[nm]]<-e[[nm]]
    }
  }
}







connect_to_model<-function(model_name,api_key)
{
  out<-list(result=TRUE,session_id="",version="",description="")

  if(thisSession$MODE_REQUIRE_API_KEY)
  {
    if(is.null(api_key))
    {
      out$result<-FALSE
      out$description<-"Error: access to the model requires a valid API key."
      return(out)
    }
    res<-get_access(model_name,api_key)
    if(res==FALSE)
    {
      out$result<-FALSE
      out$description<-"Error: invalid API key."
      return(out)
    }
  }

  if(thisSession$MODE_REQUIRE_SESSION)
  {
    session_id<-generate_session_id()
    set_redis_var(session_id,value = model_name)
    out$session_id<-session_id
  }

  out$description<-thisSession$MODEL_DESCRIPTION
  return(out)
}






disconnect_from_model<-function()
{
  if(!is.null(session_id) && session_id!="")
  {
    connect_redis_prism()
    keys<-rredis::redisKeys(pattern = paste(session_id,"*",sep=""))
    rredis::redisDelete(keys)
    #To prevent recording of this session environment by the calling gateway.
    thisSession$MODE_REQUIRE_SESSION_DATA<-FALSE
    return(TRUE)
  }
  else
  {
    warning("This was not a sessioned connection. Nothing to disconnet")
    return(FALSE)
  }
}






#Gets access to the model by checking api key. Returns true of successful, false otherwise
get_access<-function(model_name,api_key)
{
  if(api_key=="123456") return(TRUE);
  return(FALSE);
}



#Checks if the submitted session_id has the privilge to access the model. This is done by checking if (session_id,model_name) ecists in redis
check_access<-function(session_id="", func=NULL)
{
  if(thisSession$MODE_REQUIRE_SESSION==FALSE) return(TRUE)
  if(session_id=="" || func=="connect_to_model") return(TRUE)
  x<-get_redis_var(session_id)
  if(!is.null(x)) return(TRUE)
  stop("ERROR: Unauthorized access.")
}


generate_session_id<-function()
{
  id<-paste(c(sample(LETTERS,1) , sample(c(LETTERS,0:9),9,TRUE)),collapse="")
  return(id)
}












#' @export
model_run.long<-function(input)
{
  if(is.null(session_id) || session_id=="")
    stop("Error: long run is not available as this is a session-less connection")

  key<-paste(session_id,"status",sep=":")

  if(get_redis_var(key))
  {
    #There is already a job for this session in the que!
    return(FALSE)
  }
  else
    set_redis_var(key,0)

  return(TRUE)
}









#' @export
prism_check_run_progress<-function()
{
  if(is.null(session_id) || session_id=="")
    stop("Error: long run is not available as this is a session-less connection")

  key<-paste(session_id,"status",sep=":")

  val<-get_redis_var(key)

  if(val)
  {
    return(val)
  }
  else
    return(FALSE)
}











#This function is called by model code in a different R process. sesssion_infor should be passed from one process to another.
#' @export
prism_set_run_progress<-function(value)
{
  if(is.null(session_id) || session_id=="")
    stop("Error: long run is not available as this is a session-less connection")

  key<-paste(session_id,"status",sep=":")

  set_redis_var(key,value)
}








#' @export
prism_get_output_structure<-function()
{
  out<-list(
    n_agents=prism_output(source="$n_agents", type = "numeric/scalar", group = "", title = "Number of simulated individuals", description = ""),
    cumul_time=prism_output(source="$cumul_time",type = "numeric/scalar", title = "cumulative time"),
    n_deaths=prism_output(source="$n_deaths",type = "numeric/scalar", title = "number of deaths"),
    n_COPD=prism_output(source="$n_COPD",type = "numeric/scalar", title = "Number of patients with COPD"),
    total_exac=prism_output(source="$total_exac",type = "numeric/vector", title = "Total number of exacerbations by severity"),
    total_exac_time=prism_output(source="$total_exac_time",type = "numeric/vector", title = "total_exac_time"),
    total_pack_years=prism_output(source="$total_pack_years",type = "numeric/scalar", title = "Total pack-years"),
    total_doctor_visit=prism_output(source="$total_doctor_visit",type = "numeric/vector", title = "Total doctor visits"),
    total_cost=prism_output(source="$total_cost",type = "numeric/scalar", title = "Total costs"),
    total_qaly=prism_output(source="$total_qaly",type = "numeric/scalar", title = "Total QALY")
  )
  return(out)
}



####################Redis example

set_redis_var<-function(variable,value)
{
  #TODO: connect should not be in these functions as it will cause multiple connection attempts!
  connect_redis_prism()
  rredis::redisSet(variable,value)
  return(TRUE)
}


get_redis_var<-function(variable)
{
  connect_redis_prism()
  x<-rredis::redisGet(variable)
  return(x)
}



delete_redis_var<-function(variable)
{
  connect_redis_prism()
  rredis::redisDelete(variable)
}





set_var<-function(variable,value)
{
  .GlobalEnv[[variable]]<-value
}


get_var<-function(variable)
{
  return(.GlobalEnv[[variable]])
}
































#######################################LONG RUN functions###############
#This is just a placeholder. These functions should not be here!

#browses through rredis for all <session_id:staus,0> pairs and call model run
prism_patrol<-function()



  model_run.long<-function(session_id)
  {

  }
