#' Set OpenAI API key
#'
#' @description NLP functionalities require a paid OpenAI API key to communicate with Open AI models.
#'
#' @param key an OpenAI API key. For more information on keys see the \href{https://openai.com/blog/openai-api}{OpenAI API blog}
#' @param overwrite Option to overwrite any existing OpenAI keys already stored locally.
#' @param temporary Option to not store thew API key for use across sessions.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' set_openai_api_key("YOUR_OPENAI_API_KEY")
#'
#' # This will set the key permanently until overwritten again
#' set_openai_api_key("YOUR_OPENAI_API_KEY")
#' }
set_openai_api_key <- function(key, overwrite = FALSE, temporary = FALSE){
  if (!temporary) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(!file.exists(renv)){
      file.create(renv)
    } else{
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
      if(isTRUE(overwrite)){
        message("Adding key to your .Renviron file. Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv=readLines(renv)
        newenv <- oldenv[-grep("OPENAI_API_KEY", oldenv)]
        writeLines(newenv, renv, sep = "\n")
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("OPENAI_API_KEY",tv))){
          stop("An existing OpenAI API key is already saved. You can overwrite it with the argument overwrite=TRUE.", call.=FALSE)
        }
      }
    }

    keyconcat <- paste0("OPENAI_API_KEY='", key, "'")
    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message('Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("OpenAI_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
  } else {
    message("API key set for duration of session. To install your API key for use across sessions, run this function with `temporary = FALSE`.")
    Sys.setenv(OPENAI_API_KEY = key)
  }

}


#' View saved openAI API key
#'
#' @description View saved API key'
#'
#' @export
#'
#' @examples
#' show_openai_api_key()
show_openai_api_key <- function() {
  key <- Sys.getenv('OPENAI_API_KEY')
  if (key==""){
    message("No api key path set")
    key <- NULL
  }
  key
}
