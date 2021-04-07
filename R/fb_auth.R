#' R6 environment to store authentication credentials
#'
#' Used to keep persistent state.
#' @export
FacebookAuth <- R6::R6Class(
  "FacebookAuth",
  public = list(token = NULL, method = NULL),
  lock_objects = FALSE,
  parent_env = emptyenv()
)


#' Authorize Facebook API with your token
#'
#' Enter your token that can be found on https://developers.facebook.com/.
#'
#' @import httr
#'
#' @export
#' @examples
#' fb_auth(client_id = Sys.getenv("FACEBOOK_CLIENT_ID"), client_secret = Sys.getenv("FACEBOOK_CLIENT_SECRET"), token = NULL, new_user = FALSE, silent = FALSE)
fb_auth <- function(client_id = Sys.getenv("FACEBOOK_CLIENT_ID"), client_secret = Sys.getenv("FACEBOOK_CLIENT_SECRET"), token = NULL, new_user = FALSE, silent = FALSE) {
  options("FacebookR.oauth_cache" = ifelse(is.null(getOption("FacebookR.oauth_cache")),
                                         ifelse(is.null(token), "FacebookR.httr-oauth", token),
                                         getOption("FacebookR.oauth_cache")))

  httr_file <- getOption("FacebookR.oauth_cache")

  if(new_user){
    rm_old_user_cache(httr_file)
  }

  # Check if token is saved
  if(file.exists(getOption("FacebookR.oauth_cache"))){
    token <- getOption("FacebookR.oauth_cache")
  }

  if((client_id == "" | client_secret == "") & is.null(token)){
    cat(crayon::red("Enter your credentials to authorize"))
    client_id <- readline("Client Id: ")
    client_secret <- readline("Client secret: ")
  }

  Sys.setenv(FACEBOOK_CLIENT_ID = client_id)
  Sys.setenv(FACEBOOK_CLIENT_SECRET = client_secret)


  get_environment_value <- function(env){
    ifelse(
      Sys.getenv(env) == "",
      return(NULL),
      return( Sys.getenv(env))
    )
  }


  options("FacebookR.client_id" = get_environment_value("FACEBOOK_CLIENT_ID"))
  options("FacebookR.client_secret" = get_environment_value("FACEBOOK_CLIENT_SECRET"))



  if(assertthat::is.flag(httr_file)){
    stop("option('FacebookR.oauth_cache') must be a valid cache file location. Must end with .httr-oauth.", call. = FALSE)
  }

  assertthat::assert_that(assertthat::is.string(httr_file),
                          assertthat::is.flag(new_user))




  if(is.null(token)) {
    token <- create_oauth_token()
  } else if(inherits(token, "Token2.0")){

    if(!is_valid_token(token)){
      stop("Invalid token passed to function", call. = FALSE)
    }

    FacebookAuth$set("public", "method", "passed_token", overwrite=TRUE)
    FacebookAuth$set("public", "token", token, overwrite=TRUE)
  } else if(is.string(token)){

    if(file.exists(token)){
      token <- read_cache_token(token_path = token)
    } else {
      cat(crayon::red(paste0("No httr_oauth_cache file found at ", token, " - creating new file.\n")))
      options("FacebookR.oauth_cache" = token)
      FacebookAuth$set("public", "token", NULL, overwrite=TRUE)
      return(fb_auth(token = NULL))
    }

  } else {
    stop("Unrecognised token object - class ", class(token), call. = FALSE)
  }

  check_existing_token()$is_valid

  if(!silent){
    cat(crayon::green("Successfully authenticated Facebook API!\n"))
  }

  return(invisible(token))
}


#' @noRd
#' @importFrom httr oauth_endpoints oauth_app oauth2.0_token
#' @import httpuv
create_oauth_token <- function(){
  check_existing <- check_existing_token()$is_valid
  if(!check_existing){
    if(!interactive()){
      stop("Authentication options didn't match existing session token and not interactive session
           so unable to manually reauthenticate", call. = FALSE)
    }
  }


  endpoint <- oauth_endpoint(
    authorize = "https://www.facebook.com/dialog/oauth",
    access = "https://graph.facebook.com/oauth/access_token"
  )


  client_id <- getOption("FacebookR.client_id", "")
  client_secret <- getOption("FacebookR.client_secret", "")
  cache <- getOption("FacebookR.oauth_cache", "")

  if(client_id == ""){
    stop('options("FacebookR.client_id") has not been set.', call. = FALSE)
  }
  if(client_secret == ""){
    stop('options("FacebookR.client_secret") has not been set.', call. = FALSE)
  }
  if(cache == ""){
    stop('options("FacebookR.oauth_cache") has not been set.', call. = FALSE)
  }

  app <- oauth_app(
    appname = "Facebook",
    key = client_id,
    secret = client_secret
  )

  tryCatch({
    token <- oauth2.0_token(
      endpoint = endpoint,
      app = app,
      cache = cache
    )

    stopifnot(is_valid_token(token))

    FacebookAuth$set("public", "token", token, overwrite=TRUE)
    FacebookAuth$set("public", "method", "new_token", overwrite=TRUE)

    return(invisible(token))
  }, error = function(e){
    cat(crayon::red("Authentication error. Check the provided credentials and try again!\n"))
  })
}


#' @noRd
is_valid_token <- function(token){
  if(is.null(token)){
    cat(crayon::red("No token found. Create one with fb_auth().\n"))
    return(FALSE)
  }

  if(!inherits(token, "Token2.0")){
    cat(crayon::red(paste0("The object was of class ", class(token), ". Needs to be a Token2.0 object!\n")))
    if(inherits(token, "list")){
      if(inherits(token[[1]], "Token2.0")){
        warning("The first object in the list is a token. Make sure to only use the token object and not a list.")
      }
    }
    return(FALSE)
  }

  return(TRUE)
}


#' @noRd
check_existing_token <- function(token = FacebookAuth$public_fields$token){
  cache_path <- client_id <- client_secret <- FALSE

  msg <- ""

  if(is.null(token)){
    msg <- "No local token found in session"
    return(list(is_valid = FALSE, message = msg))
  }

  cache_path <- !all(token$cache_path %in% getOption("FacebookR.oauth_cache"))


  if(!is.null(token$app)){
    if(!all(token$app$key %in% getOption("FacebookR.client_id"))){
      client_id <- TRUE
    }

    if(!all(token$app$secret %in% getOption("FacebookR.client_secret"))){
      client_secret <- TRUE
    }
  } else {
    msg <- "No Client Id in token!"
  }

  return(list(is_valid = !any(cache_path, client_id, client_secret), message = msg))
}


#' @noRd
rm_empty_token <- function(token_path = getOption("FacebookR.oauth_cache")){
  ## delete token if 0B
  iz_0B <- file.info(token_path)$size == 0
  if(iz_0B){
    unlink(token_path)
  }
}



#' @noRd
rm_old_user_cache <- function(httr_file){
  FacebookAuth$set("public", "token", NULL, overwrite=TRUE)

  Sys.setenv(FACEBOOK_CLIENT_ID = "")
  Sys.setenv(FACEBOOK_CLIENT_SECRET = "")

  if(file.exists(httr_file)){
    cat(crayon::red(paste0("Removing old cached credentials from: ", normalizePath(httr_file),"\n")))
    file.remove(httr_file)
  }
}


#' @noRd
#' @import assertthat
read_cache_token <- function(token_path){

  assert_that(is.readable(token_path))

  cat(crayon::red("Reading token from file path\n"))

  token <- tryCatch({readRDS(token_path)},
                    error = function(ex){
                      stop(sprintf("Cannot read token from alleged .rds file:\n%s",
                                   token_path), ex, call. = FALSE)
                    })

  if(is.list(token)){
    cat(crayon::red("Multiple httr-tokens in cache ",token_path, ", only returning first found token\n"))
    token <- token[[1]]
  } else if(is.token2.0(token)){
    cat(crayon::red("Read token successfully from file\n"))
  } else {
    stop("Unknown object read from ", token_path, " of class ", class(token))
  }

  token$cache_path <- token_path

  if(is.null(token$app)){
    cat(crayon::red("No Client Id in token!\n"))
    return(token)
  }

  if(!all(token$app$key %in% getOption("FacebookR.client_id"))){
    options("FacebookR.client_id" = token$app$key)
  }

  if(!all(token$app$secret %in% getOption("FacebookR.client_secret"))){
    options("FacebookR.client_secret" = token$app$secret)
  }


  FacebookAuth$set("public", "method", "filepath", overwrite=TRUE)
  FacebookAuth$set("public", "token", token, overwrite=TRUE)

  return(token)
}
