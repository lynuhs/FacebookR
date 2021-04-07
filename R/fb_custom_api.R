#' Get custom API request string
#'
#' Custom request to the Facebook API with json encoded string
#'
#' @param request A Custom request path for the API
#' @param method The request method
#' @param api_version The version of the API to use
#' @param body What to include in the request body
#'
#' @import httr
#'
#'
#' @export
#' @examples
#' fb_custom_api(request, method="GET", api_version="10.0")
fb_custom_api <- function(request, method="GET", api_version="v10.0", body=NULL){
  if(!fb_check_existing_token()){
    tryCatch({
      fb_auth()
    }, error = function(e){
      stop("No authenticated token found!",call. = FALSE)
    })
  }

  request <- gsub("^\\/", "", request)
  api_version <- gsub("\\/", "", api_version)
  
  url <- paste0("https://graph.facebook.com/", api_version, "/", request))
  tryCatch({
    data <- content(get(method)(
      url = url,
      add_headers(
        .headers = c(
          "Authorization" = FacebookAuth$public_fields$token$credentials$access_token
        )
      ),
      body = body,
      encode = "json"
    ))

    if(any(grepl("errors.code",names(unlist(data))))){
      cat(crayon::red("Error: Not an authorized API call"))
    } else if(!is.null(data$errors)){
      stop(data$errors[[1]]$message, call. = FALSE)
    }
    else {
      return(data)
    }

  }, error = function(e){
    cat(crayon::red(paste0(
      "Error: ", e$message
    )))
  })
}
