#' @author HLS
#' @title xpull_csv
#' @details code to pull data from XMART into R via csv - FASTER, doesn't give column types
#' @param  table - the name of the table on xmart to be used
#' @param auth - authorisation package
#' @param use_ext - use cached data? (downloads faster, slightly out of date)
#' @param mart - which mart should be used? NCOV by default

xpull_csv <- function(table, auth, use_ext = FALSE, mart = "NCOV") {

  if (use_ext) {
    comp_url <- paste0("https://frontdoor-l4uikgap6gz3m.azurefd.net/", mart, "/")
  } else {
    comp_url <- paste0("https://extranet.who.int/xmart-api/odata/", mart, "/")
  }


  has_params <- grepl("\\?\\$", table)

  if (has_params) {
    appended_link <- "&$format=csv"
  } else {
    appended_link <- "?$format=csv"
  }


  if (use_ext) {
    tok0 <- get_azure_token(resource = auth$resource,
                            tenant = auth$tenant,
                            app = auth$app,
                            auth_type = auth$auth_type,
                            password = auth$password,
                            use_cache = auth$use_cache)

    access_token <- tok0$credentials$access_token
    bearer <- paste("Bearer", access_token)

    headers <- add_headers(Authorization = bearer)
    cached <- FALSE
  } else {
    headers <- NULL
  }

  if (!is.null(headers)) {
    response <- GET(paste0(comp_url, table, appended_link), headers)
  } else {
    response <- GET(paste0(comp_url, table, appended_link))
  }

  out <- tryCatch(
    {
      message("Attempting to read data from XMART")
      parsed <- httr::content(response,
                              type = "text")
      parsed <- readr::read_csv(parsed)

      colnames(parsed) <- str_to_lower(colnames(parsed))
      colnames(parsed) <- gsub("_fk", "", colnames(parsed))
      dat <- select(parsed, -any_of("sys_pk"))
      return(dat)
    },
    error = function(cond) {
      message("There has been an error while pulling the data from XMART.")
      message(cond)
      return(NULL)
    }

  )


  return(out)


}
