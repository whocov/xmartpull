#' @author HLS
#' @title xpull_json
#' @details code to pull data from XMART into R - NOT EXPORTED
#' @param table - what table should be pulled
#' @param auth - authorisation package
#' @param use_ext - use cached data? (downloads faster, slightly out of date)
#' @param mart - which mart should be used? NCOV by default

#'
xpull_json <- function(table, auth, use_ext = FALSE, mart = "NCOV") {


  if (missing(auth) & !use_ext) {
    warning("DATA WILL NOT BE PULLED - AUTH PACKAGE NEEDED TO ACCESS XMART")
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
    base_url <- auth$base_url
  } else {
    headers <- NULL
    base_url <- "https://frontdoor-l4uikgap6gz3m.azurefd.net/"
  }

  dat <- pull_iter(table, base_url, cached, mart)


  if (is.null(dat)) return(NULL)

  dat_out <- dat$dat

  while(!is.na(dat$next_link)) {
    print("1")
    dat <- pull_iter(dat$next_link, base_url, headers, mart)
    if (is.null(dat)) return(NULL)

    dat_out <- bind_rows(dat_out, dat$dat)

  }


  return(dat_out)
}
#' @noRd
parsed_to_df <- function(parsed) {
  purrr::map_df(parsed[["value"]], null_to_na)
}
#' @noRd

null_to_na <- function(l) {
  sapply(l, function(x) ifelse(is.null(x), NA, x))
}

#' @noRd

pull_iter <- function(table, base_url, headers, mart) {

  comp_url <- paste0(base_url, mart, "/")

  out <- tryCatch(
    {
      message("Attempting to read data from XMART")

      if (!is.null(headers)) {
        response <- GET(paste0(comp_url, table), headers)
      } else {
        response <- GET(paste0(comp_url, table))

      }


      parsed <- httr::content(response,
                              as = "parsed",
                              type = "application/json")

      next_link <- parsed[["@odata.nextLink"]]

      if(is.null(next_link)) {
        next_link <- NA
      } else {
        next_link <- str_replace(
          next_link,
          base_url,
          ""
        )
      }



      dat <- parsed_to_df(parsed)
      colnames(dat) <- stringr::str_to_lower(colnames(dat))
      colnames(dat) <- gsub("_fk", "", colnames(dat))
      dat <- select(dat, -any_of("sys_pk"))
      message("Data successfully pulled from XMART")
      list("dat" = dat, "next_link" = next_link)
    },
    error=function(cond) {
      message("There has been an error while pulling the data from XMART.")
      message(cond)
      return(NULL)
    }
  )

  return(list("dat" = out$dat, "next_link" = out$next_link))

}
