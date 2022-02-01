#' @author HLS
#' @title  xpull
#' @details code to pull data from XMART into R via csv - FASTER, doesn't give column types
#' @param table - a string representing the name of the table in xmart
#' @param auth - authorisation package
#' @param use_ext - use cached data? (downloads faster, slightly out of date)
#' @param format - the format of the table to be pulled - can take json or csv (csv)
#' @param mart - which mart should be used? NCOV by default
#'
#'
#' @export
#'
 xpull <- function(table, auth, use_ext = FALSE, format = "csv", mart = "NCOV") {

   if (missing(auth) & !use_ext) {
     stop("DATA WILL NOT BE PULLED - AUTH PACKAGE NEEDED TO ACCESS XMART")
   }

   resource_reqs <- c(
     "resource",
     "tenant",
     "app",
     "auth_type",
     "password",
     "use_cache",
     "base_url"
   )

   walk(resource_reqs, check_auth, names(auth))

  if (format == "json") {
    xpull_json(table, auth, use_ext, mart)
  } else if (format == "csv") {
    xpull_csv(table, auth, use_ext, mart)
  }

 }


 check_auth <- function(x, names) {
   if (!x %in% names) {
     stop(paste0(x, " not found in auth"))
   }
 }

