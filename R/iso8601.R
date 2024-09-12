#' @title Format a time or date to an ISO8601 string.
#' @param x a Date or POSIXt time vector.
#' @param what [string] What type of output?
#' "datetime" and "timestamp" are equivalent, "date" is just the date, and "time" is just the time (i.e. without date).
#' @param tz [Olson timezone string] The desired timezone for output string presentation; same as in `base:strftime()`.
#'        The default (empty string) uses R's current session-set timezone.
#'        (`NULL` is treated identically to the empty string.)
#' @param tz_offset [boolean] Should the timezone offset be included in the output?
#' @param digits [integer] Subsecond digits desired in the output string, as specified by the "\%OSn" format string in `base::strftime()`, where n is the number of digits.
#' @return An ISO8601 character vector.
#' @export
iso8601 <- function(x, what = c("datetime", "timestamp", "date", "time"), tz = "", tz_offset = TRUE, digits = 0) {
    ## process `what`; datetime == timestamp
    what <- match.arg(what)
    if(what == "datetime") what <- "timestamp"

    ## check for valid tz:
    if(is.null(tz)) {
        tz <- ""
        warning("NULL tz detected, using system timezone as default", immediate. = TRUE)
    }
    stopifnot(is.character(tz) && length(tz) == 1)
    if(!identical(tz, "") && is.na(match(tz, OlsonNames()))) {
        stop(sprintf("unrecognized tz (%s)", tz))
    }
    
    digits <- as.integer(digits)

    date_pattern <- "%Y-%m-%d"
    time_pattern <- paste("%H:%M:%OS", digits, sep = "")
    tz_pattern <- if(tz_offset) "%z" else ""
    
    strftime_pattern <- switch(
        what
       ,"timestamp" = sprintf("%sT%s%s", date_pattern, time_pattern, tz_pattern)
       ,"date" = date_pattern
       ,"time" = sprintf("%s%s", time_pattern, tz_pattern)
       ,stop(sprintf("unrecognized `what` = %s", what))
    )

    strftime(x, strftime_pattern, tz = tz, usetz = FALSE)
}
