#' @title Format an ID/key vector.
#' @param x An ID/key vector.
#' @return A character vector.
#' @export
format_id <- function(x) {

  if (length(x) == 0) return(character(0))

  if (is.character(x)) return(stringr::str_trim(x))

  if (bit64::is.integer64(x)) return(as.character(x))

  if (is.integer(x)) return(as.character(x))

  if (is.double(x)) {

    if (rlang::is_integerish(x)) return(format(round(x), digits = 22, scientific = FALSE, trim = TRUE, justify = "none", drop0trailing = TRUE))

    warning("non-integerish numeric ID found, recursing with as.character", immediate. = TRUE)
    rlang::current_fn()(as.character(x))
  }

  warning(sprintf("no known method for formating IDs of type %s, recursing with as.character", class(x)), immediate. = TRUE)
  rlang::current_fn()(as.character(x))
}
