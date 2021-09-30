#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @export

dtbl <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(x[...])
  eval(temp)
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @export

i <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(...)
  x[i = eval(temp)]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @export

j <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(...)
  x[j = eval(temp)]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @param by for grouping purposes
#' @export

j_by <- function(x, ..., by) {
  stopifnot(inherits(x, "data.table"))
  temp1 <- substitute(...)
  temp2 <- substitute(by)
  x[j = eval(temp1), by = eval(temp2)]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @export

j_list <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(list(...))
  x[j = eval(temp)]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @param by for grouping purposes
#' @export

j_list_by <- function(x, ..., by) {
  stopifnot(inherits(x, "data.table"))
  temp1 <- substitute(list(...))
  temp2 <- substitute(by)
  x[j = eval(temp1), by = eval(temp2)]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @importFrom data.table ':='
#' @export

create <- function(x, ...) {
  stopifnot(inherits(x, "data.table"))
  temp <- substitute(`:=`(...))
  x[j = eval(temp)][]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table
#' @param ... further arguments
#' @param by for grouping purposes
#' @importFrom data.table ':='
#' @export

create_by <- function(x, ..., by) {
  stopifnot(inherits(x, "data.table"))
  temp1 <- substitute(`:=`(...))
  temp2 <- substitute(by)
  x[j = eval(temp1), by = eval(temp2)][]
}

#' @title Pipeable functions to improve data.table's readability
#'
#' @param x a data.table or data.frame
#' @param ... further arguments
#' @export

get_var <- function(x, ...) {
  temp <- substitute(x[[...]])
  eval(temp)
}
