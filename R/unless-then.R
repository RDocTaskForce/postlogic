#' @name unless-then
#' @title Infix unless-then logic
#'
#' @description
#' These give logic that can be used as a qualifying statement that occures after the
#' value statement.
#' Take note that the `%unless%` and `%then%` operators follow the same order of operations
#' as other custom infix operators and so care should be taken that the effect is as desired.
#'
#' @usage
#' prior %unless% proposition
#' prior %unless% proposition %then% alternate
#'
#' @param prior Value to be returned unless proposition returns FALSE.
#' @param proposition The logical statement to condition on.
#' @param alternate When proposition returns true and the %then% is provided
#'                  the alternate value is returned.
#'
#' @family postlogic
#' @examples
#'     x <- 4
#'     x <- sqrt(x) %unless% is.complex(x) %then% "This is too hard :("
#'     x # 2
#'
#'     x <- 4i
#'     x <- sqrt(x) %unless% is.complex(x) %then% "This is too hard :("
#'     x # This is too hard :(
#' @export %unless% %then%
`%unless%` <- function( prior, proposition ){
    if (!proposition) return(prior)
}
#' @rdname unless-then
#' @export
`%then%` <- function( clause, alternate){
    clause.call <- substitute(clause)
    if (clause.call[[1]] != '%unless%')
        stop("Infix opperator '%then%' can only be used following an '%unless%' infix.")

    value <- clause.call[[2]]
    predicate <- clause.call[[3]]

    predicate.value <- eval(predicate, envir = parent.frame())

    if (!predicate.value) eval(value, envir = parent.frame()) else alternate
}
if(FALSE){#@testing unless-then logic
    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE
    expect_equal(val, 'it is supposed to be evaluated')
    expect_identical(val, x)

    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it still evaluated') %unless% TRUE
    expect_null(val)
    expect_false(exists('x', inherits=FALSE))

    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it still evaluated') %unless% TRUE %then% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x', inherits=FALSE))

    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %unless% FALSE %then% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x', inherits=FALSE))
    expect_equal(x, 'it is supposed to be evaluated')

    if (exists('x', inherits=FALSE)) rm(list='x')
    expect_error( 'this' %if% 'wont' %then% 'work'
                , "Infix opperator '%then%' can only be used following an '%unless%' infix."
                )
}
if(FALSE){#@example
    x <- 4
    x <- sqrt(x) %unless% is.complex(x) %then% "This is too hard :("
    x # 2

    x <- 4i
    x <- sqrt(x) %unless% is.complex(x) %then% "This is too hard :("
    x # This is too hard :(
}
