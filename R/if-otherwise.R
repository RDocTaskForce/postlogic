#' @name if-otherwise
#' @title Infix if-otherwise logic
#'
#' @description
#' This construction allows logical statements to be placed after the value to be returned.
#' Take note that the `%if%` and `%otherwise%` operators follow the same order of operations
#' as other custom infix operators and so care should be taken that the effect is as desired.
#'
#'
#' @usage
#' prior %if% proposition
#' prior %if% proposition %otherwise% alternate
#'
#' @param prior The value to be returned if proposition evaluates to TRUE.
#' @param proposition The logical statement to evaluate
#' @param alternate The value to be returned if proposition evaluates to FALSE.
#'
#' @family postlogic
#'
#' @examples
#'     x <- 1
#'     x <- (x+1) %if% is.numeric(x) %otherwise% "Hmm this isn't right O.o"
#'     x # 2
#'
#'     x <- 1i
#'     x <- (x+1) %if% is.numeric(x) %otherwise% "Hmm this isn't right O.o"
#'     x # Hmm this isn't right
#' @export
`%if%` <- function( prior, proposition ){
    if (proposition) return(prior)
}
#' @rdname if-otherwise
#' @export
`%otherwise%` <- function( clause, alternate){
    clause.call <- substitute(clause)
    if (clause.call[[1]] != '%if%')
        stop("Infix opperator '%otherwise%' can only be used following an '%if%' infix.")

    value <- clause.call[[2]]
    predicate <- clause.call[[3]]

    predicate.value <- eval(predicate, envir = parent.frame())

    if (predicate.value) eval(value, envir = parent.frame()) else alternate
}
if(FALSE){#@testing if-otherwise logic
    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it still evaluated') %if% FALSE
    expect_null(val)
    expect_false(exists('x', inherits=FALSE))

    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it still evaluated') %if% FALSE %otherwise% "should get this"
    expect_equal(val, "should get this")
    expect_false(exists('x', inherits=FALSE))

    if (exists('x', inherits=FALSE)) rm(list='x')
    val <- (x <- 'it is supposed to be evaluated') %if% TRUE %otherwise% "should not get this"
    expect_equal(val, 'it is supposed to be evaluated')
    expect_true(exists('x', inherits=FALSE))
    expect_equal(x, 'it is supposed to be evaluated')

    if (exists('x', inherits=FALSE)) rm(list='x')
    expect_error( 'this' %unless% 'wont' %otherwise% 'work'
                , "Infix opperator '%otherwise%' can only be used following an '%if%' infix."
                )
}
if(FALSE){#@example
    x <- 1
    x <- (x+1) %if% is.numeric(x) %otherwise% "Hmm this isn't right O.o"
    x # 2

    x <- 1i
    x <- (x+1) %if% is.numeric(x) %otherwise% "Hmm this isn't right O.o"
    x # Hmm this isn't right
}



