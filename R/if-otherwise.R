
`%if%` <- function( prior, proposition ){
    if (proposition) return(prior)
}
`%otherwise%` <- function(`%if% prior proposition`, alternate){
    clause.call <- substitute(`%if% prior proposition`)
    if (clause.call[[1]] != '%if%')
        stop("Infix opperator '%otherwise%' can only be used following an '%if%' infix.")

    value <- clause.call[[2]]
    predicate <- clause.call[[3]]

    predicate.value <- eval.parent(predicate)
    if (predicate.value) eval.parent(value) else alternate
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



