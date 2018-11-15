\name{if-otherwise}
\alias{if-otherwise}
\alias{\%if\%}
\alias{\%otherwise\%}
\title{Postfix if-otherwise logic}
\usage{
prior \%if\% proposition

prior \%if\% proposition \%otherwise\% alternate
}
\arguments{
\item{prior}{The value to be returned if proposition evaluates to TRUE.}

\item{proposition}{The logical statement to evaluate}

\item{alternate}{The value to be returned if proposition evaluates to FALSE.}

\item{prior \%if\% proposition}{An \code{\%if\%} statement.}
}
\description{
This construction allows logical statements to be placed after the value to be returned.
Take note that the `%if%` and `%otherwise%` operators follow the same order of operations
as other custom infix operators and so care should be taken that the effect is as desired.
}
\examples{
    x <- 1
    x <- (x+1) \%if\% is.numeric(x) \%otherwise\% "Hmm this isn't right O.o"
    x # 2

    x <- 1i
    x <- (x+1) \%if\% is.numeric(x) \%otherwise\% "Hmm this isn't right O.o"
    x # Hmm this isn't right
}
\seealso{
Other postlogic: \code{\link{unless-then}}
}
\concept{postlogic}
