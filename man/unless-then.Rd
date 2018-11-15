\name{unless-then}
\alias{unless-then}
\alias{\%unless\%}
\alias{\%then\%}
\title{Infix unless-then logic}
\usage{
prior \%unless\% proposition
prior \%unless\% proposition \%then\% alternate
}
\arguments{
\item{prior}{Value to be returned unless proposition returns FALSE.}

\item{proposition}{The logical statement to condition on.}

\item{alternate}{When proposition returns true and the %then% is provided
the alternate value is returned.}

\item{prior \%unless\% proposition}{An \code{\%if\%} statement.}
}
\description{
These give logic that can be used as a qualifying statement that occurs after the
value statement.
Take note that the `%unless%` and `%then%` operators follow the same order of operations
as other custom infix operators and so care should be taken that the effect is as desired.
}
\examples{
    x <- 4
    x <- sqrt(x) \%unless\% is.complex(x) \%then\% "This is too hard :("
    x # 2

    x <- 4i
    x <- sqrt(x) \%unless\% is.complex(x) \%then\% "This is too hard :("
    x # This is too hard :(
}
\seealso{
Other postlogic: \code{\link{if-otherwise}}
}
\concept{postlogic}
