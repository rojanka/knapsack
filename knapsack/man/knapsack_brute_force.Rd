\name{knapsack_brute_force}
\alias{knapsack_brute_force}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
knapsack_brute_force(x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{W}{
%%     ~~Describe \code{W} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, W) 
{
    stopifnot((length(x$v) == length(x$w)))
    combinations = matrix(data = FALSE, nrow = length(x$v), ncol = 2)
    combinations[1, 2] = TRUE
    for (i in 1:(length(x$v) - 1)) {
        combinations2 = combinations
        combinations2[i, ] = TRUE
        combinations = cbind(combinations, combinations2)
    }
    values = apply(combinations, 2, function(y) {
        sum(x$v[y])
    })
    weights = apply(combinations, 2, function(y) {
        sum(x$w[y])
    })
    return(max(values[weights <= W]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
