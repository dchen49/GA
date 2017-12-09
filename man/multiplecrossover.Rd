\name{multiplecrossover}
\alias{multiplecrossover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs a Genetic Cross with multiple translocation positions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multiplecrossover(parents, num.cross.locations = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parents}{
\code{matrix of dimension 2 * p}
}
  \item{num.cross.locations}{
\code{number of locations where crossover happens, defalut value is two}
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
Qi Chen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{singlecrossover}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (parents, num.cross.locations = 2)
{
    n <- dim(parents)[2]
    children <- matrix(0, nrow = 2, ncol = n)
    cross <- function(parents, num.cross.locations = 2) {
        crossoverPoint <- sort(sample(seq(1.5, dim(parents)[2],
            by = 1), size = num.cross.locations))
        if (num.cross.locations\%\%2 == 0) {
            for (i in seq(1, num.cross.locations, by = 2)) {
                crosspart <- parents[1, (crossoverPoint[i] +
                  0.5):(crossoverPoint[i + 1] - 0.5)]
                parents[1, (crossoverPoint[i] + 0.5):(crossoverPoint[i +
                  1] - 0.5)] <- parents[2, (crossoverPoint[i] +
                  0.5):(crossoverPoint[i + 1] - 0.5)]
                parents[2, (crossoverPoint[i] + 0.5):(crossoverPoint[i +
                  1] - 0.5)] <- crosspart
            }
        }
        else {
            for (i in seq(1, (num.cross.locations - 1), by = 2)) {
                crosspart <- parents[1, (crossoverPoint[i] +
                  0.5):(crossoverPoint[i + 1] - 0.5)]
                parents[1, (crossoverPoint[i] + 0.5):(crossoverPoint[i +
                  1] - 0.5)] <- parents[2, (crossoverPoint[i] +
                  0.5):(crossoverPoint[i + 1] - 0.5)]
                parents[2, (crossoverPoint[i] + 0.5):(crossoverPoint[i +
                  1] - 0.5)] <- crosspart
            }
            crosspart <- parents[1, (crossoverPoint[num.cross.locations] +
                0.5):dim(parents)[2]]
            parents[1, (crossoverPoint[num.cross.locations] +
                0.5):dim(parents)[2]] <- parents[2, (crossoverPoint[num.cross.locations] +
                0.5):dim(parents)[2]]
            parents[2, (crossoverPoint[num.cross.locations] +
                0.5):dim(parents)[2]] <- crosspart
        }
        return(parents)
    }
    parents <- cross(parents, num.cross.locations = 2)
    return(parents)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
