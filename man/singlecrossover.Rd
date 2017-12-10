\name{singlecrossover}
\alias{singlecrossover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs a Genetic Cross at a single location.
}
\description{
Singlecrossover function performs a genetic cross at a single location. For two vectors or matrix, it randomly chooses one point and switch one piece of the segments.Singlecrossover function will return a 2*p matrix.
}
\usage{
singlecrossover(parents, num.cross.locations = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parents}{
\code{matrix of dimension 2 * p}
}
}
\details{

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
\code{\link{multiplecrossover}}
}
\examples{
data <- rbind(c(1,0,1,0,1,1,1), c(0,0,1,1,1,1,0))
singlecrossover(data, 1)


## The function is currently defined as
function (parents, num.cross.locations = 1)
{
    n <- dim(parents)[2]
    children <- matrix(0, nrow = 2, ncol = n)
    crossoverPoint <- sample(seq(1.5, n, by = 1), size = 1)
    children[1, ] <- c(parents[1, (1:(crossoverPoint - 0.5))],
        parents[2, (crossoverPoint + 0.5):n])
    children[2, ] <- c(parents[2, 1:(crossoverPoint - 0.5)],
        parents[1, (crossoverPoint + 0.5):n])
    return(children)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
