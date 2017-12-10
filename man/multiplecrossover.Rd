
\name{multiplecrossover}
\alias{multiplecrossover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs a Genetic Cross with multiple translocation positions
}
\description{
Multiplecrossover function performs a genetic cross with multiple translocation positions. For two vectors or matrix, it randomly chooses more than one points and switch every other piece of the segments. Multiplecrossover function will return a 2*p matrix. The default value of number of locations is 2.
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
data <- rbind(c(1,0,1,0,1,1,1), c(0,0,1,1,1,1,0))
multiplecrossover(data, 3)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

