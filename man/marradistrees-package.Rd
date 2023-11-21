\name{marradistrees-package}
\alias{marradistrees}
\docType{package}
\title{
\packageTitle{marradistrees}
}
\description{
\packageDescription{marradistrees}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{marradistrees}
\packageIndices{marradistrees}
}
\author{
\packageAuthor{marradistrees}

Maintainer: \packageMaintainer{marradistrees}
}
\references{
Alberto Marradi (1993), L'analisi Monovariata, Franco Angeli Editore, Milano (in Italian), ISBN: 9788820496876.
}
\keyword{ package }
\examples{
set.seed(123)  # an example with ten groups
m <- rnorm(10, mean = 5, sd = 1) # group means
s <- runif(10, min = 0, max = 2) # group standard deviations

marradistree(m, s)
marradistree(m, s, textv=TRUE)
}
