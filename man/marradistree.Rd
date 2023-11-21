\name{marradistree}
\alias{marradistree}
\title{
A function for plotting Marradi's trees.
}
\description{
The function plots a Marradi's tree (see Details). The tree trunk length is the mean of the variable and the tree crown radius is the standard deviation. Similar to boxplots, they can be conveniently used to compare a variable mean and standard deviation across subgroups.
}
\usage{
marradistree(m, s, xlab = NULL, ylab = NULL, textv = FALSE, lwd = 3, glab = "")
}

\arguments{
  \item{m}{
The vector of (sub)group means.
}
  \item{s}{
The vector of (sub)group standard deviations. It must have the same length of \code{m}. 
}
  \item{xlab,ylab}{
The horizontal and vertical axis labels.
}
  \item{textv}{
Texts the mean and the standard deviation values on each tree. Default to FALSE.
}
  \item{lwd}{
  The line width used to plot the tree.
}
  \item{glab}{
  An optional vector of group labels. If NULL, trees are labeled sequentially from left to right.
}
}
\details{
A Marradi's tree is a joint, tree-like, graphical representation of a numerical variable. The tree trunk is the mean of the variable and the radius of the tree crown is the standard deviation. It was proposed by Alberto Marradi in his 1993 book (see References). 
}
\value{
A plot with \code{n=length(m)} trees representing the mean and standard deviation of the variable across \code{n} subpopulations.
}
\references{
Alberto Marradi (1993), L'analisi Monovariata, Franco Angeli Editore, Milano (in Italian), ISBN: 9788820496876.
}
\author{
Massimo Cannas
}

\examples{
set.seed(123)  # an example with ten groups
m <- rnorm(10, mean = 5, sd = 1) # group means
s <- runif(10, min = 0, max = 2) # group standard deviations

marradistree(m, s)
marradistree(m, s, textv=TRUE)
}
 \keyword{ graphics }
