\name{edist-package}
\alias{edist-package}
\alias{edist}
\docType{package}

\title{
Euclidean distance between two vectors
}

\description{
Get shortest euclidean distance between two vectors in subset.
}

\details{
\tabular{ll}{
Package: \tab edist\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-18\cr
License: \tab GPL-2\cr
}

}
\author{
Daniel Pe<c3><b1>a

Maintainer: Daniel Pe<c3><b1>a <daniel.pena@imdea.org>
~~ The author and/or maintainer of the package ~~
}

\keyword{ Euclidean }

\examples{
x <- sample(0:5000, 50, replace = TRUE)
y <- sample(0:5000, 50, replace = TRUE)
calcEdist(x,y)
}
