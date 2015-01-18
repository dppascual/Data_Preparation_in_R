\name{calcEdist}
\alias{calcEdist}

\title{
Euclidean distance between two vectors
}

\description{
Get shortest euclidean distance between two points in subset.
}

\usage{
calcEdist(...)
}

\arguments{
  \item{\dots}{
Integers vectors with n points of a n-dimensional space. The first position of each vector represents
one point in a n-dimensional space; the second position represents other point in the same n-dimensional
space, the third position....
}

}
\value{
\item{comp1 }{Integers Vector with the position X of the point}
\item{comp2 }{Integers Vector with the position Y of the point}
\item{comp3 }{Integers Vector with the position Z of the point}
.
.
.
\item{comN }{Integers Vector with the position N of the point}
}

\author{
Daniel Pe<c3><b1>a <daniel.pena@imdea.org>
}

\examples{
x <- sample(0:5000, 50, replace = TRUE)
y <- sample(0:5000, 50, replace = TRUE)
calcEdist(x,y)

x <- sample(0:5000, 50, replace = TRUE)
y <- sample(0:5000, 50, replace = TRUE)
z <- sample(0:5000, 50, replace = TRUE)
calcEdist(x,y,z)

function (...) 
{
    l.parameters <- list(...)
    vn.nparameters <- length(l.parameters)
    m.points <- mapply(cbind, l.parameters)
    df.combination <- merge(m.points, m.points, by = rownames(m.points))
    df.combination <- df.combination[-seq(from = 1, to = nrow(df.combination), 
        by = (length(l.parameters[[1]]) + 1)), ]
    vn.distance.near <- c(0)
    for (i in 1:nrow(df.combination)) {
        vn.distance <- dist(matrix(df.combination[i, ], nrow = 2, 
            ncol = vn.nparameters, byrow = TRUE))
        if (vn.distance.near == 0 | vn.distance < vn.distance.near) {
            vn.distance.near <- vn.distance
            m.distance.near <- matrix(df.combination[i, ], nrow = 2, 
                ncol = vn.nparameters, byrow = TRUE)
        }
    }
    return(list(vn.distance.near, m.distance.near))
  }
}

\keyword{ calcEdist }
\keyword{ Euclidean }
