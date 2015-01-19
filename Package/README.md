# Instalación a través de github

> library(devtools)
> install_github("dppascual/edist")
> library(edist)
> ?edist
> example(edist)

edist> x <- sample(0:5000, 50, replace = TRUE)

edist> y <- sample(0:5000, 50, replace = TRUE)

edist> calcEdist(x,y)
[[1]]
         1
2 83.23461

[[2]]
     [,1] [,2]
[1,] 4221 2495
[2,] 4269 2427
