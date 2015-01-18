###############################################################################################
# 20150117, @dppascual
#
# Paquete R
#
# Recursos utilizados:
#     http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
###############################################################################################


calcEdist <- function(...){
  # Obtiene los dos puntos más cercanos
  #
  # Args:
  #   ...: Vectores con los n puntos de un espacio n-dimensional
  #
  # Returns:
  #   Distancia y vector con los dos puntos más cercanos
  #
  l.parameters <- list(...)
  vn.nparameters <- length(l.parameters)
  m.points <- mapply(cbind, l.parameters)
  df.combination <- merge(m.points, m.points, by = rownames(m.points))
  df.combination <- df.combination[-seq(from = 1, to = nrow(df.combination), by = (length(l.parameters[[1]]) + 1) ),]
  vn.distance.near <- c(0)
  
  for(i in 1:nrow(df.combination))
  {
    #vn.distance <- sqrt( sum( (df.combination[i,1:vn.nparameters] - df.combination[i,(vn.nparameters + 1):ncol(df.combination[i,])])^2 ) )
    vn.distance <- dist(matrix(df.combination[i,], nrow = 2, ncol = vn.nparameters, byrow = TRUE))
    
    if(vn.distance.near == 0 | vn.distance < vn.distance.near)
    {
      vn.distance.near <- vn.distance
      m.distance.near <- matrix(df.combination[i,], nrow = 2, ncol = vn.nparameters, byrow = TRUE)
    }  
  }
  
  return(list(vn.distance.near, m.distance.near))
}