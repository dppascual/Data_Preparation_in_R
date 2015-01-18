###############################################################################################
# 20150111, @dppascual
#
# Clases S4 y mapas
#
# Recursos utilizados:
#     http://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
###############################################################################################

# Cargamos las librerías necesarias para el manejo de datos espaciales y para el tratamiento de datos.
library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(plyr)
library(reshape2)

# Establecer el directorio de trabajo antes de ejecutarlo
# setwd("")

# Cargamos los shapefiles y lo formateamos para que pueda ser representado por ggplot2
provincias.España <- readOGR(dsn = "datos/Provincias_ETRS89_30N","Provincias_ETRS89_30N")
provincias.España.df <- fortify(provincias.España, region = "Codigo")
provincias.España.df <- merge(provincias.España.df, provincias.España@data, by.x = "id", by.y = "Codigo")

# Cargamos y limpiamos el df con los viajeros entrados por provincia en los últimos años (2011, 2012 y 2013 - Encuesta de ocupación hotelera)
tmp <- lapply(c(11:13), function(x) 
                        read.table(paste(paste("datos/viajeros_provincias_20", x, sep = ""), ".csv", sep = ""), sep = ";", header = T, encoding = "UTF-8"))
# No se indican las columnas de unión, lo coge automáticamente por provincia.
viajeros.España <- join(join(tmp[[1]], tmp[[2]]), tmp[[3]])
viajeros.España <- viajeros.España[,-3]
colnames(viajeros.España)[2:4] <- c("2011", "2012", "2013")

provincias.España$Texto %in% viajeros.España$Provincia
provincias.España$Texto[which(!(provincias.España$Texto %in% viajeros.España$Provincia))]
levels(viajeros.España$Provincia)
# Se indexa con un factor, si fuera character no se podría
levels(viajeros.España$Provincia)[viajeros.España$Provincia[which(!(viajeros.España$Provincia %in% provincias.España$Texto))]] <- c("Vizcaya", "Guipúzcoa", "Gerona", "Orense")
provincias.España$Texto %in% viajeros.España$Provincia

# Convertimos en formato largo para trabajar con los datos
viajeros.España.melt <- melt(viajeros.España, id = "Provincia")

plot.data <- merge(provincias.España.df, viajeros.España.melt, by.x = "Texto", by.y = "Provincia")
plot.data <- plot.data[order(plot.data$order), ]
#colnames(viajeros.España.melt)[2:3] <- c("año", "visitas")

ggplot(data = plot.data, aes(x = long, y = lat, fill = value, group = group)) + 
  geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + facet_wrap(~variable)
