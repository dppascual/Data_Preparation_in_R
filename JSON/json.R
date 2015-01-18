###############################################################################################
# 20150112, @dppascual
#
# JSON & XML
#
# Obtener una línea de bus EMT junto con sus paradas y el tiempo estimado que le queda
# al siguiente autobús y representarlo
#
# Recursos utilizados:
#     http://opendata.emtmadrid.es/Servicios-web/
#     http://curl.haxx.se/docs/manual.html
#     http://www.statmethods.net/
###############################################################################################


# Cargamos las librerías necesarias para el manejo de peticiones API REST y para el tratamiento y representación de datos.
library(RCurl)
library(ggplot2)
library(rjson)

# Establecer el directorio de trabajo antes de ejecutarlo
#setwd("")

# Bajada de datos de nº de líneas y correspondiente limpieza
url.lines <- "https://openbus.emtmadrid.es:9443/emt-proxy-server/last/bus/GetListLines.php"
id.client <- ""
passKey <- ""
fecha.actual <- format(Sys.Date(), "%d/%m/%Y")
lines.json <- postForm(url.lines, .opts = list(
                postfields = paste("idClient=", id.client, "&passKey=", passKey, "&SelectDate=", fecha.actual, sep = ""),
                httpheader = c('Content-Type' = 'application/x-www-form-urlencoded', Accept = 'application/json'),
                ssl.verifypeer = FALSE))
lines.tmp <- fromJSON(lines.json)
#lapply(lines.tmp, class)
#lapply(lines.tmp[[3]][[1]], class)
lines <- sapply(1:length(lines.tmp[[3]]), function(x) lines.tmp[[3]][[x]]$line)

# Obtenemos una linea aleatoria
line <- lines[sample(1:length(lines), 1)]

# Bajada de datos con las parada de una línea y correspondiente limpieza
url.ruta <- "https://openbus.emtmadrid.es:9443/emt-proxy-server/last/bus/GetRouteLines.php"
ruta.json <- postForm(url.ruta, .opts = list(
                postfields = paste("idClient=", id.client, "&passKey=", passKey, "&Lines=", line, "&SelectDate=", fecha.actual, sep = ""),
                httpheader = c('Content-Type' = 'application/x-www-form-urlencoded', Accept = 'application/json'),
                ssl.verifypeer = FALSE)) 
ruta.tmp <- fromJSON(ruta.json)
#lapply(ruta.tmp, class)
#lapply(ruta.tmp[[3]][[1]], class)
ruta.list <-lapply(1:length(ruta.tmp[[3]]), function(x) 
              data.frame(secDetail = ruta.tmp[[3]][[x]]$secDetail, node = ruta.tmp[[3]][[x]]$node, 
                   name = ruta.tmp[[3]][[x]]$name, latitude = ruta.tmp[[3]][[x]]$latitude, 
                   longitude = ruta.tmp[[3]][[x]]$longitude, stringsAsFactors = FALSE))
ruta.df <- do.call(rbind, ruta.list)
# Según la API, devuelve un conjunto de vértices para representar la línea, sin embargo, en la consulta
# no aparecen.
ruta.df <- ruta.df[ruta.df$secDetail == 10,]
#nrow(ruta.df)

# Bajada de datos con el tiempo estimado que le queda al siguiente autobús en llegar a la parada y su correspondiente limpieza
url.tiempos <- "https://openbus.emtmadrid.es:9443/emt-proxy-server/last/geo/GetArriveStop.php"
tiempos.tmp <- lapply(ruta.df$node, function(x) postForm(url.tiempos, .opts = list(
                postfields = paste("idClient=", id.client, "&passKey=", passKey, "&idStop=", x, sep = ""),
                httpheader = c('Content-Type' = 'application/x-www-form-urlencoded', Accept = 'application/json'),
                ssl.verifypeer = FALSE)))
tiempos.tmp2 <- lapply(tiempos.tmp, fromJSON)
tiempos.list.tmp <- lapply(2:length(tiempos.tmp2), function(x)
                                                    do.call(rbind, lapply(tiempos.tmp2[[x]][[1]], data.frame, stringsAsFactors=FALSE)))
tiempos.list.tmp2 <- lapply(tiempos.list.tmp, function(x)
                                          x[x$lineId == as.numeric(line),c("lineId","busTimeLeft")][1,])
tiempos <- do.call(rbind, tiempos.list.tmp2)$busTimeLeft

# Como la primera parada no indica tiempo, le añadimos un 0 
tiempos <- append(tiempos, 0, after = 0)
ruta.df$busTimeLeft <- tiempos

# Representa cada parada y el tiempo que le queda al siguiente autobús en llegar a dicha parada (en segundos)
ggplot(data = ruta.df, aes(x = longitude, y = latitude)) + geom_point(size = 6, aes(colour = busTimeLeft)) + 
  geom_text(size = 3, aes(label = name))

# Otra representación
plot(x = ruta.df$longitude, y = ruta.df$latitude, xlab = "Longitud", ylab = "Latitud",
     axes = TRUE, col = "blue", cex = 0.75, pch = 16, main = paste("Línea:", line, " "))
with(ruta.df, text(latitude~longitude, labels = ruta.df$name, cex = 0.5, pos = 1))
lines(x = ruta.df$longitude, y = ruta.df$latitude)
