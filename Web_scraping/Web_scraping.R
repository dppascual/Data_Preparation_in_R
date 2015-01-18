###############################################################################################
# 20150110, @dppascual
#
# Web scraping, texto y fechas
#
# Recursos utilizados:
#     Foros de http://stackoverflow.com/
###############################################################################################


# Cargamos la librería XML para realizar "web scraping" y "parsear"
library(XML)
library(stringr)

# Descargamos información bursátil de http://goo.gl/yD2Bwb
url.ibex <- "http://goo.gl/yD2Bwb"
raw.data <- readLines(url.ibex, warn="F") 
#tablas <- readHTMLTable(raw.data) --> Manera cómoda de obtener la tabla, se realiza de manera manual
#                                      para estudiar el árbol HTML y poder obtener cualquier elemento.

# Analizamos la estructura HTML
bolsa.doc  <- htmlTreeParse(raw.data, error=function(...){}, useInternalNodes = TRUE)
class(bolsa.doc)
bolsa.root = xmlRoot(bolsa.doc)
# Cuantos nodos hijos
xmlSize(bolsa.root)
# Nombres de los nodos hijos
xmlSApply(bolsa.root, xmlName)
# Cuantos nodos en cada hijo 
xmlSApply(bolsa.root, xmlSize)
# Obtenemos el nodo body del documento html
class(xmlChildren(bolsa.root))
typeof(xmlChildren(bolsa.root)) # Se implementa como una lista.
bolsa.body = xmlChildren(bolsa.root)$body

# Se podría haber trabajado directamente con bolsa.doc, ej.: 
# xpathSApply(bolsa.doc, "//table[@title='Acciones']/*/tr[1]/th/a", xmlValue)
# Por motivos educativos se ha preferido utilizar la para analizar la estructura HTML
# y en futuros casos poder .
cabecera <- xpathSApply(bolsa.body, "//table[@title='Acciones']/*/tr[1]/th", xmlValue)
contenido <- xpathSApply(bolsa.body, "//table[@title='Acciones']/*/tr/td", xmlValue)

# Convertir a df y  limpieza de datos
cabecera <- sapply(cabecera, function(x)
                                gsub("^(\n){1,}|(\\*){1,}$", "", x))
bolsa.tabla <- data.frame(matrix(contenido, ncol = 12, byrow = TRUE), stringsAsFactors = FALSE)
colnames(bolsa.tabla) <- cabecera
bolsa.tabla <- bolsa.tabla[,-5]
bolsa.tabla <- bolsa.tabla[order(bolsa.tabla$TKR),]
bolsa.tabla$Último <- as.numeric(gsub(",", ".", bolsa.tabla$Último))
bolsa.tabla$Dif. <- as.numeric(gsub(",", ".", bolsa.tabla$Dif.))
bolsa.tabla$'Dif. %' <- as.numeric(gsub(",", ".", bolsa.tabla$'Dif. %'))
bolsa.tabla$Max. <- as.numeric(gsub(",", ".", bolsa.tabla$Max.))
bolsa.tabla$Min. <- as.numeric(gsub(",", ".", bolsa.tabla$Min.))
bolsa.tabla$Volumen <- as.numeric(gsub("\\.", "", bolsa.tabla$Volumen))
bolsa.tabla$Capital <- as.numeric(gsub("\\.", "", gsub("[:space]*n.d.[:space]*", NA, bolsa.tabla$Capital)))
# No se trata la columna 'Rt/Div' como numérica para incluir el signo %.
bolsa.tabla$'Rt/Div' <- gsub(",", ".", (gsub("[:space]*n.a.[:space]*", NA, bolsa.tabla$'Rt/Div')))
bolsa.tabla$PER <- as.numeric(gsub(",", ".", gsub("[:space]*n.a.[:space]*", NA, bolsa.tabla$PER)))

bolsa.fecha <- function(x)
{
  if(str_detect(x, "[0-9]{,2}/[0-9]{,2}/[0-9]{4}"))
  {
    return(as.POSIXct(x, format = "%d/%m/%Y"))
  }
  
  if(str_detect(x, "[0-9]{,2}:[0-9]{2}"))
  {
    return(as.POSIXct(x, format = "%H:%M"))
  }
}

bolsa.tabla$Hora <- sapply(bolsa.tabla$Hora, bolsa.fecha)
class(bolsa.tabla$Hora) <- c("POSIXct", "POSIXt")
bolsa.tabla