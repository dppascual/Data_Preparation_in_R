###############################################################################################
# 20150118, @dppascual
#
# plyr, dplyr y data.table
#
###############################################################################################

# Cargamos las librerías necesarias para el tratamiento y representación de datos.
library(MicroDatosEs)
library(ggplot2)

# Establecer el directorio de trabajo antes de ejecutarlo
#setwd("")

# Cargamos los datos del censo
censo <- censo2010("datos/MicrodatosCP_NV_per_nacional.txt", columns = c("edad", "sexo", "cpro", "ecivil", "factor", summary=FALSE))
censo.df <- as.data.frame(censo)

##############
# Proporción de divorcios por tramo de edad y provincia
#
# Filtro la edad entre 20 y 70 años para representar con detalle el rango de edad 
# donde se intuye se producirá una mayor variación en la proporción.

##### plyr #####
library(plyr)

divorces.by.age.pro <- ddply(censo.df[censo.df$edad > 20 & censo.df$edad < 70,], .(edad, cpro), function(df){
                                        divorces <- sum(df[df$ecivil == "Divorciado",]$factor)
                                        total <- sum(df$factor)
                                        proportion <- (divorces / total) * 100
                                        data.frame(proportion = proportion)
})

qplot(edad, proportion, data = divorces.by.age.pro, geom = "jitter", colour = cpro, facets = ~cpro, 
      xlab = "age", ylab = "proportion")

qplot(edad, proportion, data = divorces.by.age.pro, geom = "jitter", size = 5, colour = cpro, xlab = "age", 
      ylab = "proportion")


##### dplyr #####
detach("package:plyr", unload=TRUE) #Conflicto entre paquete plyr y dplyr
library(dplyr) 

total <- censo.df %>%
  filter(censo.df$edad > 20, censo.df$edad < 70) %>%
  group_by(edad, cpro) %>%
  summarise(total = sum(factor)) %>%
  arrange(desc(total))

divorces <- censo.df %>%
  filter(censo.df$edad > 20, censo.df$edad < 70, censo.df$ecivil == "Divorciado") %>%
  group_by(edad, cpro) %>%
  summarise(divorces = sum(factor)) %>%
  arrange(desc(divorces))

divorces.by.age.pro.tmp <- base::merge(total, divorces, by = c("edad", "cpro"))
divorces.by.age.pro <- data.frame(edad = divorces.by.age.pro.tmp$edad, cpro = divorces.by.age.pro.tmp$cpro, proportion = 
                                    (divorces.by.age.pro.tmp$divorces / divorces.by.age.pro.tmp$total) * 100)
  
qplot(edad, proportion, data = divorces.by.age.pro, geom = "jitter", colour = cpro, facets = ~cpro, 
      xlab = "age", ylab = "proportion")

qplot(edad, proportion, data = divorces.by.age.pro, geom = "jitter", size = 5, colour = cpro, xlab = "age", 
      ylab = "proportion")


##### data.table #####
library(data.table)

censo.dt <- as.data.table(censo.df)
tables()
setkey(censo.dt, edad, cpro, ecivil)
tables()

censo.dt <- censo.dt[edad > 20]
censo.dt <- censo.dt[edad < 70]

total <- censo.dt[, list(total = sum(factor)), by = c("edad", "cpro")]

divorces <- censo.dt[ecivil == "Divorciado"][, list(divorces = sum(factor)), by = c("edad", "cpro")]

divorces.by.age.pro.tmp <- base::merge(total, divorces, by = c("edad", "cpro"))
divorces.by.age.pro <- data.frame(edad = divorces.by.age.pro.tmp$edad, cpro = divorces.by.age.pro.tmp$cpro, proportion = 
                                    (divorces.by.age.pro.tmp$divorces / divorces.by.age.pro.tmp$total) * 100)


qplot(edad, proportion, data = divorces.by.age.pro, geom = "jitter", colour = cpro, facets = ~cpro, 
      xlab = "age", ylab = "proportion")

qplot(edad, proportion, data = divorces.by.age.pro, geom = "jitter", size = 5, colour = cpro, xlab = "age", 
      ylab = "proportion")


########
# 
# Comprobación diferencia de rendimiento 
#
#

##### plyr #####
detach("package:dplyr", unload=TRUE)
library(plyr)

system.time(
divorces.by.age.pro <- ddply(censo.df[censo.df$edad > 20 & censo.df$edad < 70,], .(edad, cpro), function(df){
  divorces <- sum(df[df$ecivil == "Divorciado",]$factor)
  total <- sum(df$factor)
  proportion <- (divorces / total) * 100
  data.frame(proportion = proportion)
})
)


##### dplyr #####
detach("package:plyr", unload=TRUE) #Conflicto entre paquete plyr y dplyr
library(dplyr) 

system.time(
total <- censo.df %>%
  filter(censo.df$edad > 20, censo.df$edad < 70) %>%
  group_by(edad, cpro) %>%
  summarise(total = sum(factor)) %>%
  arrange(desc(total))
)

system.time(
divorces <- censo.df %>%
  filter(censo.df$edad > 20, censo.df$edad < 70, censo.df$ecivil == "Divorciado") %>%
  group_by(edad, cpro) %>%
  summarise(divorces = sum(factor)) %>%
  arrange(desc(divorces))
)

system.time(
  divorces.by.age.pro.tmp <- base::merge(total, divorces, by = c("edad", "cpro"))
)
divorces.by.age.pro <- data.frame(edad = divorces.by.age.pro.tmp$edad, cpro = divorces.by.age.pro.tmp$cpro, proportion = 
                                    (divorces.by.age.pro.tmp$divorces / divorces.by.age.pro.tmp$total) * 100)


##### data.table #####

censo.dt <- as.data.table(censo.df)
tables()
setkey(censo.dt, edad, cpro, ecivil)
tables()

system.time(
censo.dt <- censo.dt[edad > 20]
)
system.time(
censo.dt <- censo.dt[edad < 70]
)

system.time(
total <- censo.dt[, list(total = sum(factor)), by = c("edad", "cpro")]
)

system.time(
divorces <- censo.dt[ecivil == "Divorciado"][, list(divorces = sum(factor)), by = c("edad", "cpro")]
)

system.time(
divorces.by.age.pro.tmp <- base::merge(total, divorces, by = c("edad", "cpro"))
)
divorces.by.age.pro <- data.frame(edad = divorces.by.age.pro.tmp$edad, cpro = divorces.by.age.pro.tmp$cpro, proportion = 
                                    (divorces.by.age.pro.tmp$divorces / divorces.by.age.pro.tmp$total) * 100)



# La sintaxis de dplyr es más clara que la de data.table, sin tener conocimientos sobre el paquete puedes
# intuir que está realizando el código. Aún así, me quedaría con data.table, debido a su rapidez en el
# procesamiento de los datos.

