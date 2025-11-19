#JULIO DAVID RUIZ MENDOZA   06.05.2021

#Paquetes
library("tibble")
library("dplyr")


#1. Leer el fichero biom2003.csv y asignarlo al objeto biom2003

biom2003 <- read.csv2("biom2003.csv")


#2. Mostrar las 10 primeras filas de biom2003

head(biom2003, 10)


#3. Mostrar la estructura del objeto biom2003

str(biom2003)


#4. Cambiar el tipo de int a factor de las variables Grupo, Sexo, Ojos y Tipo. Comprobar que se ha
#cambiado correctamente.

biom2003$Grupo <-  as.factor(biom2003$Grupo) 
biom2003$Sexo <-  as.factor(biom2003$Sexo)
biom2003$Ojos <-  as.factor(biom2003$Ojos)
biom2003$Tipo <-  as.factor(biom2003$Tipo)

class(biom2003$Grupo)
is.factor(biom2003$Sexo)
str(biom2003)


#5. Generar un resumen de estadística básica del objeto biom2003

summary(biom2003)


#6. Representar mediante un diagrama de dispersión o de puntos las variables Peso y Altura

plot(biom2003$Peso, biom2003$Altura, xlab = "Peso", ylab = "Altura",
     main = "Mapa de dispersión", col=biom2003$Sexo)
legend(x = "topleft", legend = c("1", "2"), fill = c("Black", "Red"), title = "Sexo")


#7. Representar gráficamente mediante un diagrama de cajas y bigotes, el Peso por Grupo y por Sexo

plot(biom2003$Grupo, biom2003$Peso, xlab="Grupo", ylab="Peso",
     main="Diagrama de cajas y bigotes")
plot(biom2003$Sexo, biom2003$Peso, xlab="Sexo", ylab="Peso",
     main="Diagrama de cajas y bigotes")


#8. Seleccionar los individuos (registros) del grupo 3, se asignan al objeto biom2003g3 y se representan 
#gráficamente todas las variables 

biom2003g3 <- biom2003[biom2003$Grupo==3,]
plot(biom2003g3)


#9. Seleccionar todos los individuos con una altura superiora 175 cm. La selección se guarda en el objeto
#biomAltos y se genera un resumen de estadística básica por grupos

biomAltos <- biom2003[biom2003$Altura > 175,]
summary(biomAltos)


#10. Genera un modelo lineal que relacione la variable Pie con la variable Altura. Guardarlo en el objeto
#modelo y mostrar el resumen de modelo (hasta 1 punto).

modelo <- lm(biom2003$Pie~biom2003$Altura)
modelo
summary(modelo)


#11. Representar la recta de regresión del objeto modelo sobre el diagrama de puntos de las variables Pie y
#Altura

plot(biom2003$Altura, biom2003$Pie, xlab = "Altura", ylab = "Pie",
     main = "Ajuste lineal",
     col= biom2003$Sexo, pch= 19 )
legend(x = "topleft", legend = c("1", "2"), fill = c("Black", "Red"),
       title = "Sexo")
abline(modelo, col="blue")

