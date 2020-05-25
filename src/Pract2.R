library(stringr)
library(lubridate)
library(VIM)

# Carga del archivo
setwd("C:/Users/clian/Desktop/Data Science/Tipologia y ciclo de vida datos/Pract2/Lianez_Padron")
cars <-read.csv("USA_cars_datasets.csv",header=TRUE)
head(cars)

#Examino el tipo de datos de cada variable
str(cars)

# Prescindimos de las variables X, lot y condition
cars <- cars[,-(1)]
cars <- cars[,-(9)]
cars <- cars[,-(11)]
str(cars)
head(cars)

#variables cualitativas
table(cars$brand)
table(cars$model)
table(cars$title_status)
table(cars$color)
table(cars$state)
table(cars$country)


#Normalizar colores:  beige, black, blue, brown, orange, gold,red, silver, white, gray, green, purple, yellow, no_color

cars$color <- str_replace(cars$color, ".*beige.*", "beige")
cars$color <- str_replace(cars$color, ".*black.*", "black")
cars$color <- str_replace(cars$color, ".*blue.*", "blue")
cars$color <- str_replace(cars$color, ".*brown.*", "brown")
cars$color <- str_replace(cars$color, ".*orange.*", "orange")
cars$color <- str_replace(cars$color, ".*gold.*", "gold")
cars$color <- str_replace(cars$color, ".*red.*", "red")
cars$color <- str_replace(cars$color, ".*silver.*", "silver")
cars$color <- str_replace(cars$color, ".*white.*", "white")
cars$color <- str_replace(cars$color, ".*gray.*", "gray")
cars$color <- str_replace(cars$color, ".*green.*", "green")
cars$color <- str_replace(cars$color, ".*purple.*", "purple")
cars$color <- str_replace(cars$color, ".*yellow.*", "yellow")
cars$color <- str_replace(cars$color, "burgundy", "red")
cars$color <- str_replace(cars$color, "charcoal", "black")
cars$color <- str_replace(cars$color, "color:", "no_color")
cars$color <- str_replace(cars$color, "maroon", "brown")
cars$color <- str_replace(cars$color, "magnetic metallic", "black")
cars$color <- str_replace(cars$color, "royal crimson metallic tinted clearcoat", "purple")
cars$color <- str_replace(cars$color, "turquoise", "blue")
cars$color <- str_replace(cars$color, "tan", "beige")
cars$color <- str_replace(cars$color, "guard", "black")


table(cars$color)


# valores perdidos(0 o vacíos)

#Con la siguiente instrucción vemos si hay registros que están incompletos
complete.cases(cars)

#Analizamos valores perdidos 
sapply(cars, function(x) sum(is.na(x)))


years <- subset(cars$year, subset = cars$mileage == 0)
print(years)



#imputación del precio cuyo valor es 0

cars$price <- ifelse(cars$price == 0, NA, cars$price)
cars$mileage <- ifelse(cars$mileage == 0, NA, cars$mileage)


cars$price <- kNN(cars)$price
sum(is.na(cars$price))
sum(cars$price == 0)
cars$mileage <- kNN(cars)$mileage
sum(is.na(cars$mileage))
sum(cars$mileage == 0)



#valores outliers en variables cuantitativas

boxplot(cars$price)

boxplot(cars$year)

boxplot(cars$mileage)


#Agrupaciones de datos

#Agrupar por año de primera matriculación, diferenciando los que tienen una antigüedad de 20 años o más.

year <- subset(cars$year, subset = cars$year >= 2000)
price <- subset(cars$price, subset = cars$year >= 2000)
mileage <- subset(cars$mileage, subset = cars$year >= 2000)
cochesMenos20 <- data.frame(price, year, mileage)
head(cochesMenos20)

year <- subset(cars$year, subset = cars$year < 2000)
price <- subset(cars$price, subset = cars$year < 2000)
mileage <- subset(cars$mileage, subset = cars$year < 2000)
cochesMas20 <- data.frame(price, year, mileage)
head(cochesMas20)

#Agrupar coches por color blanco o negro.

precioCochesBlancos <- subset(cars$price, subset = cars$color == "white")
precioCochesNegros <- subset(cars$price, subset = cars$color == "black")


#Comparar por marcas de vehículos.

# correlaciones 

#price y year

plot(cochesMenos20$price, cochesMenos20$year, xlab = "Precio $", ylab = "Año primera matriculación")
abline(lm(cochesMenos20$year~cochesMenos20$price),col="red",lwd=4)
cor(x=cochesMenos20$price, y=cochesMenos20$year)

#price y mileage
plot(cochesMenos20$price, cochesMenos20$mileage, xlab = "Precio $", ylab = "Kilometraje en millas")
abline(lm(cochesMenos20$mileage~cochesMenos20$price),col="red",lwd=4)
cor(x=cochesMenos20$price, y=cochesMenos20$mileage)

#Contraste de hipótesis color blanco Vs negro
t.test(precioCochesBlancos, precioCochesNegros, alternative = "less")

#Modelo de regresión lineal

#predecir precio en función del año de matriculación y kilometraje
modelo1 <- lm(formula = cars$price ~ cars$year + cars$mileage)
summary(modelo1)

#predecir precio en función del año de matriculación, kilometraje y marca
modelo2 <- lm(formula = cars$price ~ cars$year + cars$mileage + cars$brand)
summary(modelo2)

#predecir precio en función del año de matriculación, kilometraje, marca y color
modelo3 <- lm(formula = cars$price ~ cars$year + cars$mileage + cars$brand + cars$color)
summary(modelo3)

#Tabla con los coeficientes de determinación
coeficientes <- matrix(c( 1, summary(modelo1)$r.squared,
                         2, summary(modelo2)$r.squared,
                         3, summary(modelo3)$r.squared),
                         ncol = 2, byrow = TRUE)
print(coeficientes)

#gráficos

#Varianza de los errores/residuos constante
plot(fitted.values(modelo3),rstandard(modelo3), xlab="Valores ajustados", ylab="Residuos estandarizados")  
abline(h=0) 

#Distribución normal de los residuos
qqnorm(modelo3$residuals)
qqline(modelo3$residuals)

