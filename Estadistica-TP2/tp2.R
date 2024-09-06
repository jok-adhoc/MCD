## CARGA DE LIBRERIAS ##

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(DescTools)
library(car)

## Funciones necesarias ##

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Seteo de WD ##

setwd('C:/Users/josek/Desktop/Maestria/Estadistica/Trabajos/TP2')

## Carga inicial de dataset ##

data<- read.csv("Diabetes_TP.csv", sep=";")

any(duplicated(data))
sum(duplicated(data))

summary(data)
glimpse(data)

data$DBT<-as.factor(data$DBT)
data$Grosor_Piel<-factor(data$Grosor_Piel, levels=c("delgada",
                                                    "moderada",
                                                    "gruesa"
                                                    ))
summary(data)
glimpse(data)

# Creación de variable BMI_OMS

data = data %>% 
  mutate(
    BMI_OMS = case_when(
      is.na(BMI) ~ NA_character_,
      BMI < 18.5 ~ 'bajo',
      BMI < 25 ~ 'normal',
      BMI < 30 ~ 'sobrepeso',
      BMI < 35 ~ 'obesidad_1',
      BMI < 40 ~ 'obesidad_2',
      .default = 'obesidad_3'
    )
  )

data$BMI_OMS<-factor(data$BMI_OMS, levels=c("bajo",
                                            "normal",
                                            "sobrepeso",
                                            "obesidad_1",
                                            "obesidad_2",
                                            "obesidad_3"
))


#1 porcentaje de mujeres adultas que tienen diabetes tipo II

n = nrow(data)
y = sum(data$DBT == "1")

BinomCI(y, n, conf.level = 0.95, method = "clopper-pearson")

#2 glucosa promedio de pacientes diabeticas

#Primero obtengo un nuevo dataset solo con las personas diabeticas

pacientes_diab <- data %>% filter(DBT == "1" & !is.na(Glucosa))

# En principio desconocemos como se distribuye la glucosa en las pacientes con diabetes, por eso vamos a graficar 

# histograma
ggplot(pacientes_diab, aes(x = Glucosa)) + 
  geom_histogram( col='black', fill='slateblue')+
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="Glucosa [mg/dl]") +
  ggtitle("Histograma de frecuencia absoluta de niveles de Glucosa")

#La distribución parece ser simetrica pero no podemos asegurar normalidad. Dada la cantidad de muestras usamos TCL para considerar
#normalidad del estadistico x raya

MeanCI(pacientes_diab$Glucosa, method = "classic", conf.level = 0.95)

#3 dispersion de gluicosa en pacientes adultas diabeticas del instituto

#Segumios usando el dataset previo pacientes_diab

IC_var = VarCI(pacientes_diab$Glucosa, method = "bonett", conf.level = 0.95) #uso bonett porque no tengo garantizada la normalidad

sqrt(IC_var)

#4 ¿La presión diastólica media de las pacientes adultas diabéticas es similar a la de las pacientes que no presentan diabetes?

pacientes_no_diab_pad <- data %>% filter(DBT == "0" & !is.na(PAD))
pacientes_diab_pad <- data %>% filter(DBT == "1" & !is.na(PAD))

data_clean <- data %>% filter(!is.na(PAD))

# Boxplot
boxplot(PAD~ DBT, data = data_clean,
        xlab="DBT", ylab="PAD [mmHg]",
        main="Boxplot de niveles de PAD en mmHg segun si tienen diabetes tipo II o no",
        col = c("#FFE0B2", "#FFA726"),
        names = c("NO", "SI"))

MeanCI(pacientes_diab_pad$PAD, method = "classic", conf.level = 0.95)
MeanCI(pacientes_no_diab_pad$PAD, method = "classic", conf.level = 0.95)

# Estimando la media con intervalos de confianza, con un 95%, podemos decir que las medias
# de las personas con diabetes es mayor que las que si tienen

#5  ¿Las pacientes del instituto, en promedio, presentan obesidad? Obesidad, según la OMS corresponde a 
# un valor de BMI>30

# Aquí presentamos primero nuestro parametro a buscar, mu: promedio de BMI de las pacientes del instituto
# Debido a que nos preguntamos si en promedio presentan obesidad, presentamos nuestras hipótesis

# H0: mu = 30
# Ha: mu>30

datos_BMI <- data %>% filter(!is.na(BMI))

# Si bien desconocemos la distribución de BMI en las pacientes, con 757 muestras podemos pensar
# en una distribución de xraya normal por el Teorema Central del Limite. Lo que nos deja aplicar ahora
# la prueba mediante la T-Student


t.test(datos_BMI$BMI,
       alternative = "greater",
       mu = 30,
       conf.level = 0.95)

# El p-value es menor a 2.2e-16, obviamente menor a 0.05. Esto, confirmandolo con el intervalo
# unilateral, permite rechazar la H0.

# En conclusion, podemos afirmar, trabajando con niveles de confianza del 95%, que en promedio las pacientes
# presentan obesidad

# 6 ¿Podemos decir que el porcentaje de mujeres adultas con diabetes tipo II de nuestra institución es 
# menor al del Instituto de Endocrinología de Capital Federal (37.5% )?

# Estamos buscando entender el valor de pi: proporción de mujeres adultas en la institución

# Este caso, nuestras hipótesis son:

# H0: pi = 0.375
# Ha: pi < 0.375

# Aquí debemos conocer, la cantidad de personas en la muestra con diabetes y el tamaño de la muestra

# x: personas de la muestra con diabetes
# n: tamaño de muestra

x <- sum(data$DBT == "1")
n <- nrow(data)

binom.test(x = x,
           n = n,
           p =  0.375,
           alternative = "less",
           conf.level = 0.95)

# Para este caso, el valor de p-value es 0.07254, mayor que 0.05. Esto no lleva a decir
# que no hay evidencia muestral suficiente para rechazar la hipótesis de que la proporcion de pacientes
# en el instituto sea mayor o igual a la del instituto de Capital Federal.


# 7 Si tuviésemos una muestra de mayor tamaño, por ejemplo de 900 mujeres, y hubiésemos observado el
# mismo porcentaje de pacientes adultas diabéticas ¿Cambia la conclusión de la pregunta anterior? ¿Y
# si aumentamos el tamaño muestral a 1500 cambia la conclusión?

# para el caso de n=900, deberiamos tener un x ~ 314
binom.test(x = 314,
           n = 900,
           p =  0.375,
           alternative = "less",
           conf.level = 0.95)

# Para este caso, el p-value, al igual que el IC, nos siguen demostrando la inexistencia de evidencia muestral
# para rechazar H0

#En el caso de n=1500, con x~523:
binom.test(x = 523,
           n = 1500,
           p =  0.375,
           alternative = "less",
           conf.level = 0.95)
# Para este caso el p-value = 0.0184 lo que nos lleva, junto al IC, a rechazar la hipotesis nula
# y por lo tanto afirmar con el 95% de confianza, que la proporcion es menor al instituto de Capital Federal



# 8 Hace 6 meses el grupo de médicos del instituto decidió intensificar las recomendaciones de un “estilo
# de vida saludable” en el grupo de pacientes adultas diabéticas. Esto se vio acompañado de
# estrategias educativas y del uso de una App de celular. Se pretende de esta manera que la glucosa
# en sangre de las pacientes, en promedio, sea menor a 150 mg/dl. ¿Se logró este objetivo?

# Primero definamos qué parte de la muestra trabajar. Para este caso es el grupo de personas diabeticas
# Al querer medir la glucosa, obtenemos el siguente subset de datos:

pacientes_diab_glucosa <- data %>% filter(DBT == "1" & !is.na(Glucosa))

# Nuestro parametro a entender es mu: glucosa en sangre promedio de las mujeres con diabetes tipo II

# Al querer evaluar si se logró el objetivo de tener una glucosa promedio menor a 150 mg/dl, se plantean
# las siguientes hipótesis

# H0: mu = 150
# Ha: mu < 150

# Como en casos anteriores, no concemos la distribución de la Glucosa, pero por la cantidad de muestras
# podemos entender que la variable aleatoria "promedio muestral de glucosa" tendrá una distribución
# Normal. Al no tener informacion del desvío, se usa una prueba utilizando la T-Student

t.test(pacientes_diab_glucosa$Glucosa,
       alternative = "less",
       mu = 150,
       conf.level = 0.95)


# El muy bajo valor de p-value, acompañado del resultado del IC unilateral, nos lleva a rechazar la hipotesis nula
# y conlcuir que se evidencia un cumplimiento del objetivo


# 9 Corroborar la conclusión del punto 4 con un test de hipótesis.

#Prueba de normalidad

ad.test(pacientes_diab_pad$PAD)  #Rechazo H0
ad.test(pacientes_no_diab_pad$PAD)  #Rechazo H0


#prueba de igualdad de varianza

leveneTest (PAD ~ DBT, data) #varianzas iguales

#No rechazo H0

# Prueba de igualdad de media

t.test(x = pacientes_diab_pad$PAD,
       y= pacientes_no_diab_pad$PAD,
       mu = 0,
       var.equal = TRUE,
       conf.level = 0.95)
# Rechazo H0