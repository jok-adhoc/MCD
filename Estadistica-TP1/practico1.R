## CARGA DE LIBRERIAS ##

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

## Funciones necesarias ##

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Seteo de WD ##

setwd('C:/Users/josek/Desktop/Maestria/Estadistica/Trabajos/TP1')

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


#####################################
####ANALISIS UNIVARIADO##############
#####################################

# ------------------------------------
# Glucosa

# Elimina las filas con NA en la columna Glucosa
data_clean <- data %>% filter(!is.na(Glucosa))

# Posición

quantile(data_clean$Glucosa)
quantile(data_clean$Glucosa,probs = seq(0, 1, by = 0.1))

# Dispersión y resumen

data_clean %>%
  summarise(
    mean = mean(Glucosa),
    median = median(Glucosa),
    mode = Mode(Glucosa),
    min=min(Glucosa),
    max=max(Glucosa),
    rango= max(Glucosa) - min(Glucosa),
    iqr= IQR(Glucosa),
    var=var(Glucosa),
    sd = sd(Glucosa),
    cv=(sd(Glucosa)/mean(Glucosa))*100)

# histograma
ggplot(data_clean, aes(x = Glucosa)) + 
  geom_histogram( col='black', fill='slateblue')+   #col= color linea borde de la barra #fill= color de la barra
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="Glucosa [mg/dl]") +
  ggtitle("Histograma de frecuencia absoluta de niveles de glucosa en sangre [mg/dl]")

# boxplot
boxplot(data_clean$Glucosa, col="lightgreen", main="Boxplot de niveles de glucosa", ylab="Glucosa [mg/dl]")

# ------------------------------------
# PAD

# Elimina las filas con NA en la columna PAD
data_clean <- data %>% filter(!is.na(PAD))


#Posición

quantile(data_clean$PAD)
quantile(data_clean$PAD,probs = seq(0, 1, by = 0.1))

# Dispersión y resumen

data_clean %>%
  summarise(
    mean = mean(PAD),
    median = median(PAD),
    mode = Mode(PAD),
    min=min(PAD),
    max=max(PAD),
    rango= max(PAD) - min(PAD),
    iqr= IQR(PAD),
    var=var(PAD),
    sd = sd(PAD),
    cv=(sd(PAD)/mean(PAD))*100)

# histograma

ggplot(data_clean, aes(x = PAD)) + 
  geom_histogram( col='black', fill='slateblue')+   #col= color linea borde de la barra #fill= color de la barra
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="PAD [mmHg]") +
  ggtitle("Histograma de frecuencia absoluta de niveles de PAD [mmHg]")

# Boxplot
boxplot(data_clean$PAD, col="lightgreen", main="Boxplot de niveles de PAD", ylab="PAD [mmHg]")

# ------------------------------------
# BMI

# Elimina las filas con NA en la columna BMI
data_clean <- data %>% filter(!is.na(BMI))

# Posición

quantile(data_clean$BMI)
quantile(data_clean$BMI,probs = seq(0, 1, by = 0.1))

# Dispersión y resumen

data_clean %>%
  summarise(
    mean = mean(BMI),
    median = median(BMI),
    mode = Mode(BMI),
    min=min(BMI),
    max=max(BMI),
    rango= max(BMI) - min(BMI),
    iqr= IQR(BMI),
    var=var(BMI),
    sd = sd(BMI),
    cv=(sd(BMI)/mean(BMI))*100)

# histograma
ggplot(data_clean, aes(x = BMI)) + 
  geom_histogram( col='black', fill='slateblue')+   #col= color linea borde de la barra #fill= color de la barra
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="BMI [kg/m2]") +
  ggtitle("Histograma de frecuencia absoluta de niveles de BMI [kg/m2]")

# Boxplot
boxplot(data_clean$BMI, col="lightgreen", main="Boxplot de niveles de BMI", ylab="BMI [kg/m2]")


# ------------------------------------
#BMI OMS


# Elimina las filas con NA en la columna BMI_OMS
data_clean <- data %>% filter(!is.na(BMI_OMS))

# Tabla de frecuencias

tabyl(data_clean, BMI_OMS)

data_clean %>% 
  tabyl(BMI_OMS) %>% 
  adorn_totals %>% 
  adorn_pct_formatting(digits = 1) 

conteo <- data_clean %>% 
  filter(BMI_OMS == Mode(data_clean$BMI_OMS)) %>% 
  count(BMI_OMS) %>% 
  pull()
conteo_100 <- ceiling(conteo / 100) * 100

#Gráfico de barras - verticales
barplot(table(data_clean$BMI_OMS),
        main="Cantidad de pacientes según su clasificación de BMI por la OMS",
        ylim=c(0,conteo_100),
        ylab ="Cantidad de pacientes",
        xlab="BMI OMS", 
        cex.lab=1,                           
        cex.names=0.8,                     
        font.axis=1,                       
        col="skyblue3",
        las=1)


#Gráfico de barras - verticales - porcentaje

BMI_OMS_table <- table(data_clean$BMI_OMS)
BMI_OMS_percentage <- prop.table(BMI_OMS_table) * 100

graf1<-barplot(BMI_OMS_percentage,
               main = "Porcentaje de pacientes según su clasificación de BMI por la OMS", 
               xlab = "BMI OMS", 
               ylab = "Porcentaje de pacientes", 
               col = "lightblue", 
               ylim=c(0,max(educ_percentage) + 20),
               cex.lab=1,                           
               cex.names=0.8, 
               font.axis=1,
               las=1)


# Añadir etiquetas de porcentaje en las barras
text(x = graf1, 
     y = BMI_OMS_percentage, 
     labels = round(BMI_OMS_percentage, 1), 
     pos = 3) 

# ------------------------------------
# Grosor Piel


# Simplemente asignamos el dataset a data_clean para mantener el estandar
data_clean <- data

# Tabla de frecuencias

tabyl(data_clean, Grosor_Piel)

data_clean %>% 
  tabyl(Grosor_Piel) %>% 
  adorn_totals %>% 
  adorn_pct_formatting(digits = 1) 

conteo <- data_clean %>% 
  filter(Grosor_Piel == Mode(data_clean$Grosor_Piel)) %>% 
  count(Grosor_Piel) %>% 
  pull()
conteo_100 <- ceiling(conteo / 100) * 100

# Gráfico de barras - verticales
barplot(table(data_clean$Grosor_Piel),
        main="Cantidad de pacientes según su clasificación de grosor de piel",
        ylim=c(0,conteo_100),
        ylab ="Cantidad de pacientes",
        xlab="Grosor de piel", 
        cex.lab=1,                           
        cex.names=0.8,                       
        font.axis=1,                    
        col="skyblue3",
        las=1) 


# Gráfico de barras - verticales - porcentaje

Grosor_Piel_table <- table(data_clean$Grosor_Piel)
Grosor_Piel_percentage <- prop.table(Grosor_Piel_table) * 100

graf1<-barplot(Grosor_Piel_percentage,
               main = "Porcentaje de pacientes según su clasificación de grosor de piel", 
               xlab = "Grosor de piel", 
               ylab = "Porcentaje de pacientes", 
               col = "lightblue", 
               ylim=c(0,max(educ_percentage) + 20),
               cex.lab=1,
               cex.names=0.8,
               font.axis=1,
               las=1)


# Añadir etiquetas de porcentaje en las barras
text(x = graf1, 
     y = Grosor_Piel_percentage, 
     labels = round(Grosor_Piel_percentage, 1), 
     pos = 3) # pos = 3 coloca las etiquetas encima de las barras


# ------------------------------------
# DBT


# Simplemente asignamos el dataset a data_clean para mantener el estandar
data_clean <- data

# Tabla de frecuencias
tabyl(data_clean, DBT)

data_clean %>% 
  tabyl(DBT) %>% 
  adorn_totals %>% 
  adorn_pct_formatting(digits = 1) 

conteo <- data_clean %>% 
  filter(DBT == Mode(data_clean$DBT)) %>% 
  count(DBT) %>% 
  pull()
conteo_100 <- ceiling(conteo / 100) * 100

#gráfico de sectores
porcentajes <- as.numeric(round(((prop.table(table(data_clean$DBT)))*100),2))
etiquetas <- c("No", "Si")   #Mejorar reconocer el 0 y 1

pie(porcentajes, etiquetas,col = c("lightblue","lightgreen"),
    main = "Distribución de pacientes según si presentan Diabetes tipo II o no")



#####################################
####ANALISIS BIVARIADO###############
#####################################

# ------------------------------------
# Glucosa vs DBT
data_clean <- data %>% filter(!is.na(Glucosa))

# Medidas resumen agrupadas por DBT (0/1)
data_clean %>% 
  group_by(DBT) %>% 
  summarise(mean = mean(Glucosa),
            median = median(Glucosa),
            mode = Mode(Glucosa),
            min=min(Glucosa),
            max=max(Glucosa),
            rango= max(Glucosa) - min(Glucosa),
            iqr= IQR(Glucosa),
            var=var(Glucosa),
            sd = sd(Glucosa),
            cv=(sd(Glucosa)/mean(Glucosa))*100)

# Impresion de cuartiles y deciles
result <- data_clean %>%
  group_by(DBT) %>% 
  summarise(quantiles = list(quantile(Glucosa))) %>%
  unnest_wider(quantiles)
print(result)

result <- data_clean %>%
  group_by(DBT) %>% 
  summarise(quantiles = list(quantile(Glucosa, probs = seq(0, 1, by = 0.1)))) %>%
  unnest_wider(quantiles)
print(result)

# Seteo mínimo y máximo para asegurarme escala fija en X
minimo = data_clean %>% 
  summarise(min(Glucosa)) %>% 
  pull()

maximo = data_clean %>% 
  summarise(max(Glucosa)) %>% 
  pull()

# Histograma con frecuencias absolutas
ggplot(data_clean, aes(x = Glucosa)) +
  geom_histogram( col='black', fill='lightgreen')+   #col= color linea borde de la barra #fill= color de la barra
  facet_wrap(~DBT) +
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="Glucosa [mg/dl]", limit = (c(minimo, maximo))) +
  ggtitle("Niveles de glucosa en mg/dl segun si tienen diabetes tipo II (1) o no (0) en frecuencia absoluta")

# Histograma con frecuencias relativas
ggplot(data_clean, aes(x = Glucosa, y = ..density..)) +
  geom_histogram(aes(y = ..density..), col='black', fill='lightgreen', binwidth = 5) +  
  facet_wrap(~DBT) +
  scale_y_continuous(name = "Frecuencia relativa") +
  scale_x_continuous(name = "Glucosa [mg/dl]") +
  ggtitle("Niveles de glucosa en mg/dl según si tienen diabetes tipo II (1) o no (0) en frecuencia relativa")

# Boxplot
boxplot(Glucosa~ DBT, data = data_clean,
        xlab="DBT", ylab="Glucosa [mg/dl]",
        main="Boxplot de niveles de Glucosa en mg/dl segun si tienen diabetes tipo II (1) o no (0)",
        col = c("#FFE0B2", "#FFA726"))


# ------------------------------------
#PAD vs DBT
data_clean <- data %>% filter(!is.na(PAD))

# Medidas resumen agrupadas por DBT (0/1)
data_clean %>% 
  group_by(DBT) %>% 
  summarise(mean = mean(PAD),
            median = median(PAD),
            mode = Mode(PAD),
            min=min(PAD),
            max=max(PAD),
            rango= max(PAD) - min(PAD),
            iqr= IQR(PAD),
            var=var(PAD),
            sd = sd(PAD),
            cv=(sd(PAD)/mean(PAD))*100)

# Impresion de cuartiles y deciles
result <- data_clean %>%
  group_by(DBT) %>% 
  summarise(quantiles = list(quantile(PAD))) %>%
  unnest_wider(quantiles)
print(result)

result <- data_clean %>%
  group_by(DBT) %>% 
  summarise(quantiles = list(quantile(PAD, probs = seq(0, 1, by = 0.1)))) %>%
  unnest_wider(quantiles)
print(result)

# Seteo mínimo y máximo para asegurarme escala fija en X
minimo = data_clean %>% 
  summarise(min(PAD)) %>% 
  pull()

maximo = data_clean %>% 
  summarise(max(PAD)) %>% 
  pull()

# Histograma con frecuencias absolutas
ggplot(data_clean, aes(x = PAD)) +
  geom_histogram( col='black', fill='lightgreen')+   #col= color linea borde de la barra #fill= color de la barra
  facet_wrap(~DBT) +
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="PAD [mmHg]", limit = (c(minimo, maximo))) +
  ggtitle("Niveles de PAD en mmHg segun si tienen diabetes tipo II (1) o no (0) en frecuencia absoluta")

# Histograma con frecuencias relativas
ggplot(data_clean, aes(x = PAD, y = ..density..)) +
  geom_histogram(aes(y = ..density..), col='black', fill='lightgreen', binwidth = 5) +  
  facet_wrap(~DBT) +
  scale_y_continuous(name = "Frecuencia relativa") +
  scale_x_continuous(name = "PAD [mmHg]") +
  ggtitle("Niveles de PAD en mmHg segun si tienen diabetes tipo II (1) o no (0) en frecuencia relativa")

# Boxplot
boxplot(PAD~ DBT, data = data_clean,
        xlab="DBT", ylab="PAD [mmHg]",
        main="Boxplot de niveles de PAD en mmHg segun si tienen diabetes tipo II (1) o no (0)",
        col = c("#FFE0B2", "#FFA726"))


# ------------------------------------
#BMI vs DBT
data_clean <- data %>% filter(!is.na(BMI))

# Medidas resumen agrupadas por DBT (0/1)
data_clean %>% 
  group_by(DBT) %>% 
  summarise(mean = mean(BMI),
            median = median(BMI),
            mode = Mode(BMI),
            min=min(BMI),
            max=max(BMI),
            rango= max(BMI) - min(BMI),
            iqr= IQR(BMI),
            var=var(BMI),
            sd = sd(BMI),
            cv=(sd(BMI)/mean(BMI))*100)

# Impresion de cuartiles y deciles
result <- data_clean %>%
  group_by(DBT) %>% 
  summarise(quantiles = list(quantile(BMI))) %>%
  unnest_wider(quantiles)
print(result)

result <- data_clean %>%
  group_by(DBT) %>% 
  summarise(quantiles = list(quantile(BMI, probs = seq(0, 1, by = 0.1)))) %>%
  unnest_wider(quantiles)
print(result)

# Seteo mínimo y máximo para asegurarme escala fija en X
minimo = data_clean %>% 
  summarise(min(BMI)) %>% 
  pull()

maximo = data_clean %>% 
  summarise(max(BMI)) %>% 
  pull()

# Histograma con frecuencias absolutas
ggplot(data_clean, aes(x = BMI)) +
  geom_histogram( col='black', fill='lightgreen')+   #col= color linea borde de la barra #fill= color de la barra
  facet_wrap(~DBT) +
  scale_y_continuous(name="Frecuencia absoluta") +
  scale_x_continuous(name="BMI [kg/m2]", limit = (c(minimo, maximo))) +
  ggtitle("Niveles de BMI en kg/m2 segun si tienen diabetes tipo II (1) o no (0) en frecuencia absoluta")

# Histograma con frecuencias relativas
ggplot(data_clean, aes(x = BMI, y = ..density..)) +
  geom_histogram(aes(y = ..density..), col='black', fill='lightgreen', binwidth = 5) +  
  facet_wrap(~DBT) +
  scale_y_continuous(name = "Frecuencia relativa") +
  scale_x_continuous(name = "BMI [kg/m2]") +
  ggtitle("Niveles de BMI en kg/m2 segun si tienen diabetes tipo II (1) o no (0) en frecuencia relativa")

# Boxplot
boxplot(BMI~ DBT, data = data_clean,
        xlab="DBT", ylab="BMI [kg/m2]",
        main="Boxplot de niveles de BMI en kg/m2 segun si tienen diabetes tipo II (1) o no (0)",
        col = c("#FFE0B2", "#FFA726"))


# ------------------------------------
#BMI_OMS vs DBT
data_clean <- data %>% filter(!is.na(BMI_OMS))

# Tablas de frecuencia
data_clean %>% 
  tabyl(BMI_OMS, DBT) %>% 
  adorn_totals(c ("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting( digits = 2) %>% 
  adorn_ns()

data_clean %>% 
  tabyl(BMI_OMS, DBT) %>% 
  adorn_totals(c ("row", "col")) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting( digits = 2) %>% 
  adorn_ns()

# Barras 100%
tabla=table(data_clean$DBT, data_clean$BMI_OMS)

barplot(prop.table(tabla, margin=2)*100,
        xlab = "BMI OMS", ylab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.7, cex.lab=0.7,cex.names =0.7,cex.main=0.8, 
        ylim=c(0,100),
        legend.text = rownames(tabla),
        args.legend = list(x = "topright"),
        main="Gráfico de barras al 100% de presencia de diabetes tipo II segun BMI_OMS",
        beside=FALSE)

# ------------------------------------
#Grosor Piel vs DBT

# Simplemente asignamos el dataset a data_clean para mantener el estandar
data_clean <- data

# Tablas de frecuencia
data_clean %>% 
  tabyl(Grosor_Piel, DBT) %>% 
  adorn_totals(c ("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting( digits = 2) %>% 
  adorn_ns()

data_clean %>% 
  tabyl(Grosor_Piel, DBT) %>% 
  adorn_totals(c ("row", "col")) %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting( digits = 2) %>% 
  adorn_ns()

# Barras 100%
tabla=table(data_clean$DBT, data_clean$Grosor_Piel)

barplot(prop.table(tabla, margin=2)*100,
        xlab = "Grosor de piel", ylab = "Frecuencia relativa (%)",
        col = c("indianred1", "dodgerblue"),
        cex.axis = 0.7, cex.lab=0.7,cex.names =0.7,cex.main=0.8, 
        ylim=c(0,100),
        legend.text = rownames(tabla),
        args.legend = list(x = "topright"),
        main="Gráfico de barras al 100% de presencia de diabetes tipo II segun grosor de piel",
        beside=FALSE)



# ------------------------------------
# ------------------------------------
## Pregunta 8 ##
# Glucosa vs BMI

#Limpio dataset
data_clean <- data %>% filter(!is.na(BMI) & !is.na(Glucosa))

# Gráfico de dispersión 
plot (data_clean$Glucosa, data_clean$BMI, , pch=20, col="lightblue", 
      main= "Diagrama de dispersión Glucosa vs BMI", 
      xlab="Glucosa [mg/dl]", ylab="BMI [kg/m2]")

