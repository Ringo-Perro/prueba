library(readxl)
library(dplyr)
library(caret)
Donor <- readxl::read_excel("/Users/asaavedra/Projects/R/modelamiento-predictivo/Donor.xls")

Donor
dim(Donor)

set.seed(12345)
# Se crean los índices de las observaciones de entrenamiento
datos <- dplyr::top_n(Donor, 3120)
datos_final <- dplyr::top_n(Donor, -2000)

dim(datos)
train <- caret::createDataPartition(y = datos$target_b, p = 0.6, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]

# Verificación de variable respuesta para train y test, que sean aproximadas
prop.table(table(datos_train$target_b))
dim(datos_train)
prop.table(table(datos_test$target_b))
dim(datos_test)

# Preprocesamiento de datos
any(is.na(Donor))

# Revisión de varianza
library(magrittr)
datos %>% caret::nearZeroVar(saveMetrics = TRUE)

# Modelo
modelo = glm(target_b~.,data=datos_train, family = binomial)
summary(modelo)


# nombre de los atributos
nombreVar <- names(datos_train)
nombreVar <- nombreVar[-1]
nombreVar

atrib.sig <- NULL
nombres <- NULL
for(i in seq_along(nombreVar)){
  formula <- reformulate(nombreVar[i], "target_b")
  modelo1 <- glm(formula,data=datos_train, family = binomial)
  atrib.sig <- rbind(atrib.sig,summary(modelo1)$coefficients[-1,3:4])
  nombres <- c(nombres,rownames(summary(modelo1)$coefficients)[-1])
  rownames(atrib.sig) <- nombres
}

# 1 indica significativo, 0 no significativo
atrib.sig <- cbind(atrib.sig,Significativo = atrib.sig[,2] <0.05)
atrib.sig
# ordenamos para una mejor visualizacion
o <- order(atrib.sig[,2])
atrib.sig[o,]
