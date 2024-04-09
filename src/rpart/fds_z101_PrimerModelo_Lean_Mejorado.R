# Arbol elemental con libreria rpart
# Debe tener instaladas las librerias data.table, rpart y rpart.plot

# Carga las librerías necesarias
require("data.table")
require("rpart")
require("rpart.plot")

# Parametriza el directorio de trabajo, el directorio de exportación, prefijo de archivo y el contador inicial
#setwd("~/buckets/b1/")
working_directory <- "~/buckets/b1/"
export_directory <- "./exp/HT3330"
prefijo_archivo <- "K3330_"
inicio_contador <- 1  # Asegúrate de que este sea el número inicial correcto

# Establezco el Working Directory
setwd(working_directory)

# Carga el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# Define dónde voy a entrenar y aplicar el modelo
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# Define las corridas específicas basadas en resultados anteriores
resultados_corridas <- list(
  list(posicion = 10, cp = -0.62, maxdepth = 3, minsplit = 2932, minbucket = 1415, ganancia_promedio = 999),
  list(posicion = 50, cp = -0.03, maxdepth = 4, minsplit = 7409, minbucket = 2352, ganancia_promedio = 999)
)

# Asegura que el directorio de exportación exista
output_path <- file.path(getwd(), export_directory)
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Itera sobre las corridas específicas
for(i in seq_along(resultados_corridas)) {
  corrida <- resultados_corridas[[i]]
  
  # Entrena el modelo con los parámetros de la corrida actual
  modelo <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    xval = 0,
    cp = corrida$cp,
    minsplit = corrida$minsplit,
    minbucket = corrida$minbucket,
    maxdepth = corrida$maxdepth
  )
  
  # Aplica el modelo a los datos nuevos
  prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
  )
  
  # Agrega a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
  
  # Solo envía estímulo a los registros con probabilidad de BAJA+2 mayor a 1/40
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  # Genera el nombre del archivo, incrementando el contador y manteniendo el formato
  file_counter <- sprintf("%03d", inicio_contador)
  file_name <- sprintf("%s/%s%s-cp_%s-md_%d-ms_%d-mb_%d.csv", 
                       export_directory, 
                       prefijo_archivo, 
                       file_counter, 
                       corrida$cp, 
                       corrida$maxdepth, 
                       corrida$minsplit, 
                       corrida$minbucket)
  fwrite(dapply[, .(numero_de_cliente, Predicted)], file = file_name, sep = ",")

  # Imprime un mensaje informativo sobre la entrega
  print(paste("Entrega Kaggle:", file_name, 
              "- cp:", corrida$cp, 
              "maxdepth:", corrida$maxdepth, 
              "minsplit:", corrida$minsplit, 
              "minbucket:", corrida$minbucket, 
              "ganancia_promedio:", corrida$ganancia_promedio))
  
  inicio_contador <- inicio_contador + 1  # Incrementa el contador para el próximo archivo
}

# Asegúrate de que los directorios existen antes de correr este script o incluye la creación dentro del script.

