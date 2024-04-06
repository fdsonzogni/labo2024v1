# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.9, # esto significa no limitar la complejidad de los splits
        minsplit = 800, # minima cantidad de registros para que se haga el split
        minbucket = 240, # tamaño minimo de una hoja
        maxdepth = 14
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
    extra = 106,  # Muestra solo los porcentajes de clasificación correcta por nodo
    cex = 0.6,    # Ajusta el tamaño del texto globalmente
    box.palette = "RdBu",  # Usa una paleta de colores para diferenciar los nodos
    branch = 0.8,  # Ajusta el grosor de las líneas de las ramas
    type = 4,  # Mantiene el diseño del árbol
    varlen = 10,  # Limita la longitud de las etiquetas de las variables a 10 caracteres
    faclen = 10   # Limita la longitud de las etiquetas de los factores a 10 caracteres
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_clase3_001.csv",
        sep = ","
)
