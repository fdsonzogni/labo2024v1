# Grafico de la ganancia que visualiza el overfitting
# La idea es probar con distintos hiperparametros del arbol de decision
# y ver como se acercan o separan las curvas de ganancia
# MUY importante :  notar que Training = 50%  y  Testing = 50%

# Notar que la curva en training es siempre convexa
# mientras que la de testing puede tener concavidades

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("ggplot2")


# cambiar aqui los parametros
PARAM <- list()
PARAM$minsplit <- 250
PARAM$minbucket <- 25
PARAM$maxdepth <- 4

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
       if (!is.na(seed)) set.seed(seed)

       bloque <- unlist(mapply(
              function(x, y) {
                     rep(y, x)
              },
              division, seq(from = start, length.out = length(division))
       ))

       data[, (campo) := sample(rep(
              bloque,
              ceiling(.N / length(bloque))
       ))[1:.N],
       by = agrupa
       ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

#cargo MI amada primera semilla, que esta en MI bucket
tabla_semillas <- fread( "./datasets//mis_semillas.txt" )
ksemilla_azar <- tabla_semillas[ 1, semilla ]  # 1 es mi primera semilla

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# a partir de ahora solo trabajo con 202107, el mes que tiene clase
dataset <- dataset[foto_mes == 202107] # defino donde voy a entrenar


# La division training/testing es 50%, 50%
#  que sea 50/50 se indica con el c(1,1)
particionar(dataset,
       division = c(1, 1),
       agrupa = "clase_ternaria", seed = ksemilla_azar
)

# Entreno el modelo
# los datos donde voy a entrenar
# aqui es donde se deben probar distintos hiperparametros
modelo <- rpart(
       formula = "clase_ternaria ~ . -fold",
       data = dataset[fold == 1, ],
       xval = 0,
       cp = -1,
       minsplit = PARAM$minsplit,
       minbucket = PARAM$minbucket,
       maxdepth = PARAM$maxdepth
)

png("./plots/arbol_decision.png", width = 2000, height = 1500, res = 300)

# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)

# Cierra el dispositivo gráfico, lo que guarda el archivo
dev.off()


# aplico el modelo a TODOS los datos, inclusive los de training
prediccion <- predict(modelo, dataset, type = "prob")

# Pego la probabilidad de  BAJA+2
dataset[, prob_baja2 := prediccion[, "BAJA+2"]]


# Dibujo la curva de ganancia acumulada
setorder(dataset, fold, -prob_baja2)

# agrego una columna que es la de las ganancias
# la multiplico por 2 para que ya este normalizada
#  es 2 porque cada fold es el 50%
dataset[, gan := 2 * ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]
dataset[, ganancia_acumulada := cumsum(gan), by = fold]
dataset[, pos := sequence(.N), by = fold] 
# Esta hermosa curva muestra como en el mentiroso training
#   la ganancia es siempre mejor que en el real testing
# segundo grafico solo los primeros 20k enviso
# Calculamos los límites y breaks para el eje Y

# Asumimos que los datos están en una escala de millones para empezar

breaks_y <- seq(from = 0, to = 75000000, by = 5000000)

# Define los títulos de los ejes
titulo_x <- "Posición"
titulo_y <- "Ganancia Acumulada (en Millones)"
# Crear una nueva columna con 'Train' o 'Test'
dataset$ColorGroup <- ifelse(dataset$fold == 1, "Train", "Test")

gra <- ggplot(
           data = dataset[pos <= 20000],
           aes( x = pos, y = ganancia_acumulada,
                color = ColorGroup)) +
                geom_line() + 
             scale_y_continuous(breaks = breaks_y, labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 1e-3)) +
  theme(legend.position = "bottom") +
  xlab(titulo_x) +  # Agrega el título al eje X
  ylab(titulo_y) +   # Agrega el título al eje Y
  # Título eje Y
  annotate("text", label = "EXPERIMENTO\nGrid Search con mejores hiperparámetros:\nMD: 16 - MS: 250 - MB: 25\nGananacia Entrenamiento: 79.704 M\nGanancia Testeo: 45.252 M", x = 7500, y = 25000000, size = 4, color = "blue") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red"),
                     labels = c("Set de entrenamiento", "Set de Testeo"))  # Define manualmente los colores y etiquetas

print( gra )

dir.create("./plots/", showWarnings = FALSE)
ggsave("./plots/Z365_graf_GS_MD16_MS250_MB25.png", gra, width = 10, height = 6, dpi = 300, units = "in")

cat( "Train gan max: ", dataset[fold == 1, max(ganancia_acumulada)], "\n" )
cat( "Test  gan max: ", dataset[fold == 2, max(ganancia_acumulada)], "\n" )
