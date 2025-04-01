# Setup:
require(BiocManager)
require(SummarizedExperiment)
require(ggplot2)
require(reshape2)
require(kableExtra)

# Importamos los datos del archivo:
data_set <- read.csv(human_cachexia.csv)

# Vemos su estructura:
str(data_set)

# Eliminamos la primera columna del dataset (IDs de pacientes)
data_set <- data_set[,-1]

# Creamos un objeto de clase SummarizedExperiment con estos datos:
sum_exp <- SummarizedExperiment(assays = list(original=data_set))

# Vemos qué contiene ese objeto:
sum_exp

# Creamos una función propia que transforma cualquier vector e código binario:

to.binary <- function(vector, positive) {
  # "vector" es el vector
  # "positive" es el valor que se codifica como 1
  # cualquier otro valor que no sea el especificado en "positive" se codifica como 0
  x <- as.character(vector)
  for (i in 1:length(x)) {
    if (x[i] == as.character(positive)) {
      x[i] <- 1
    }
    else {
      x[i] <- 0
    }
  }
  x <- as.numeric(x)
  return(x)
}

# Guardamos los datos de expresión del único assay del dataset:
expr_data <- assays(sum_exp)[[1]] 

# Convertimos la variable `Muscle.loss` a código binario:
expr_data[,1] <- to.binary(expr_data[,1], "cachexic")

# Verificamos la nueva estuctura de la variable que acabos de codificar:
str(expr_data[,1])

# Guardamos el nuevo 'assay' en el objeto sum_exp:
assay(sum_exp, "numeric") <- expr_data

# Vemos que contiene ahora el objeto SummarizedExperiment:
sum_exp

# Creamos un assay con los coeficientes de relación:
correlaciones <- cor(assay(sum_exp, "numeric"))[1,-1]

# Lo guardamos en un dataframe para poder obtener el gráfico:
dataframe <- data.frame(names=names(correlaciones), values=unname(correlaciones))

# Generamos el gráfico de barras, destacando en azul los coeficientes > 0.4:
ggplot(dataframe, aes(x=reorder(names, values), y=values, fill = values > 0.4)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "", y = "Coeficiente")

