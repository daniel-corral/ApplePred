---
  title: "Predicción ventas Apple"
author: "Daniel Corral Ruiz"
date: "16-11-2020"
output:
  pdf_document:
  toc: yes
toc_depth: '5'
---
  
  ```{r echo=FALSE,warning= FALSE, message=FALSE}
library(here) # Comentar
library(tidyverse)
library(janitor) # Clean names
library(graphics) # Beautiful Summarize
library(magrittr) # Pipe operators
library(corrplot) # Correlations
library(ggcorrplot)  # Correlations
library(PerformanceAnalytics) # Correlations
library(leaps) # Model selection
library(gplots)
library(MASS)
library(dplyr)
library(readr)
library(gvlma)
library(MASS)
library(car)
library(glmnet)
library(boot)
library(leaps)
library(rsample)
library(factoextra)
library(FactoMineR)
library(gam)
library(corrplot)
```

## Introducción
El objetivo es predecir las ventas de Apple. Para ello, hemos acudido a Bloomberg y hemos obtenido los datos trimestrales desde el 2T de 2008 hasta el 3T del 2017. ```{r warning= FALSE, message=FALSE}
partidos <- read.csv("AFC-votos.csv", sep = ";")
rownames(partidos) <- partidos$X
partidos <- partidos[,-1]
tabla <- as.table(as.matrix(partidos))
tabla
```

A través de los resultados obtenidos en la tabla, podemos añadir un breve análisis exploratorio de datos. Observamos como tiene gran importancia en el grupo parlamentario UP los Parados asi como encontramos un gran número de jubilados en el PP.

Podemos representar gráficamente mediante el siguiente gráfico. 

```{r warning= FALSE, message=FALSE}
balloonplot(t(tabla), main ="Distribución de votos", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE, dotcol = "orange")
```

## Contraste de independencia Chi cuadrado
Contraste de independencia Chi cuadrado
Los test de asociación o independencia Chi-cuadrado sirven para determinar
si existe una relación entre dos o más variables. Para evaluar si dos 
variables categóricas están asociadas (relacionadas) es necesario comprobar 
si la distribución de los valores de una variable difiere en función de los
valores de la otra. Para ello, debemos partir de las siguientes hipótesis estadísticas:
  
  - Hipótesis nula, Ho: No existe relación entre las variables (los resultados de  las categorías de una variable no se ven afectados o influenciados por las categorías de la segunda variable).

- Hipótesis alterna, Ha: Existe asociación o relación entre las variables.

```{r warning= FALSE, message=FALSE}
chisq=chisq.test(partidos)
chisq
```

Observand los resultados, con un pvalor de 0, rechazamos la hipotesis nula y podemos confirmar la existencia de algun tipo de relacion entre filas y columnas.

## Análisis de correspondencias
Una vez que hemos observado la existencia de relacion entre categorías, el análisis de correspondencias nos permitirá identificar cuales son, de una manera sencilla en un espacio de n dimensiones (valor "ncp").

```{r warning= FALSE, message=FALSE}
CA(partidos, graph = TRUE)
partidos.afc=CA(partidos, graph=FALSE)
print(partidos.afc)
summary(partidos.afc, nb.dec = 2, ncp = 2)
```

Observamos el resumen para las dos primeras dimensiones:
  
  - En la primera parte del summary podemos ver los autovalores (eigenvalues),
varianzas y porcentajes de varianza explicada por cada una de las dimensiones.
- La segunda tabla muestra la calidad de la representacion de filas.
- La tercera tabla muestra la calidad de la representacion de columnas.

```{r warning= FALSE, message=FALSE}
summary(partidos.afc, nb.dec = 2, ncp = 3)
```

En el caso de tres dimensiones vemos una baja calidad de los valores tanto de filas como de columnas. Por lo que a nuestro criterio preferimos quedarnos con un total de dos dimensiones.

## Interpretación del análisis de correspondencias
### Nivel de asociación entre filas y columnas
Para conocer si se da o no una asociación significativa entre filas y columnas empleamos los metodos de la traza y el chi-cuadrado.

#### Traza
A modo de resumen, se puede interpretar como coeficiente de correlación entre filas y columnas. Se calcula:
  
  ```{r warning= FALSE, message=FALSE}
autov = get_eigenvalue(partidos.afc)
traza = sum(autov[,1]) 
cor.coef = sqrt(traza)
cor.coef
```
Teniendo en cuenta que a partir de un nivel de 0.2 la correlación puede cosiderarse como importante, nuestro valor 0.3831 puede decirse que se trata de una relacion fuerte.

#### Chi-Cuadrado
El estadístico chi-cuadrado visto anteriormente nos lleva a rechazar la hipótesis de independencia de filas y columnas, permitiendo continuar con el análisis.

```{r warning= FALSE, message=FALSE}
chisq=chisq.test(partidos)
chisq
```

## Autovalores y gráfico de sedimentación
Para saber con cuantas dimensiones vamos a trabajar debemos de hacer un exámen de autovalores. Podemos calcular la proporción de varianza explicada por las distintas dimensiones:
  
  ```{r warning= FALSE, message=FALSE}
autoval = get_eigenvalue(partidos.afc)
head(round(autoval, 2))
```

Podemos observar un total de 4 dimensiones, las cuales nos permiten explicar el 100% de la varianza. En nuestro análisis vamos a escoger un total de dos dimensiones, las cuales explican  aproximadamente el 90% de la varianza. 
Una alternativa es realizar un gráfico de sedimentación:
  
  ```{r warning= FALSE, message=FALSE}
fviz_screeplot(partidos.afc, addlabels = TRUE, barfill = "orange", ggtheme = theme_dark())+
  ggtitle("Gráfico de sedimentación") +
  labs(x="Dimensiones",y="Porcentaje de varianza explicada")
```

Observamos las 4 posibles dimensiones y la varianza que explica cada una de las dimensiones. 

## Gráfico de dispersión del análisis de correspondencias entre filas y columnas.
Representamos las coordenadas de filas y columnas en el espacio de dos dimensiones.

```{r warning= FALSE, message=FALSE}
fviz_ca_biplot(partidos.afc)+
  theme_dark()+
  ggtitle("Mapa 2D. Análisis de correspondencias simples")
```