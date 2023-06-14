Analisis espacial Boston Housing
================
Isaac Kelly Ramirez / a00829261
2023-05-12

# Analisis espacial Boston Housing

## Informacion acerca del ESDA

### 1.-

- autocorrelación espacial: indica la similitud o la relación espacial
  entre los valores de una variable en diferentes ubicaciones
  geográficas.

- autocorrelación espacial positiva: significa que los valores de una
  variable están correlacionados positivamente con los valores de la
  misma variable en las ubicaciones geográficas cercanas.

- autocorrelación espacial negativa: significa que los valores de una
  variable están correlacionados negativamente con los valores de la
  misma variable en las ubicaciones geográficas cercanas.

### 2.- Diferencias entre estacionareidad y no estacionareidad en contexto espacial

- presencia de estacionareidad en contexto espacial: La estacionariedad
  espacial se refiere a la NO VARIACION de la autocorrelación espacial a
  lo largo del espacio. Esto significa que la autocorrelación espacial
  NO ES diferente en diferentes áreas geográficas. esto quiere decir que
  la media y mediana de la variable NO cambian de acuerdo con el
  contexto geografico o ubicacion

- no estacionareidad en contexto espacial: La no estacionariedad
  espacial se refiere a la variación de la autocorrelación espacial a lo
  largo del espacio. Esto significa que la autocorrelación espacial
  puede ser diferente en diferentes áreas geográficas. esto quiere decir
  que la media y mediana de la variable cambian de acuerdo con el
  contexto geografico o ubicacion

### 3.- Diferencias entre el Analisis exploratorio espacial de datos (ESDA) y analisis exploratorio de datos (EDA)

- Tipo de datos necesarios:EDA se aplica a datos en general, mientras
  que ESDA requiere datos con una estructura espacial con una ubicación
  geográfica relacionada.

- El objetivo principal del EDA es descubrir patrones y relaciones
  interesantes entre las variables de los datos utilizando metodos
  estadisticos y visualizaciones graficas. mientras que el objetivo
  principal de ESDA es analizar y visualizar patrones espaciales y
  relaciones entre los datos relacionados geográficamente.

- EDA se enfoca en técnicas como histogramas, gráficos de dispersión,
  boxplots, entre otros.

- ESDA se enfoca en técnicas como gráficos de dispersión espacial, mapas
  de calor, índices de autocorrelación espacial, entre otros.

- En el EDA, la interpretación de los resultados se basa en la
  exploración visual de los gráficos y resúmenes estadísticos.

- En el ESDA, la interpretación de los resultados se basa en la
  identificación de patrones, relaciones espaciales y en la
  interpretación de los índices de autocorrelación espacial.

### 4.- consecuencias de identificar autocorrelación espacial en los residuales de un modelo de regresión estimado

- La autocorrelación espacial en los residuales puede hacer que los
  coeficientes de regresión estimados sean sesgados e imprecisos debido
  a que los coeficientes de regresión capturan tanto la influencia de
  las variables explicativas como la influencia de los factores
  espaciales no observados.

- tambien puede afectar la precisión de las predicciones realizadas,
  como la significancia de los coeficientes de regresión y los
  intervalos de confianza. La autocorrelación espacial puede hacer que
  la varianza de los coeficientes de regresión estimados sea
  subestimada, lo que puede llevar a conclusiones erróneas sobre la
  significancia de las variables explicativas.

- La autocorrelación espacial en los residuales puede hacer que la
  interpretación de los resultados del modelo sea sesgada ya que la
  autocorrelación espacial puede hacer que la influencia de los factores
  espaciales no observados se atribuya incorrectamente a las variables.

### 5.- Como puede el proceso de análisis espacial de datos mejorar las herramientas de Descriptive Analytics y Predictive Analytics en un contexto de Inteligencia de Negocios.

El añadir a las variables un contexto geografico o una referencia
geografica puede ayudar tanto a los modelos predictivos y analiticos a
proporcionar predicciones mas precisas al encontrar relacion geografica
entre los modelos, ya que los modelos convencionales carecen de la
capacidad para analizar esta relacion geografica, el preoceso de
analisis espacial tambien puede ayudar a identificar tendencias
temporales y espaciales en los datos. Por ejemplo, los datos de ventas
pueden ser analizados espacialmente a lo largo del tiempo para
identificar patrones de crecimiento o declive en áreas geográficas
específicas, puede tambien ayudar a optimizar la ubicación de recursos,
como tiendas o instalaciones, en función de la ubicación de los clientes
y otros factores espaciales relevantes.

En el contexto de inteligencia de negocios puede ayudarnos a entender
mejor como se comporta nuestro entorno, que areas tienen una mayor
densidad de clientes o crecimiento economico, en que areas se agrupa
nuestro cliente optimo o que fenomenos geograficos como la presencia de
vialidades o crimen nos pueden afectar en nuestras operaciones
empresariales.

# Segunda parte

``` r
library(sf)
library(tmap)
library(spdep)
library(rgdal)
library(tidyverse)
library(tigris)
library(mapview)
library(GWmodel)    
library(regclass)
library(viridis)
library(grid)
library(RColorBrewer)
library(rgeoda)
library(sjPlot)
library(jtools)
library(dlookr)
library(SpatialML)
library(spgwr)
library(grid)
library(corrplot)
library(maptools)
library(ncf)
```

base de datos a utilizar

``` r
###
library(maptools)
data(columbus) ### dataset
columbus_shp <- readShapePoly(system.file("etc/shapes/columbus.shp",package="spdep"))
col.gal.nb <- read.gal(system.file("etc/weights/columbus.gal", package="spdep"))
columbus[, c(15,22)] <- NULL
#View(columbus)
```

## estadisticas descriptivas de la base de datos

``` r
summary(columbus) 
```

    ##       AREA           PERIMETER        COLUMBUS.    COLUMBUS.I     POLYID  
    ##  Min.   :0.03438   Min.   :0.9021   Min.   : 2   Min.   : 1   Min.   : 1  
    ##  1st Qu.:0.09315   1st Qu.:1.4023   1st Qu.:14   1st Qu.:13   1st Qu.:13  
    ##  Median :0.17477   Median :1.8410   Median :26   Median :25   Median :25  
    ##  Mean   :0.18649   Mean   :1.8887   Mean   :26   Mean   :25   Mean   :25  
    ##  3rd Qu.:0.24669   3rd Qu.:2.1992   3rd Qu.:38   3rd Qu.:37   3rd Qu.:37  
    ##  Max.   :0.69926   Max.   :5.0775   Max.   :50   Max.   :49   Max.   :49  
    ##       NEIG        HOVAL            INC             CRIME        
    ##  Min.   : 1   Min.   :17.90   Min.   : 4.477   Min.   : 0.1783  
    ##  1st Qu.:13   1st Qu.:25.70   1st Qu.: 9.963   1st Qu.:20.0485  
    ##  Median :25   Median :33.50   Median :13.380   Median :34.0008  
    ##  Mean   :25   Mean   :38.44   Mean   :14.375   Mean   :35.1288  
    ##  3rd Qu.:37   3rd Qu.:43.30   3rd Qu.:18.324   3rd Qu.:48.5855  
    ##  Max.   :49   Max.   :96.40   Max.   :31.070   Max.   :68.8920  
    ##       OPEN             PLUMB             DISCBD            X        
    ##  Min.   : 0.0000   Min.   : 0.1327   Min.   :0.370   Min.   :24.25  
    ##  1st Qu.: 0.2598   1st Qu.: 0.3323   1st Qu.:1.700   1st Qu.:36.15  
    ##  Median : 1.0061   Median : 1.0239   Median :2.670   Median :39.61  
    ##  Mean   : 2.7709   Mean   : 2.3639   Mean   :2.852   Mean   :39.46  
    ##  3rd Qu.: 3.9364   3rd Qu.: 2.5343   3rd Qu.:3.890   3rd Qu.:43.44  
    ##  Max.   :24.9981   Max.   :18.8111   Max.   :5.570   Max.   :51.24  
    ##        Y              NSA              NSB               EW        
    ##  Min.   :24.96   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:28.26   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :31.91   Median :0.0000   Median :1.0000   Median :1.0000  
    ##  Mean   :32.37   Mean   :0.4898   Mean   :0.5102   Mean   :0.5918  
    ##  3rd Qu.:35.92   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :44.07   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##        CP             THOUS          NEIGNO    
    ##  Min.   :0.0000   Min.   :1000   Min.   :1001  
    ##  1st Qu.:0.0000   1st Qu.:1000   1st Qu.:1013  
    ##  Median :0.0000   Median :1000   Median :1025  
    ##  Mean   :0.4898   Mean   :1000   Mean   :1025  
    ##  3rd Qu.:1.0000   3rd Qu.:1000   3rd Qu.:1037  
    ##  Max.   :1.0000   Max.   :1000   Max.   :1049

## Pruebas de normalidad

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

Se hara una transformacion a la variable dependiente HOVAL para
normalizarla, al igual que plumb y area, ya que presentan un mayor sesgo

``` r
columbus$HOVAL = log(columbus$HOVAL)
columbus$PLUMB = log(columbus$PLUMB)
columbus$AREA = log(columbus$AREA)
```

## Boxplots

``` r
cute_boxplots <- function(data) {
  for (col in colnames(data)) {
    plot_data <- data.frame(group = col, value = data[[col]])
    p <- ggplot(plot_data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.4, color = "black") +
      geom_jitter(width = 0.2, size = 2, alpha = 0.8) +
      scale_fill_manual(values = c("#F8766D", "#00BFC4", "#7CAE00")) +
      labs(x = "Group", y = col) +
      theme_classic()
    print(p)
  }
}

cute_boxplots(columbus)
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-11.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-12.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-13.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-14.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-15.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-16.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-17.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-18.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-19.png)<!-- -->![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-6-20.png)<!-- -->

## Matriz de correlación:

``` r
cor_matrix <- cor(columbus)
corrplot(cor_matrix, method = "number")
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

De acuerdo con nuestra Matriz de correlacion la variable dependiente
HOVAL se encuentra principalmente relacionada con las variables: \*
crimen: con una relacion negativa de -.57 \* inc: con una relacion
negativa de -7 \* discbd: con una relacion positiva de .49 \* CP: con
una relacion negativa de -.49

# Análisis Exploratorio Espacial de los Datos (ESDA)

## Autocorrelacion espacial global en variables de interes

### Metodo Queen

``` r
swm_queen <- poly2nb(columbus_shp, queen = TRUE)
```

### Metodo Rook

``` r
swm_rook <- poly2nb(columbus_shp, queen = FALSE)
```

``` r
rswm_queen <- nb2listw(swm_queen, style = "W", zero.policy = TRUE)
rswm_rook  <- nb2listw(swm_rook, style = "W", zero.policy = TRUE)
```

### Global Moran test

``` r
moran.test(columbus$HOVAL, listw = rswm_queen, zero.policy = TRUE, na.action = na.omit)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  columbus$HOVAL  
    ## weights: rswm_queen    
    ## 
    ## Moran I statistic standard deviate = 2.9453, p-value = 0.001613
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.252777699      -0.020833333       0.008629742

Podemos encontrar auto correlacion global con la variable de Hoval el p
value es menor a .05 por lo tanto se rechaza la hipotesis nula y se
acepta la hipotesis alternativa de que existe autocorrelacion espacial

``` r
moran.test(columbus$CRIME, listw = rswm_queen, zero.policy = TRUE, na.action = na.omit)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  columbus$CRIME  
    ## weights: rswm_queen    
    ## 
    ## Moran I statistic standard deviate = 5.5894, p-value = 1.139e-08
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.500188557      -0.020833333       0.008689289

Podemos encontrar auto correlacion global con la variable de CRIME el p
value es menor a .05 por lo tanto se rechaza la hipotesis nula y se
acepta la hipotesis alternativa de que existe autocorrelacion espacial

``` r
moran.test(columbus$PLUMB, listw = rswm_queen, zero.policy = TRUE, na.action = na.omit)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  columbus$PLUMB  
    ## weights: rswm_queen    
    ## 
    ## Moran I statistic standard deviate = 7.5613, p-value = 1.996e-14
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.681818585      -0.020833333       0.008635588

Podemos encontrar que no hay auto correlacion global con la variable de
PLUMB el p value es mayor a .05 por lo tanto se acepta la hipotesis nula

``` r
moran.test(columbus$DISCBD, listw = rswm_queen, zero.policy = TRUE, na.action = na.omit)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  columbus$DISCBD  
    ## weights: rswm_queen    
    ## 
    ## Moran I statistic standard deviate = 8.7904, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.800562617      -0.020833333       0.008731396

Podemos encontrar auto correlacion global con la variable de DISCBD el p
value es menor a .05 por lo tanto se rechaza la hipotesis nula y se
acepta la hipotesis alternativa de que existe autocorrelacion espacial

``` r
moran.test(columbus$INC, listw = rswm_queen, zero.policy = TRUE, na.action = na.omit)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  columbus$INC  
    ## weights: rswm_queen    
    ## 
    ## Moran I statistic standard deviate = 4.7645, p-value = 9.467e-07
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.415628778      -0.020833333       0.008391926

Podemos encontrar auto correlacion global con la variable de INC el p
value es menor a .05 por lo tanto se rechaza la hipotesis nula y se
acepta la hipotesis alternativa de que existe autocorrelacion espacial

## Autocorrelación espacial LOCAL en las variables de interes

``` r
lisa(columbus_shp$X, columbus_shp$Y, columbus_shp$HOVAL, 100, resamp = 999, latlon = FALSE, quiet = FALSE)
```

    ## 100  of  999 200  of  999 300  of  999 400  of  999 500  of  999 600  of  999 700  of  999 800  of  999 900  of  999 

    ## $correlation
    ##  [1] -1.101792e-01 -2.344205e-03 -9.110579e-03 -1.710019e-03 -1.443089e-02
    ##  [6] -5.851588e-03 -8.338093e-02 -1.072306e-04 -1.251188e-02 -2.095454e-01
    ## [11] -2.189419e-02 -2.142926e-02 -6.643631e-04 -1.242710e-03 -2.604750e-02
    ## [16] -2.404810e-02 -6.848743e-04 -2.900109e-02 -3.829820e-03 -1.144133e-01
    ## [21] -2.125620e-02 -3.977843e-03 -5.390506e-03 -1.359439e-02 -2.630304e-02
    ## [26] -2.051438e-02 -1.172704e-03 -1.515118e-02 -2.197786e-03 -1.583928e-02
    ## [31] -2.746673e-03 -2.166464e-04 -1.372813e-02 -6.219668e-03 -8.156992e-03
    ## [36] -2.846157e-04 -1.475406e-03 -1.544421e-02 -8.446983e-05 -3.448335e-02
    ## [41] -8.371859e-04 -2.168672e-03 -1.011687e-02 -1.519688e-03 -7.144864e-03
    ## [46] -8.847332e-02 -1.029969e-03 -8.444791e-03 -4.334408e-04
    ## 
    ## $p
    ##  [1] 0.065 0.332 0.462 0.288 0.371 0.428 0.098 0.023 0.440 0.018 0.228 0.257
    ## [13] 0.095 0.194 0.185 0.220 0.132 0.139 0.383 0.043 0.262 0.356 0.393 0.404
    ## [25] 0.164 0.290 0.188 0.342 0.309 0.278 0.348 0.045 0.400 0.453 0.480 0.061
    ## [37] 0.219 0.311 0.001 0.109 0.135 0.288 0.441 0.276 0.478 0.085 0.165 0.474
    ## [49] 0.083
    ## 
    ## $mean
    ##  [1] 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622
    ##  [9] 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622
    ## [17] 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622
    ## [25] 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622
    ## [33] 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622
    ## [41] 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622 38.43622
    ## [49] 38.43622
    ## 
    ## $dmean
    ##  [1] 13.370540 12.481334 10.969792 10.796221  8.837821 10.543824 10.852500
    ##  [8]  9.535692  8.686217 11.137335  8.040996  8.106966  8.598071  8.412031
    ## [15]  7.486869  7.482920 11.217379  7.901361  8.167058  9.666498  9.187850
    ## [22]  7.996200 11.771124  7.744453  7.175989  7.321964  8.152572  7.437981
    ## [29]  7.374207  7.600013 13.402001 12.353927  8.376570 12.046552  8.546218
    ## [36] 14.274825  8.131718  8.017208 17.291509 11.666823 13.839096 13.566306
    ## [43]  8.632369  9.204501  9.646364 16.622260 14.686620  9.616005 10.543328
    ## 
    ## $n
    ##  [1] 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
    ## [26] 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
    ## 
    ## $z
    ##  [1] 80.467 44.567 26.350 33.200 23.225 28.750 75.000 37.125 52.600 96.400
    ## [11] 19.700 19.900 41.700 42.900 18.000 18.800 41.750 60.000 30.600 81.267
    ## [21] 19.975 30.450 47.733 53.200 17.900 20.300 34.100 22.850 32.500 22.500
    ## [31] 31.800 40.300 23.600 28.450 27.000 36.300 43.300 22.700 39.600 61.950
    ## [41] 42.100 44.333 25.700 33.500 27.733 76.100 42.500 26.800 35.800
    ## 
    ## $coord
    ## $coord$x
    ##  [1] 38.80 35.62 39.82 36.50 40.01 43.75 33.36 36.71 43.44 47.61 37.85 37.13
    ## [13] 35.95 35.72 39.61 37.60 48.58 36.15 35.76 46.73 34.08 43.37 49.61 36.60
    ## [25] 39.36 41.13 43.95 41.31 39.72 38.29 27.94 50.11 44.10 30.32 43.70 27.27
    ## [37] 38.32 41.04 24.25 48.44 51.24 29.02 41.09 43.23 39.32 25.47 50.89 41.21
    ## [49] 42.67
    ## 
    ## $coord$y
    ##  [1] 44.07 42.38 41.18 40.52 38.00 39.28 38.41 38.71 35.92 36.42 36.30 36.12
    ## [13] 36.40 35.60 34.91 34.08 34.46 33.92 34.66 31.91 30.42 33.46 32.65 32.09
    ## [25] 32.88 33.14 31.61 30.90 30.64 30.35 29.85 29.91 30.40 28.26 29.18 28.21
    ## [37] 28.82 28.78 26.69 27.93 27.80 26.58 27.49 27.31 25.85 25.71 25.24 25.90
    ## [49] 24.96
    ## 
    ## 
    ## $call
    ## [1] "lisa(x = columbus_shp$X, y = columbus_shp$Y, z = columbus_shp$HOVAL, "
    ## [2] "    neigh = 100, resamp = 999, latlon = FALSE, quiet = FALSE)"        
    ## 
    ## attr(,"class")
    ## [1] "lisa"

En este caso dada la funcion LISA y los valores P arrojados podriamos
ver que solo en la minoria de casos existe autocorrelacion LOCAL
significativa.

``` r
columbus_shp$sp_CRIME<-lag.listw(rswm_queen,columbus_shp$CRIME,zero.policy=TRUE) 
qtm(columbus_shp, "sp_CRIME")
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Podemos ver que existe en la ciudad una mayor concentracion de crimen en
las areas cercanas al centro de la ciudad

``` r
columbus_shp$sp_HOVAL<-lag.listw(rswm_queen,columbus_shp$HOVAL,zero.policy=TRUE) 
qtm(columbus_shp, "sp_HOVAL")
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

De acuerdo con este mapa aquellas casas ubicadas en el centro de la
ciudad tienden a tener un menor valor que aquellas a las afueras.

``` r
columbus_shp$sp_PLUMB<-lag.listw(rswm_queen,columbus_shp$PLUMB,zero.policy=TRUE) 
qtm(columbus_shp, "PLUMB")
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

De acuerdo con este mapa las casas en el centro de la ciudad tienden a
tener mejor acceso a servicios de drenaje que aquellas a las afueras,
sin embargo las casas en las afueras tienen un mayor valor.

``` r
columbus_shp$sp_OPEN<-lag.listw(rswm_queen,columbus_shp$OPEN,zero.policy=TRUE) 
qtm(columbus_shp, "OPEN")
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
De igual forma el espacio abierto en las colonias tiende a ser mayor a
las afueras de la ciudad que en el centro, por lo tanto puede existir
una correlacion con el precio.

Podemos observar que las casas en el centro de la ciudad tienden a tener
mas acceso a servicios de plomeria que aquellas a las afueras

## Modelos de prediccion

## Spatial Autoregressive Model (SAR)

``` r
spatial_autoregressive = lagsarlm(log(HOVAL) ~ PLUMB+OPEN+CRIME, data = columbus_shp, rswm_queen, Durbin = FALSE)
summary(spatial_autoregressive)
```

    ## 
    ## Call:
    ## lagsarlm(formula = log(HOVAL) ~ PLUMB + OPEN + CRIME, data = columbus_shp, 
    ##     listw = rswm_queen, Durbin = FALSE)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.4089284 -0.2289422  0.0041198  0.1145555  0.9709689 
    ## 
    ## Type: lag 
    ## Coefficients: (asymptotic standard errors) 
    ##               Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept)  2.9498188  0.5556923  5.3084 1.106e-07
    ## PLUMB        0.0302238  0.0123617  2.4450   0.01449
    ## OPEN         0.0157815  0.0092656  1.7032   0.08852
    ## CRIME       -0.0170972  0.0030058 -5.6881 1.285e-08
    ## 
    ## Rho: 0.30635, LR test value: 3.5997, p-value: 0.057789
    ## Asymptotic standard error: 0.14256
    ##     z-value: 2.149, p-value: 0.031634
    ## Wald statistic: 4.6182, p-value: 0.031634
    ## 
    ## Log likelihood: -9.321417 for lag model
    ## ML residual variance (sigma squared): 0.083764, (sigma: 0.28942)
    ## Number of observations: 49 
    ## Number of parameters estimated: 6 
    ## AIC: 30.643, (AIC for lm: 32.243)
    ## LM test for residual autocorrelation
    ## test value: 0.21388, p-value: 0.64374

### spatial durbin model

``` r
spatial_durbin = lagsarlm(log(HOVAL) ~ OPEN + CRIME + PLUMB + PERIMETER+AREA, data = columbus_shp, rswm_queen)
summary(spatial_durbin)
```

    ## 
    ## Call:lagsarlm(formula = log(HOVAL) ~ OPEN + CRIME + PLUMB + PERIMETER + 
    ##     AREA, data = columbus_shp, listw = rswm_queen)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.428799 -0.209677 -0.026141  0.141890  0.914670 
    ## 
    ## Type: lag 
    ## Coefficients: (asymptotic standard errors) 
    ##               Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept)  3.2937095  0.5871588  5.6096 2.028e-08
    ## OPEN         0.0168863  0.0088472  1.9086  0.056307
    ## CRIME       -0.0188558  0.0031578 -5.9711 2.357e-09
    ## PLUMB        0.0371662  0.0122694  3.0292  0.002452
    ## PERIMETER   -0.4504245  0.2055560 -2.1913  0.028434
    ## AREA         2.4894178  1.1248392  2.2131  0.026889
    ## 
    ## Rho: 0.33025, LR test value: 4.4174, p-value: 0.035573
    ## Asymptotic standard error: 0.14202
    ##     z-value: 2.3254, p-value: 0.020052
    ## Wald statistic: 5.4074, p-value: 0.020052
    ## 
    ## Log likelihood: -6.943226 for lag model
    ## ML residual variance (sigma squared): 0.075721, (sigma: 0.27517)
    ## Number of observations: 49 
    ## Number of parameters estimated: 8 
    ## AIC: 29.886, (AIC for lm: 32.304)
    ## LM test for residual autocorrelation
    ## test value: 0.0055424, p-value: 0.94065

### Geographic Weighted Regression (GWR)

``` r
bw1 <- bw.gwr(HOVAL ~OPEN + CRIME + PLUMB + PERIMETER+AREA, 
              approach = "AIC", adaptive = T, data = columbus_shp)
```

    ## Adaptive bandwidth (number of nearest neighbours): 37 AICc value: 420.24 
    ## Adaptive bandwidth (number of nearest neighbours): 31 AICc value: 428.7673 
    ## Adaptive bandwidth (number of nearest neighbours): 42 AICc value: 415.9619 
    ## Adaptive bandwidth (number of nearest neighbours): 44 AICc value: 414.5406 
    ## Adaptive bandwidth (number of nearest neighbours): 46 AICc value: 413.0554 
    ## Adaptive bandwidth (number of nearest neighbours): 47 AICc value: 412.8153 
    ## Adaptive bandwidth (number of nearest neighbours): 48 AICc value: 411.8357 
    ## Adaptive bandwidth (number of nearest neighbours): 48 AICc value: 411.8357

``` r
m.gwr <- gwr.basic(HOVAL ~ OPEN + CRIME + PLUMB + PERIMETER+AREA, 
                   adaptive = T, data = columbus_shp, bw = bw1)  
m.gwr
```

    ##    ***********************************************************************
    ##    *                       Package   GWmodel                             *
    ##    ***********************************************************************
    ##    Program starts at: 2023-06-14 15:08:49 
    ##    Call:
    ##    gwr.basic(formula = HOVAL ~ OPEN + CRIME + PLUMB + PERIMETER + 
    ##     AREA, data = columbus_shp, bw = bw1, adaptive = T)
    ## 
    ##    Dependent (y) variable:  HOVAL
    ##    Independent variables:  OPEN CRIME PLUMB PERIMETER AREA
    ##    Number of data points: 49
    ##    ***********************************************************************
    ##    *                    Results of Global Regression                     *
    ##    ***********************************************************************
    ## 
    ##    Call:
    ##     lm(formula = formula, data = data)
    ## 
    ##    Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.273  -8.022  -2.486   3.691  57.580 
    ## 
    ##    Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ##    (Intercept)  77.2902    13.2902   5.816 6.78e-07 ***
    ##    OPEN          0.7080     0.4581   1.546   0.1295    
    ##    CRIME        -0.8036     0.1564  -5.139 6.42e-06 ***
    ##    PLUMB         1.4283     0.6250   2.285   0.0273 *  
    ##    PERIMETER   -19.6448    10.5911  -1.855   0.0705 .  
    ##    AREA        113.3562    57.8921   1.958   0.0567 .  
    ## 
    ##    ---Significance stars
    ##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ##    Residual standard error: 14.25 on 43 degrees of freedom
    ##    Multiple R-squared: 0.4665
    ##    Adjusted R-squared: 0.4045 
    ##    F-statistic: 7.521 on 5 and 43 DF,  p-value: 3.808e-05 
    ##    ***Extra Diagnostic information
    ##    Residual sum of squares: 8731.792
    ##    Sigma(hat): 13.63022
    ##    AIC:  407.0184
    ##    AICc:  409.7501
    ##    BIC:  398.5038
    ##    ***********************************************************************
    ##    *          Results of Geographically Weighted Regression              *
    ##    ***********************************************************************
    ## 
    ##    *********************Model calibration information*********************
    ##    Kernel function: bisquare 
    ##    Adaptive bandwidth: 48 (number of nearest neighbours)
    ##    Regression points: the same locations as observations are used.
    ##    Distance metric: Euclidean distance metric is used.
    ## 
    ##    ****************Summary of GWR coefficient estimates:******************
    ##                   Min.   1st Qu.    Median   3rd Qu.     Max.
    ##    Intercept  56.37188  70.79705  78.07835  83.60510  97.5815
    ##    OPEN        0.36160   0.52425   0.67909   0.75816   0.8371
    ##    CRIME      -1.02041  -0.86286  -0.78696  -0.70171  -0.6208
    ##    PLUMB       1.18185   1.32538   1.41076   1.53354   1.7248
    ##    PERIMETER -28.77662 -23.68468 -22.12539 -19.11283  -1.6203
    ##    AREA       -5.42755 107.80813 125.49743 139.04117 149.9512
    ##    ************************Diagnostic information*************************
    ##    Number of data points: 49 
    ##    Effective number of parameters (2trace(S) - trace(S'S)): 11.1129 
    ##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 37.8871 
    ##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 411.8357 
    ##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): 394.4764 
    ##    BIC (GWR book, Fotheringham, et al. 2002,GWR p. 61, eq. 2.34): 372.2258 
    ##    Residual sum of squares: 7448.039 
    ##    R-square value:  0.5449577 
    ##    Adjusted R-square value:  0.4078681 
    ## 
    ##    ***********************************************************************
    ##    Program stops at: 2023-06-14 15:08:49

``` r
gwr_sf = st_as_sf(m.gwr$SDF)
gwr_sf$y_predicted <- exp(gwr_sf$yhat)

tm_shape(gwr_sf) +
  tm_polygons(col = "y_predicted", palette="YlOrRd", style="quantile", n=8, title="Rate per 10,0000") +
   tm_layout(title= 'HOVAL',  title.position = c('right', 'top'))
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## Diagnóstico de Resultados Estimados

### Multicolinealidad

``` r
lm_auto <- lm(HOVAL ~ INC + CRIME + PLUMB+DISCBD+Y, data =  columbus)
summary(lm_auto)
```

    ## 
    ## Call:
    ## lm(formula = HOVAL ~ INC + CRIME + PLUMB + DISCBD + Y, data = columbus)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.44556 -0.22366 -0.02115  0.13431  0.90870 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.422750   0.453294   5.345 3.25e-06 ***
    ## INC          0.039456   0.011497   3.432 0.001337 ** 
    ## CRIME       -0.010837   0.004157  -2.607 0.012500 *  
    ## PLUMB        0.232858   0.060416   3.854 0.000382 ***
    ## DISCBD       0.136259   0.053397   2.552 0.014354 *  
    ## Y            0.016885   0.009140   1.847 0.071570 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2857 on 43 degrees of freedom
    ## Multiple R-squared:  0.6089, Adjusted R-squared:  0.5634 
    ## F-statistic: 13.39 on 5 and 43 DF,  p-value: 6.943e-08

Se dejaron para el modelo solo las variables de INC, CRIME, PLUMB, “Y” y
DISCBD ya que solo estas son significativas se logro un p value bastante
bajo por lo tanto se rechaza la hipotesis nula y se acepta la hipotesis
alternativa de que nuestro modelo puede predecir la variable dependiente

``` r
aic_lm = AIC(lm_auto)
aic_lm
```

    ## [1] 23.8928

El valor de AIC arrojado por este modelo es bastante bajo

``` r
VIF(lm_auto)
```

    ##      INC    CRIME    PLUMB   DISCBD        Y 
    ## 2.527805 2.843616 3.391452 3.492668 1.162565

Las variables seleccionadas arrojaron un resultado de VIF mayor a 1, por
lo tanto existe una intensa multicolinealidad entre las variables
seleccionadas para el modelo lineal, DISCBD, PLUMB Y CRIME tienen el
mayor valor VIF.

## Lagrange Multiplier Diagnostic for Spatial Dependence (LMlag)

``` r
lm.LMtests(lm_auto, rswm_queen, test = c("RLMlag"))
```

    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = HOVAL ~ INC + CRIME + PLUMB + DISCBD + Y, data =
    ## columbus)
    ## weights: rswm_queen
    ## 
    ## RLMlag = 0.0062677, df = 1, p-value = 0.9369

## Lagrange Multiplier Diagnostic for Spatial Error Dependence (LMerr)

``` r
lm.LMtests(lm_auto, rswm_queen, test = c("LMerr"))
```

    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = HOVAL ~ INC + CRIME + PLUMB + DISCBD + Y, data =
    ## columbus)
    ## weights: rswm_queen
    ## 
    ## LMErr = 0.12294, df = 1, p-value = 0.7259

## Autocorrelación Espacial de los residuales estimados (εi)

``` r
gwr_sf$exp_residuals <- exp(gwr_sf$residual)
tm_shape(gwr_sf) +
  tm_polygons(col = "exp_residuals", palette="Greys", style="quantile", n=8, title="Residuals") +
  tm_layout(title= 'Regression Residuals',  title.position = c('right', 'top'))
```

![](analisis_espacial_pp_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

## Selección de Modelo

### Especificar e interpretar criterio de selección de modelo

``` r
m_ = c('Linear Regression','SAR','Spatial durbin model',"GWR")
AIC = c(aic_lm,
       summary(spatial_autoregressive)$AIC,
       summary(spatial_durbin)$AIC,
       m.gwr$GW.diagnostic$AIC)
valores_ = data.frame(m_, AIC)
valores_
```

    ##                     m_       AIC
    ## 1    Linear Regression  23.89280
    ## 2                  SAR  32.24256
    ## 3 Spatial durbin model  32.30390
    ## 4                  GWR 394.47639

Tenemos que en este caso el modelo linear arrojo los mejores resultados
con el AIC mas bajo, el modelo del SAR arrojo tambien un AIC bastante
bajo sin embargo el P value de este modelo (mayor a .05) nos hace
descartarlo como significativo para predecir la variable dependiente.

## hallazgos identificados a partir de los resultados de ESDA y del modelo seleccionado

### 1.-

De acuerdo con el modelo linear la variable mas significativa es “PLUMB”
con una relacion positiva de .23, por lo tanto parece ser que las casas
SIN acceso a servicios de drenaje tienen un valor mucho mas elevado que
aquellas que carecen de dichos servicios; esto puede deberse de igual
forma a que son aquellas a las afueras de la ciudad.

### 2.-

la variable de INC, tiene la segunda relacion positiva mas significativa
en nuestro modelo, por lo tanto el precio de la casas tiende a elevarse
mas de acuerdo al ingreso de quienes habitan en ellas.

### 3.-

de acuerdo con el modelo Spatial durbin y SAR la variable perimetro
tiene una relacion negativa de -.4 con nuestra variable dependiente,
indicando que a mayor perimetro menor tiende a ser el precio de la
propiedad.

### 4.-

Si analizamos los mapas podemos ver que existe una relacion entre el
crimen, que tiende a acercarse al centro de la ciudad, con el precio que
tiende a ser mayor en areas lejos del centro de la ciudad y en areas con
menor crimen

### 5.-

Parece ser que las casas en las afueras tienen un mayor valor que
aquellas en el centro, por las variables ya mencionadas anteriormente,
el crimen que se acumula en el centro de la ciudad, el nivel de ingreso
de aquellas personas que viven en las afueras y tal vez pueden darse el
lujo de vivir en una zona mas segura y con mas areas abiertas, a pesar
de las carencias en cuestion de servicios de drenaje. Variables como
perimetro y drenaje, de las cuales podria intuirse que elevarian el
precio parecen tener una relacion negativa, no por el hecho de que sean
desventajas en una propiedad si no por que las casas con estas
caracteristicas tienden a agruparse en el centro de la ciudad, junto con
las desventajas que esto trae consigo.
