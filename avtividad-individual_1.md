Actv 1
================
Isaac Kelly Ramirez / a00829261
2023-05-12

# R Markdown

# Actividad 1

``` r
library(dplyr)
library(tigris)
library(foreign)
library(ggplot2)
library(dplyr)
library(regclass)
library(mctest)
library(lmtest)
library(spdep)
library(sf)
library(spData)
library(mapview)
library(spatialreg)
library(naniar)
library(dlookr)
library(caret)
library(e1071)
library(SparseM)
library(Metrics)
library(randomForest)
library(rpart.plot)
library(knitr)
library(insight)
library(rgeoda)
library(rgeos)
library(jtools)
library(xgboost)
library(DiagrammeR)
library(effects)
library(foreign)
library(ggplot2)
library(spdep)
library(spmoran)
library(spatialreg)
library(maptools)
library(mapproj)
library(sp)
library(maps)
library(rgeos)
library(ggmap)
library(mapproj)
library(RColorBrewer)
library(rgdal)
library(scales)
library(ggsn)
```

``` r
library(readr)
covid19_confirmados <- read.csv("C:/Users/chico/Downloads/covid19_confirmados.csv")
denue_hospitales <- read.csv("C:/Users/chico/Downloads/denue_hospitales.csv")
denue_hospitales_og= denue_hospitales
```

Se utilizaron dos bases de datos, covid19 confirmados, donde se
documentaron casos de covid entidad y municipio, asi como otras
variables como poblacion con acceso a servicios de salud, educacion,
etc.

``` r
#Deleting all the health centers that aren't considered as hospitals or related to the treatment of Covid or related.
denue_hospitales <- filter(denue_hospitales, codigo_act != 624191 & codigo_act !=
                               623311 & codigo_act !=
                               621331 & codigo_act !=
                               624198 & codigo_act !=
                               621311 & codigo_act !=
                               623991 & codigo_act !=
                               623992 & codigo_act !=
                               624221 & codigo_act !=
                               624112 & codigo_act !=
                               624111 & codigo_act !=
                               624311 & codigo_act !=
                               622311 & codigo_act !=
                               624411 & codigo_act !=
                               624412 & codigo_act !=
                               624122 & codigo_act !=
                               624222 & codigo_act !=
                               624312 & codigo_act !=
                               621411 & codigo_act !=
                               623312 & codigo_act !=
                               624121 & codigo_act !=
                               621412 & codigo_act !=
                               621312)
```

``` r
#Creacion de la clave de municipio en la base de datos de hospitales limpia
denue_hospitales$cve_mun <- (as.numeric(denue_hospitales$cve_ent) * 1000) + denue_hospitales$cve_mun
#Cambio de nombre para la clave de muncipio, para que ambas bases de datos cuenten con el mismo nombre. 
colnames(covid19_confirmados)[colnames(covid19_confirmados) == "cve_ent"] ="clave_municipio"
colnames(denue_hospitales)[colnames(denue_hospitales) == "cve_ent"] ="clave_municipio"

#Se elimina una coluna repetida y se cambia de nombre de mar_2021...32 a march_2021
covid19_confirmados$mar_2021.1 <- NULL
```

``` r
#covid19_confirmados <- covid19_confirmados %>% rename_with(~ "march_2021", mar_2021.1)
covid19_confirmados$total_casos <- rowSums(covid19_confirmados[,c("feb_2020", "march_2020", "april_2020", "may_2020", "june_2020", "july_2020", "august_2020", "sept_2020", "oct_2020", "nov_2020", "dic_2020","jan_2021","feb_2021", "mar_2021", "april_2021", "may_2021", "june_2021", "july_2021", "august_2021", "sept_2021", "oct_2021", "nov_2021", "dic_2021")], na.rm=TRUE)
```

## Tasa de Covid

Se crea la variable predictora Tasa covid, que toma en cuenta el total
de casos de covid 19 divido entre el total de poblacion

``` r
covid19_confirmados <- covid19_confirmados %>% mutate_at(c("hogrem2015", "hogremjefmuj2015", "popnoafmed2015", "gini2015", "porcentaje_pob_pobreza", "porcentaje_pob_pobreza_ext", "porcentaje_pob_servicios_salud", "porcentaje_pob_acceso_ss", "popden2020"), as.numeric)
covid19_confirmados$tasa_covid = (covid19_confirmados$total_casos/covid19_confirmados$poblacion_2022)*10000
summary(covid19_confirmados$tasa_covid)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   38.88   91.98  138.72  184.00 1950.98

``` r
#se crea una base de datos que hace genera una columna nueva con la suma de los hospitales
hospitales_con_contador <- denue_hospitales %>%
  group_by(clave_municipio) %>%
  mutate(num_hospitales = n()) %>% 
  ungroup()
```

``` r
# Nueva base de datos que cuenta solo con las columnas de numero de hospitales, clave muncipio, municipio y entidad. 
hospitalesMex <- denue_hospitales %>%
  count(clave_municipio,municipio,entidad)

#Merge para juntar numero de hospitales en la base de datos de covid
hospitalesMex$clave_municipio <- (as.numeric(hospitalesMex$clave_municipio) * 1000) + hospitalesMex$clave_municipio
#hospital_sum <- hospitalesMex %>%
#  group_by(clave_municipio, entidad) %>%
#  summarise(numero_hospitales = sum(n))

#hospital_sum$clave_municipio = prettyNum(hospital_sum$clave_municipio, scientific = FALSE, digits = 16)

cov_lista <- merge(covid19_confirmados,hospitalesMex, by = "clave_municipio", all.x=TRUE)

#Se elimina la columna de municipio repetida
colnames(cov_lista)[which(names(cov_lista) == "n")] <- "numero_hospitales"
cov_lista$municipio <- NULL
```

\#ZONA CENTRO

Se crea a continuacion un subset de la zona que analizaremos, en este
caso la zona centro del pais

``` r
zonacentro <- subset(cov_lista, entidad %in% c("Ciudad de M\xe9xico","Guanajuato", "Hidalgo",
"Morelos", "Puebla", "Querétaro","Tlaxcala", ""))
summary(zonacentro)
```

    ##  clave_municipio     mpio           poblacion_2022     hogrem2015    
    ##  Min.   : 9009   Length:458         Min.   :  1390   Min.   : 0.892  
    ##  1st Qu.:13013   Class :character   1st Qu.:  1390   1st Qu.: 3.646  
    ##  Median :21021   Mode  :character   Median : 14396   Median :12.665  
    ##  Mean   :18862                      Mean   : 30688   Mean   :16.374  
    ##  3rd Qu.:21021                      3rd Qu.: 41039   3rd Qu.:29.226  
    ##  Max.   :29029                      Max.   :139371   Max.   :29.226  
    ##  hogremjefmuj2015 popnoafmed2015     gini2015        popden2020     
    ##  Min.   :23.21    Min.   :13.01   Min.   :0.3390   Min.   :  43.89  
    ##  1st Qu.:26.27    1st Qu.:15.91   1st Qu.:0.3560   1st Qu.:  43.89  
    ##  Median :32.66    Median :15.91   Median :0.3560   Median : 119.96  
    ##  Mean   :30.04    Mean   :16.97   Mean   :0.3643   Mean   : 746.44  
    ##  3rd Qu.:32.66    3rd Qu.:19.38   3rd Qu.:0.3820   3rd Qu.: 637.09  
    ##  Max.   :40.80    Max.   :20.23   Max.   :0.4040   Max.   :6618.48  
    ##   crimen_2018      crimen_2019    inclusion_fin_2019 porcentaje_pob_pobreza
    ##  Min.   :  0.00   Min.   :10.13   Min.   :0.0000     Min.   :32.86         
    ##  1st Qu.:  0.00   1st Qu.:37.38   1st Qu.:0.0000     1st Qu.:40.33         
    ##  Median :  7.58   Median :77.84   Median :0.0000     Median :51.98         
    ##  Mean   : 21.74   Mean   :56.63   Mean   :0.1888     Mean   :47.59         
    ##  3rd Qu.: 22.72   3rd Qu.:77.84   3rd Qu.:0.0000     3rd Qu.:51.98         
    ##  Max.   :114.93   Max.   :77.98   Max.   :0.9800     Max.   :62.30         
    ##  porcentaje_pob_pobreza_ext porcentaje_pob_servicios_salud
    ##  Min.   : 2.850             Min.   :16.07                 
    ##  1st Qu.: 4.130             1st Qu.:16.07                 
    ##  Median : 9.880             Median :22.82                 
    ##  Mean   : 7.534             Mean   :21.00                 
    ##  3rd Qu.: 9.880             3rd Qu.:24.21                 
    ##  Max.   :11.890             Max.   :34.36                 
    ##  porcentaje_pob_acceso_ss pob_6.14_no_edu rezago_social       grado_rs        
    ##  Min.   :44.59            Min.   :3.970   Min.   :-1.1600   Length:458        
    ##  1st Qu.:49.85            1st Qu.:5.240   1st Qu.:-0.9000   Class :character  
    ##  Median :70.36            Median :5.240   Median :-0.7500   Mode  :character  
    ##  Mean   :62.40            Mean   :5.289   Mean   :-0.7624                     
    ##  3rd Qu.:70.36            3rd Qu.:6.190   3rd Qu.:-0.5400                     
    ##  Max.   :71.65            Max.   :6.410   Max.   :-0.5400                     
    ##     feb_2020   march_2020       april_2020        may_2020     
    ##  Min.   :0   Min.   :0.0000   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:0   1st Qu.:0.0000   1st Qu.:  0.00   1st Qu.:  0.00  
    ##  Median :0   Median :0.0000   Median :  3.00   Median : 14.00  
    ##  Mean   :0   Mean   :0.5066   Mean   : 10.86   Mean   : 39.22  
    ##  3rd Qu.:0   3rd Qu.:1.0000   3rd Qu.:  5.00   3rd Qu.: 28.00  
    ##  Max.   :0   Max.   :2.0000   Max.   :210.00   Max.   :627.00  
    ##    june_2020        july_2020       august_2020       sept_2020  
    ##  Min.   :  0.00   Min.   :  2.00   Min.   :  0.00   Min.   :  0  
    ##  1st Qu.:  0.00   1st Qu.:  2.00   1st Qu.:  0.00   1st Qu.:  0  
    ##  Median : 11.00   Median : 17.00   Median : 28.00   Median : 12  
    ##  Mean   : 52.48   Mean   : 55.29   Mean   : 46.09   Mean   : 44  
    ##  3rd Qu.: 35.00   3rd Qu.: 44.00   3rd Qu.: 32.00   3rd Qu.: 22  
    ##  Max.   :812.00   Max.   :812.00   Max.   :544.00   Max.   :558  
    ##     oct_2020        nov_2020         dic_2020       jan_2021     
    ##  Min.   :  0.0   Min.   :  0.00   Min.   :   0   Min.   :   4.0  
    ##  1st Qu.:  0.0   1st Qu.:  0.00   1st Qu.:   0   1st Qu.:   4.0  
    ##  Median :  5.0   Median : 14.00   Median :  22   Median :  71.0  
    ##  Mean   : 41.7   Mean   : 68.16   Mean   : 130   Mean   : 172.6  
    ##  3rd Qu.: 23.0   3rd Qu.: 54.00   3rd Qu.:  60   3rd Qu.:  84.0  
    ##  Max.   :549.0   Max.   :880.00   Max.   :2627   Max.   :2776.0  
    ##     feb_2021          mar_2021       april_2021        may_2021     
    ##  Min.   :   1.00   Min.   :  1.0   Min.   :  0.00   Min.   :  0.00  
    ##  1st Qu.:   1.00   1st Qu.:  1.0   1st Qu.:  0.00   1st Qu.:  0.00  
    ##  Median :  29.00   Median :  5.0   Median : 14.00   Median :  3.00  
    ##  Mean   :  83.96   Mean   : 48.6   Mean   : 25.25   Mean   : 13.45  
    ##  3rd Qu.:  44.00   3rd Qu.: 33.0   3rd Qu.: 29.00   3rd Qu.: 16.00  
    ##  Max.   :1370.00   Max.   :826.0   Max.   :372.00   Max.   :219.00  
    ##    june_2021        july_2021     august_2021       sept_2021      
    ##  Min.   :  0.00   Min.   :   0   Min.   :   1.0   Min.   :   3.00  
    ##  1st Qu.:  0.00   1st Qu.:   0   1st Qu.:   1.0   1st Qu.:   3.00  
    ##  Median :  1.00   Median :  28   Median :  35.0   Median :  19.00  
    ##  Mean   : 14.75   Mean   : 139   Mean   : 171.7   Mean   :  87.69  
    ##  3rd Qu.: 15.00   3rd Qu.:  60   3rd Qu.: 161.0   3rd Qu.:  77.00  
    ##  Max.   :271.00   Max.   :2806   Max.   :2955.0   Max.   :1036.00  
    ##     oct_2021         nov_2021         dic_2021       total_casos   
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :   12  
    ##  1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:   12  
    ##  Median :  7.00   Median :  2.00   Median :  4.00   Median :  362  
    ##  Mean   : 48.07   Mean   : 22.73   Mean   : 17.99   Mean   : 1334  
    ##  3rd Qu.: 47.00   3rd Qu.: 16.00   3rd Qu.: 11.00   3rd Qu.: 1066  
    ##  Max.   :429.00   Max.   :190.00   Max.   :216.00   Max.   :21086  
    ##    tasa_covid        entidad          numero_hospitales
    ##  Min.   :  86.33   Length:458         Min.   :   1.00  
    ##  1st Qu.:  86.33   Class :character   1st Qu.:   6.25  
    ##  Median : 146.65   Mode  :character   Median :  17.50  
    ##  Mean   : 212.05                      Mean   : 107.55  
    ##  3rd Qu.: 259.75                      3rd Qu.:  51.75  
    ##  Max.   :1512.94                      Max.   :4028.00

# MEXICO

``` r
summary(cov_lista)
```

    ##  clave_municipio     mpio           poblacion_2022      hogrem2015    
    ##  Min.   : 1001   Length:4822        Min.   :     95   Min.   : 0.000  
    ##  1st Qu.:14027   Class :character   1st Qu.:   4086   1st Qu.: 3.036  
    ##  Median :20020   Mode  :character   Median :  12804   Median : 6.583  
    ##  Mean   :19258                      Mean   :  42560   Mean   :10.398  
    ##  3rd Qu.:24024                      3rd Qu.:  41039   3rd Qu.:17.910  
    ##  Max.   :32058                      Max.   :1815551   Max.   :52.027  
    ##                                                       NA's   :1       
    ##  hogremjefmuj2015 popnoafmed2015      gini2015       popden2020      
    ##  Min.   : 0.00    Min.   : 1.124   Min.   :0.303   Min.   :    0.16  
    ##  1st Qu.:23.21    1st Qu.:10.543   1st Qu.:0.370   1st Qu.:   16.24  
    ##  Median :27.08    Median :15.913   Median :0.395   Median :   46.52  
    ##  Mean   :27.17    Mean   :15.961   Mean   :0.402   Mean   :  312.46  
    ##  3rd Qu.:32.34    3rd Qu.:19.356   3rd Qu.:0.443   3rd Qu.:  144.50  
    ##  Max.   :48.24    Max.   :62.101   Max.   :0.640   Max.   :56489.74  
    ##  NA's   :1        NA's   :1        NA's   :1       NA's   :1         
    ##   crimen_2018      crimen_2019     inclusion_fin_2019 porcentaje_pob_pobreza
    ##  Min.   :  0.00   Min.   :  0.00   Min.   : 0.0000    Min.   : 5.45         
    ##  1st Qu.:  0.00   1st Qu.:  5.23   1st Qu.: 0.0000    1st Qu.:47.29         
    ##  Median :  9.76   Median : 15.88   Median : 0.0000    Median :63.18         
    ##  Mean   : 23.42   Mean   : 23.26   Mean   : 0.4757    Mean   :61.88         
    ##  3rd Qu.: 33.90   3rd Qu.: 30.83   3rd Qu.: 0.6400    3rd Qu.:78.34         
    ##  Max.   :719.42   Max.   :551.82   Max.   :10.6800    Max.   :99.65         
    ##                                                       NA's   :1             
    ##  porcentaje_pob_pobreza_ext porcentaje_pob_servicios_salud
    ##  Min.   : 0.00              Min.   : 1.05                 
    ##  1st Qu.: 5.92              1st Qu.:15.72                 
    ##  Median :15.21              Median :21.45                 
    ##  Mean   :19.59              Mean   :23.47                 
    ##  3rd Qu.:29.40              3rd Qu.:30.41                 
    ##  Max.   :84.45              Max.   :83.86                 
    ##  NA's   :1                  NA's   :1                     
    ##  porcentaje_pob_acceso_ss pob_6.14_no_edu  rezago_social       grado_rs        
    ##  Min.   :22.03            Min.   : 0.000   Min.   :-1.5500   Length:4822       
    ##  1st Qu.:65.36            1st Qu.: 4.753   1st Qu.:-0.6800   Class :character  
    ##  Median :75.20            Median : 6.200   Median :-0.3200   Mode  :character  
    ##  Mean   :72.46            Mean   : 7.452   Mean   : 0.1114                     
    ##  3rd Qu.:82.98            3rd Qu.: 9.220   3rd Qu.: 0.9600                     
    ##  Max.   :96.99            Max.   :38.560   Max.   : 6.8300                     
    ##  NA's   :1                                                                     
    ##     feb_2020          march_2020       april_2020          may_2020      
    ##  Min.   :0.000000   Min.   :  0.00   Min.   :   0.000   Min.   :   0.00  
    ##  1st Qu.:0.000000   1st Qu.:  0.00   1st Qu.:   0.000   1st Qu.:   0.00  
    ##  Median :0.000000   Median :  0.00   Median :   0.000   Median :   0.00  
    ##  Mean   :0.001659   Mean   :  1.28   Mean   :   8.816   Mean   :  28.41  
    ##  3rd Qu.:0.000000   3rd Qu.:  0.00   3rd Qu.:   2.000   3rd Qu.:  11.00  
    ##  Max.   :1.000000   Max.   :139.00   Max.   :1902.000   Max.   :4543.00  
    ##                                                                          
    ##    june_2020         july_2020        august_2020        sept_2020      
    ##  Min.   :   0.00   Min.   :   0.00   Min.   :   0.00   Min.   :   0.00  
    ##  1st Qu.:   1.00   1st Qu.:   1.00   1st Qu.:   0.00   1st Qu.:   1.00  
    ##  Median :   4.00   Median :   4.00   Median :   3.00   Median :   2.00  
    ##  Mean   :  48.72   Mean   :  67.66   Mean   :  55.12   Mean   :  48.99  
    ##  3rd Qu.:  26.00   3rd Qu.:  34.00   3rd Qu.:  29.00   3rd Qu.:  21.00  
    ##  Max.   :5872.00   Max.   :6079.00   Max.   :4295.00   Max.   :3877.00  
    ##                                                                         
    ##     oct_2020          nov_2020          dic_2020           jan_2021      
    ##  Min.   :   0.00   Min.   :   0.00   Min.   :    0.00   Min.   :    0.0  
    ##  1st Qu.:   1.00   1st Qu.:   0.00   1st Qu.:    0.00   1st Qu.:    1.0  
    ##  Median :   3.00   Median :   3.00   Median :    3.00   Median :    7.0  
    ##  Mean   :  55.75   Mean   :  66.56   Mean   :   96.53   Mean   :  129.4  
    ##  3rd Qu.:  16.00   3rd Qu.:  17.00   3rd Qu.:   24.00   3rd Qu.:   51.0  
    ##  Max.   :8984.00   Max.   :6798.00   Max.   :19628.00   Max.   :24992.0  
    ##                                                                          
    ##     feb_2021          mar_2021         april_2021        may_2021      
    ##  Min.   :    0.0   Min.   :   0.00   Min.   :   0.0   Min.   :   0.00  
    ##  1st Qu.:    1.0   1st Qu.:   0.00   1st Qu.:   0.0   1st Qu.:   0.00  
    ##  Median :    5.0   Median :   2.00   Median :   1.0   Median :   1.00  
    ##  Mean   :   60.7   Mean   :  42.92   Mean   :  28.6   Mean   :  19.56  
    ##  3rd Qu.:   25.0   3rd Qu.:  16.00   3rd Qu.:  11.0   3rd Qu.:   6.00  
    ##  Max.   :11834.0   Max.   :9918.00   Max.   :5065.0   Max.   :3917.00  
    ##                                                                        
    ##    june_2021         july_2021        august_2021         sept_2021      
    ##  Min.   :   0.00   Min.   :    0.0   Min.   :    0.00   Min.   :   0.00  
    ##  1st Qu.:   0.00   1st Qu.:    1.0   1st Qu.:    4.00   1st Qu.:   1.00  
    ##  Median :   1.00   Median :    5.0   Median :   12.00   Median :   8.00  
    ##  Mean   :  29.04   Mean   :  116.3   Mean   :  176.60   Mean   :  83.55  
    ##  3rd Qu.:   9.00   3rd Qu.:   45.0   3rd Qu.:   88.75   3rd Qu.:  47.00  
    ##  Max.   :6640.00   Max.   :20315.0   Max.   :17590.00   Max.   :7742.00  
    ##                                                                          
    ##     oct_2021          nov_2021          dic_2021        total_casos      
    ##  Min.   :   0.00   Min.   :   0.00   Min.   :   0.00   Min.   :     0.0  
    ##  1st Qu.:   0.00   1st Qu.:   0.00   1st Qu.:   0.00   1st Qu.:    27.0  
    ##  Median :   3.00   Median :   1.00   Median :   1.00   Median :    99.5  
    ##  Mean   :  41.33   Mean   :  29.44   Mean   :  39.96   Mean   :  1275.3  
    ##  3rd Qu.:  25.00   3rd Qu.:  12.00   3rd Qu.:   7.00   3rd Qu.:   609.5  
    ##  Max.   :3556.00   Max.   :4854.00   Max.   :4708.00   Max.   :158582.0  
    ##                                                                          
    ##    tasa_covid        entidad          numero_hospitales
    ##  Min.   :   0.00   Length:4822        Min.   :   1.0   
    ##  1st Qu.:  57.05   Class :character   1st Qu.:   3.0   
    ##  Median :  92.18   Mode  :character   Median :  10.0   
    ##  Mean   : 140.66                      Mean   :  73.1   
    ##  3rd Qu.: 188.15                      3rd Qu.:  33.0   
    ##  Max.   :1950.98                      Max.   :4505.0   
    ##                                       NA's   :2429

Algunas diferencias entre los datos de la zona norte y el país de México
en general son las siguientes:

``` r
#Suma de la población en el 2022 de la zona norte del país y del país en general. 
sum(zonacentro$poblacion_2022)
```

    ## [1] 14054898

``` r
sum(cov_lista$poblacion_2022)
```

    ## [1] 205224680

Podemos observar que en la zona centro se concentra el 6% de la
poblacion total del pais

``` r
#Se genera un calculo para conocer el % de la población que se encuentra en la zona centro 
(sum(zonacentro$poblacion_2022)*100)/(sum(cov_lista$poblacion_2022))
```

    ## [1] 6.848542

``` r
# porcentaje_pob_servicios_salud
hist(cov_lista$poblacion_2022, probability = TRUE)
abline(v = mean(cov_lista$poblacion_2022), col='red', lwd = 3)
lines(density(cov_lista$poblacion_2022), col = 'green', lwd = 3)
title('Mexico ,poblacion 2022',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
hist(zonacentro$poblacion_2022, probability = TRUE)
abline(v = mean(zonacentro$poblacion_2022), col='red', lwd = 3)
lines(density(zonacentro$poblacion_2022), col = 'green', lwd = 3)
title('Zona centro, poblacion 2022',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
#Rezago social
hist(cov_lista$rezago_social, probability = TRUE)
abline(v = mean(cov_lista$rezago_social), col='red', lwd = 3)
lines(density(cov_lista$rezago_social), col = 'green', lwd = 3)
title('Mexico ,Rezago social',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
hist(zonacentro$rezago_social, probability = TRUE)
abline(v = mean(zonacentro$rezago_social), col='red', lwd = 3)
lines(density(zonacentro$rezago_social), col = 'green', lwd = 3)
title('Zona centro ,rezago social',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
hist(covid19_confirmados$total_casos, probability = TRUE)
abline(v = mean(covid19_confirmados$total_casos), col='red', lwd = 3)
lines(density(covid19_confirmados$total_casos), col = 'green', lwd = 3)
title('total_de_casos',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
hist(zonacentro$total_casos, probability = TRUE)
abline(v = mean(zonacentro$total_casos), col='red', lwd = 3)
lines(density(zonacentro$total_casos), col = 'green', lwd = 3)
title('Zona centro ,total_de_casos',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
ok <- sapply(cov_lista, is.numeric)
cov_lista[ok] <- lapply(cov_lista[ok], na.aggregate)
#Suma de la numero de hospitales de la zona centro del país y del país en general. 
sum(zonacentro$numero_hospitales)
```

    ## [1] 49256

``` r
sum(cov_lista$numero_hospitales, na.rm=TRUE)
```

    ## [1] 352503.7

Podemos ver que la zona centro cuenta con casi 50 mil hospitales, el 13%
de todos los hospitales del pais

``` r
#Se genera un calculo para conocer el % de la hospitales que se encuentra en la zona centro 
(sum(zonacentro$numero_hospitales)*100)/(sum(cov_lista$numero_hospitales))
```

    ## [1] 13.97319

``` r
hist(cov_lista$numero_hospitales, probability = TRUE)
abline(v = mean(cov_lista$numero_hospitales), col='red', lwd = 3)
lines(density(cov_lista$numero_hospitales), col = 'green', lwd = 3)
title('Mexico, numero_hospitales',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
hist(zonacentro$numero_hospitales, probability = TRUE)
abline(v = mean(zonacentro$numero_hospitales), col='red', lwd = 3)
lines(density(zonacentro$numero_hospitales), col = 'green', lwd = 3)
title(main='Zona centro, numero hospitales',outer=TRUE)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
### Modelo de Regresion No Espacial
model = lm(tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss +porcentaje_pob_servicios_salud + pob_6.14_no_edu +rezago_social, data = covid19_confirmados)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + 
    ##     porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss + 
    ##     porcentaje_pob_servicios_salud + pob_6.14_no_edu + rezago_social, 
    ##     data = covid19_confirmados)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -511.29  -56.59  -17.66   27.81 1477.15 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.761e+02  1.743e+01  21.574  < 2e-16 ***
    ## poblacion_2022                  1.824e-04  1.895e-05   9.623  < 2e-16 ***
    ## inclusion_fin_2019              3.390e+01  3.542e+00   9.571  < 2e-16 ***
    ## porcentaje_pob_pobreza         -2.472e+00  2.593e-01  -9.532  < 2e-16 ***
    ## porcentaje_pob_pobreza_ext      2.378e+00  4.214e-01   5.643 1.86e-08 ***
    ## porcentaje_pob_acceso_ss       -1.543e+00  2.610e-01  -5.911 3.87e-09 ***
    ## porcentaje_pob_servicios_salud -8.174e-01  2.173e-01  -3.762 0.000173 ***
    ## pob_6.14_no_edu                -3.026e+00  8.050e-01  -3.760 0.000174 ***
    ## rezago_social                  -3.312e+01  5.966e+00  -5.550 3.16e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 122.5 on 2447 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.4147, Adjusted R-squared:  0.4128 
    ## F-statistic: 216.7 on 8 and 2447 DF,  p-value: < 2.2e-16

Para el modelo de regresion lineal se seleccionaron las variables de
poblacion, inclusion financiera, porcentaje de poblacion en pobreza y
pobreza extrema, asi como acceso a servicios de salud, seguro social,
rezago social y poblacion sin servicios educativos.

``` r
covid19_map<-read_sf("C:/Users/chico/Downloads/shp_mx_mpios/mx_mpios.shp")
mex_states<-read_sf("C:/Users/chico/Downloads/shp_mx_mpios/mx_mpios.shp")
mex_states_a<-ggplot(data=mex_states) + geom_sf()
mex_states_a
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# se hace el rename de la columna clave_municipio a IDUNICO
cov_lista <- cov_lista %>%
                 rename(IDUNICO = clave_municipio)
#Se unen los datos de mapa con los de casos de covid en México para generar una sabana de datos. 
mapa_completo = right_join(covid19_map,cov_lista, by ="IDUNICO")
#Eliminar columna entidad la cual tenia acentos 
mapa_completo$entidad <- NULL
mapa_completo <- mapa_completo %>% mutate_at(c("hogrem2015", "hogremjefmuj2015", "popnoafmed2015","inclusion_fin_2019", "gini2015", "porcentaje_pob_pobreza", "porcentaje_pob_pobreza_ext", "porcentaje_pob_servicios_salud", "porcentaje_pob_acceso_ss", "popden2020"), as.numeric)
#Cambiar los datos a numerico
mapa_completo$`pob_6-14_no_edu` <- as.numeric(as.character(mapa_completo$`pob_6.14_no_edu`))
```

``` r
#Multiplicar por 100 todos los datos que se encuentran en %, los cuales se agregaron a una nueva base de datos para no perder los datos originales. 
mapa_completo_uno <- mapa_completo
mapa_completo_uno$hogrem2015 = mapa_completo$hogrem2015 * 100
mapa_completo_uno$hogremjefmuj2015 = mapa_completo$hogremjefmuj2015 * 100
mapa_completo_uno$popnoafmed2015 = mapa_completo$popnoafmed2015 * 100
mapa_completo_uno$inclusion_fin_2019 = mapa_completo$inclusion_fin_2019 * 100
mapa_completo_uno$porcentaje_pob_pobreza = mapa_completo$porcentaje_pob_pobreza * 100
mapa_completo_uno$porcentaje_pob_pobreza_ext = mapa_completo$porcentaje_pob_pobreza_ext * 100
mapa_completo_uno$porcentaje_pob_servicios_salud = mapa_completo$porcentaje_pob_servicios_salud * 100
mapa_completo_uno$porcentaje_pob_acceso_ss = mapa_completo$porcentaje_pob_acceso_ss * 100
mapa_completo_uno$`pob_6-14_no_edu` = mapa_completo$`pob_6.14_no_edu` * 100
```

``` r
library(car)
plot_normality(mapa_completo_uno,numero_hospitales)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
plot_normality(mapa_completo_uno,porcentaje_pob_servicios_salud)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
plot_normality(mapa_completo_uno,porcentaje_pob_pobreza)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->

``` r
plot_normality(mapa_completo_uno,porcentaje_pob_servicios_salud)
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-23-4.png)<!-- -->

``` r
library(corrr)
alt_mex <- mapa_completo_uno %>% select(numero_hospitales, poblacion_2022, crimen_2018, popnoafmed2015, inclusion_fin_2019, porcentaje_pob_pobreza, porcentaje_pob_servicios_salud, porcentaje_pob_pobreza_ext, porcentaje_pob_acceso_ss, rezago_social, `pob_6-14_no_edu`)

alt_mex_numeric <- select_if(alt_mex, is.numeric)

alt_mex_numeric$geometry <-NULL 
alt_mex_numeric$inclusion_fin_2019 <-NULL 
alt_mex_numeric$`pob_6-14_no_edu` <-NULL 
alt_mex_numeric$popnoafmed2015 <-NULL 

correlate(alt_mex_numeric) %>%  plot()
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
correlation_matrix <- cor(alt_mex_numeric[, c("poblacion_2022", "porcentaje_pob_pobreza_ext", "porcentaje_pob_acceso_ss","porcentaje_pob_pobreza","rezago_social","numero_hospitales","crimen_2018")])
correlation_matrix
```

    ##                            poblacion_2022 porcentaje_pob_pobreza_ext
    ## poblacion_2022                1.000000000                -0.20100681
    ## porcentaje_pob_pobreza_ext   -0.201006807                 1.00000000
    ## porcentaje_pob_acceso_ss     -0.378938384                 0.60362996
    ## porcentaje_pob_pobreza       -0.281370148                 0.84217026
    ## rezago_social                -0.212157826                 0.85542249
    ## numero_hospitales             0.105031102                -0.09348266
    ## crimen_2018                   0.004438592                 0.26385230
    ##                            porcentaje_pob_acceso_ss porcentaje_pob_pobreza
    ## poblacion_2022                          -0.37893838            -0.28137015
    ## porcentaje_pob_pobreza_ext               0.60362996             0.84217026
    ## porcentaje_pob_acceso_ss                 1.00000000             0.72993138
    ## porcentaje_pob_pobreza                   0.72993138             1.00000000
    ## rezago_social                            0.55459443             0.73609727
    ## numero_hospitales                       -0.04678666            -0.07291229
    ## crimen_2018                              0.07889377             0.07622622
    ##                            rezago_social numero_hospitales  crimen_2018
    ## poblacion_2022               -0.21215783        0.10503110  0.004438592
    ## porcentaje_pob_pobreza_ext    0.85542249       -0.09348266  0.263852305
    ## porcentaje_pob_acceso_ss      0.55459443       -0.04678666  0.078893766
    ## porcentaje_pob_pobreza        0.73609727       -0.07291229  0.076226224
    ## rezago_social                 1.00000000       -0.06883341  0.146766707
    ## numero_hospitales            -0.06883341        1.00000000 -0.050251897
    ## crimen_2018                   0.14676671       -0.05025190  1.000000000

Al generar la matriz de correlación encontramos que la vaariable de
poblacion_2022 tiene correlación con las variables de rezago_social,
numero_hospitales y crimen_2018.

# SPATIAL CONNECTIVITY MATRIX

``` r
mex.tr<-as(covid19_map, "Spatial")
mex_nb<-poly2nb(covid19_map)


boston_map_centroid<-coordinates(mex.tr) 
mex_map.linkW<-nb2listw(mex_nb, style="W")   
plot(mex.tr,border="blue",axes=FALSE,las=1, main="Covid_19 Spatial Connectivity Matrix")
plot(mex.tr,col="grey",border=grey(0.9),axes=T,add=T) 
plot(mex_map.linkW,coords=boston_map_centroid,pch=19,cex=0.1,col="red",add=T) 
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
Se puede observar con este mapa que existe una mayor concentración de
los datos en la zona centro y sur del país.

``` r
### Modelo de Regresion No Espacial
model = lm(tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss +porcentaje_pob_servicios_salud + pob_6.14_no_edu +rezago_social, data = mapa_completo_uno)

summary(model)
```

    ## 
    ## Call:
    ## lm(formula = tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + 
    ##     porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss + 
    ##     porcentaje_pob_servicios_salud + pob_6.14_no_edu + rezago_social, 
    ##     data = mapa_completo_uno)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -542.26  -59.92   -9.68   22.40 1511.79 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.815e+02  1.447e+01  26.369  < 2e-16 ***
    ## poblacion_2022                  2.585e-04  1.671e-05  15.466  < 2e-16 ***
    ## inclusion_fin_2019             -1.207e-02  1.709e-02  -0.706   0.4802    
    ## porcentaje_pob_pobreza         -1.492e-02  2.235e-03  -6.676 2.74e-11 ***
    ## porcentaje_pob_pobreza_ext      2.709e-03  2.832e-03   0.957   0.3388    
    ## porcentaje_pob_acceso_ss       -2.490e-02  2.142e-03 -11.624  < 2e-16 ***
    ## porcentaje_pob_servicios_salud  3.060e-03  1.683e-03   1.818   0.0691 .  
    ## pob_6.14_no_edu                 1.462e+00  6.235e-01   2.345   0.0191 *  
    ## rezago_social                  -1.701e+01  3.475e+00  -4.896 1.01e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 127.3 on 4813 degrees of freedom
    ## Multiple R-squared:  0.3116, Adjusted R-squared:  0.3104 
    ## F-statistic: 272.3 on 8 and 4813 DF,  p-value: < 2.2e-16

SPATIAL CLUSTERING

``` r
queen_w<-queen_weights(covid19_map)
```

``` r
lisa_MEDV<-local_moran(queen_w, covid19_map) 
covid19_map$cluster_MEDV<-as.factor(lisa_MEDV$GetClusterIndicators())
levels(covid19_map$cluster_MEDV)<-lisa_MEDV$GetLabels() # median values of owner-occupied housing in USD 1000
#SPATIAL CLUSTERING
# According to the Local Indicators of Spatial Autocorrelation, the following map indicates the clustering of high median house unit prices 
# particularly on the west side. It is required to inclue the spatial patterns of our dependent variable into the specification of regression model so we 
# improve prediction. 
ggplot(data=covid19_map) +
  geom_sf(aes(fill=cluster_MEDV)) + 
  ggtitle(label = "Spatial connectivity matrix", subtitle = "Covid 19 MX")
```

![](avtividad-individual_1_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
#mapa_completo_uno <- mapa_completo_uno %>% rename_with(~ "march_2021", mar_2021...32)
```

``` r
#mapa_completo_uno$Total_Casos <- rowSums(mapa_completo_uno[,c("feb_2020", "march_2020","april_2020","may_2020","june_2020","july_2020","august_2020","sept_2020","oct_2020","nov_2020","dic_2020", "jan_2021", "feb_2021", "mar_2021","april_2021","may_2021","june_2021","july_2021","august_2021","sept_2021","oct_2021","nov_2021","dic_2021")])
#mapa_completo_uno$mar_2021...33 <- NULL
```

SPATIAL AUTOGRESSIVE MODEL

``` r
#mapa_completo_uno <- mapa_completo_uno[!duplicated(mapa_completo_uno$tasa_covid),]
mapa_completo_uno <- na.omit(mapa_completo_uno)
mapa_completo_uno.link_a_rook<-poly2nb(mapa_completo_uno,queen=T)
mapa_completo_uno.linkW_a_rook<-nb2listw(mapa_completo_uno.link_a_rook, style="W")
```

### Spatial Lag model

Podemos ver que de acuerdo con el modelo Spatial Lag. la variable de
densidad de poblacion tiene un efecto positivo en los casos de covid
mientras que inclusion financiera, acceso a servicios de salud y seguro
social tienen efectos negativos sobre la variable dependiente.

``` r
spatial_lag_model <- lagsarlm(tasa_covid ~ inclusion_fin_2019+ popden2020 + porcentaje_pob_servicios_salud + porcentaje_pob_acceso_ss, data=mapa_completo_uno, method="Matrix", listw=nb2listw(mapa_completo_uno.link_a_rook))
summary(spatial_lag_model)
```

    ## 
    ## Call:lagsarlm(formula = tasa_covid ~ inclusion_fin_2019 + popden2020 + 
    ##     porcentaje_pob_servicios_salud + porcentaje_pob_acceso_ss, 
    ##     data = mapa_completo_uno, listw = nb2listw(mapa_completo_uno.link_a_rook), 
    ##     method = "Matrix")
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -817.3927  -21.6108   -4.5345   11.0338 1076.8300 
    ## 
    ## Type: lag 
    ## Coefficients: (numerical Hessian approximate standard errors) 
    ##                                   Estimate  Std. Error  z value  Pr(>|z|)
    ## (Intercept)                     1.3763e+02  7.9871e+00  17.2310 < 2.2e-16
    ## inclusion_fin_2019              4.1247e-02  1.0550e-02   3.9095 9.248e-05
    ## popden2020                      1.1131e-02  1.0176e-03  10.9384 < 2.2e-16
    ## porcentaje_pob_servicios_salud  3.0221e-04         NaN      NaN       NaN
    ## porcentaje_pob_acceso_ss       -1.6137e-02  9.9676e-04 -16.1894 < 2.2e-16
    ## 
    ## Rho: 0.79393, LR test value: 3735.5, p-value: < 2.22e-16
    ## Approximate (numerical Hessian) standard error: 0.0094985
    ##     z-value: 83.585, p-value: < 2.22e-16
    ## Wald statistic: 6986.5, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -28243.19 for lag model
    ## ML residual variance (sigma squared): 6639.1, (sigma: 81.481)
    ## Number of observations: 4821 
    ## Number of parameters estimated: 7 
    ## AIC: 56500, (AIC for lm: 60234)

### Spatial Error model

De las variables seleccionadas tenemos que inclusion financera y
densidad de poblacion tienen el mayor impacto en nuestra variable
dependiente, a mientras mas densidad de poblacion es mas probable que
aumenten los casos de covid de acuerdo con nuestro modelo, mientras que
a mayor inclusion financiera menos son los casos de covid presentes.

De igual forma se tiene que el P value de nuestro modelo es menor a 0.05
por lo tanto se rechaza la hipotesis nula y se concluye que nuestro
modelo puede predecir la variable dependiente con un AIC de 4038.

``` r
spatial_error_model <- errorsarlm(tasa_covid ~ porcentaje_pob_pobreza + porcentaje_pob_servicios_salud + popden2020 + inclusion_fin_2019, data = mapa_completo_uno, nb2listw(mapa_completo_uno.link_a_rook), method="Matrix")
summary(spatial_error_model)
```

    ## 
    ## Call:
    ## errorsarlm(formula = tasa_covid ~ porcentaje_pob_pobreza + porcentaje_pob_servicios_salud + 
    ##     popden2020 + inclusion_fin_2019, data = mapa_completo_uno, 
    ##     listw = nb2listw(mapa_completo_uno.link_a_rook), method = "Matrix")
    ## 
    ## Residuals:
    ##         Min          1Q      Median          3Q         Max 
    ## -862.281010  -15.479956    0.064225    8.558185 1158.062407 
    ## 
    ## Type: error 
    ## Coefficients: (asymptotic standard errors) 
    ##                                   Estimate  Std. Error  z value Pr(>|z|)
    ## (Intercept)                    325.3324517  13.4410812  24.2043  < 2e-16
    ## porcentaje_pob_pobreza          -0.0325331   0.0014378 -22.6277  < 2e-16
    ## porcentaje_pob_servicios_salud  -0.0037605   0.0015325  -2.4537  0.01414
    ## popden2020                       0.0144236   0.0012163  11.8585  < 2e-16
    ## inclusion_fin_2019               0.2987478   0.0194031  15.3969  < 2e-16
    ## 
    ## Lambda: 0.88101, LR test value: 4221.5, p-value: < 2.22e-16
    ## Approximate (numerical Hessian) standard error: 0.0080907
    ##     z-value: 108.89, p-value: < 2.22e-16
    ## Wald statistic: 11857, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -27949.07 for error model
    ## ML residual variance (sigma squared): 5705.4, (sigma: 75.534)
    ## Number of observations: 4821 
    ## Number of parameters estimated: 7 
    ## AIC: 55912, (AIC for lm: 60132)
