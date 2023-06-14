Actv 1
================
Isaac Kelly Ramirez / a00829261
2023-05-12

# R Markdown

# Actividad 2

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
covid19_map<-read_sf("C:/Users/chico/Downloads/shp_mx_mpios/mx_mpios.shp")
mex_states<-read_sf("C:/Users/chico/Downloads/shp_mx_mpios/mx_mpios.shp")
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

``` r
ok <- sapply(cov_lista, is.numeric)
cov_lista[ok] <- lapply(cov_lista[ok], na.aggregate)
```

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

![](actividad_2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot_normality(mapa_completo_uno,porcentaje_pob_servicios_salud)
```

![](actividad_2_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
plot_normality(mapa_completo_uno,porcentaje_pob_pobreza)
```

![](actividad_2_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
plot_normality(mapa_completo_uno,porcentaje_pob_servicios_salud)
```

![](actividad_2_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

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

![](actividad_2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

SPATIAL CONNECTIVITY MATRIX

``` r
mex.tr<-as(covid19_map, "Spatial")
mex_nb<-poly2nb(covid19_map)


boston_map_centroid<-coordinates(mex.tr) 
mex_map.linkW<-nb2listw(mex_nb, style="W")
```

## Modelos

### Regresion no espacial: regresion linear

``` r
model = lm(tasa_covid ~ poblacion_2022 + porcentaje_pob_pobreza + porcentaje_pob_acceso_ss +porcentaje_pob_servicios_salud + pob_6.14_no_edu +rezago_social, data = mapa_completo_uno)

summary(model)
```

    ## 
    ## Call:
    ## lm(formula = tasa_covid ~ poblacion_2022 + porcentaje_pob_pobreza + 
    ##     porcentaje_pob_acceso_ss + porcentaje_pob_servicios_salud + 
    ##     pob_6.14_no_edu + rezago_social, data = mapa_completo_uno)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -539.61  -59.44   -6.82   22.41 1510.75 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     3.769e+02  1.367e+01  27.576  < 2e-16 ***
    ## poblacion_2022                  2.577e-04  1.648e-05  15.637  < 2e-16 ***
    ## porcentaje_pob_pobreza         -1.344e-02  1.754e-03  -7.661 2.22e-14 ***
    ## porcentaje_pob_acceso_ss       -2.496e-02  2.128e-03 -11.727  < 2e-16 ***
    ## porcentaje_pob_servicios_salud  2.899e-03  1.673e-03   1.732  0.08327 .  
    ## pob_6.14_no_edu                 1.578e+00  6.113e-01   2.582  0.00985 ** 
    ## rezago_social                  -1.577e+01  3.163e+00  -4.987 6.36e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 127.3 on 4815 degrees of freedom
    ## Multiple R-squared:  0.3114, Adjusted R-squared:  0.3106 
    ## F-statistic: 362.9 on 6 and 4815 DF,  p-value: < 2.2e-16

Podemos observar que de acuerdo con el modelo de regresion lineal la
variable poblacion y porcentaje de poblacion con acceso a servicios de
salud tienen el mayor impacto positivo con la variable dependiente, por
lo tanto se puede determinar que a mas poblacion mas casos de covid y
mientras mas poblacion tiene acceso a servicios de salud al parecer mas
casos fueron diagnosticados, de igual forma parece ser que mientras mas
poblacion este afiliada a seguro social menos son los casos de covid, de
igual forma el rezago social y la pobreza al ser menores parece ser que
se reducen los casos. el p value de este modelo es menor a 0.05 por lo
tanto se rechaza la hipotesis nula y se concluye que el modelo puede
explicar la variable dependiente.

### Regresion no espacial: Regresion polinomial

Se convirtieron las variables significativas a polinomiales, para
determinar si esto ayuda al modelo a explicar de mejor manera la
variable dependiente.

``` r
mapa_completo_uno$poblacion_2022_2 <- mapa_completo_uno$poblacion_2022^2
mapa_completo_uno$inclusion_fin_2019_2 <- mapa_completo_uno$inclusion_fin_2019^2
mapa_completo_uno$porcentaje_pob_pobreza_2 <- mapa_completo_uno$porcentaje_pob_pobreza^2
mapa_completo_uno$rezago_social_2 <- mapa_completo_uno$rezago_social^2
mapa_completo_uno$porcentaje_pob_servicios_salud_2 <- mapa_completo_uno$porcentaje_pob_servicios_salud^2

regresorpoly = lm(tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss +porcentaje_pob_servicios_salud + pob_6.14_no_edu +rezago_social + poblacion_2022_2+ inclusion_fin_2019_2 + porcentaje_pob_pobreza_2+ rezago_social_2 + porcentaje_pob_servicios_salud_2, data = mapa_completo_uno)

summary(regresorpoly)
```

    ## 
    ## Call:
    ## lm(formula = tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + 
    ##     porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss + 
    ##     porcentaje_pob_servicios_salud + pob_6.14_no_edu + rezago_social + 
    ##     poblacion_2022_2 + inclusion_fin_2019_2 + porcentaje_pob_pobreza_2 + 
    ##     rezago_social_2 + porcentaje_pob_servicios_salud_2, data = mapa_completo_uno)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -353.26  -52.77   -0.96   16.60 1387.58 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       2.005e+02  1.942e+01  10.324  < 2e-16 ***
    ## poblacion_2022                    6.175e-04  4.122e-05  14.983  < 2e-16 ***
    ## inclusion_fin_2019                4.245e-01  4.338e-02   9.784  < 2e-16 ***
    ## porcentaje_pob_pobreza           -1.007e-02  5.530e-03  -1.820 0.068785 .  
    ## porcentaje_pob_pobreza_ext        2.222e-02  3.003e-03   7.399 1.62e-13 ***
    ## porcentaje_pob_acceso_ss         -7.583e-03  2.146e-03  -3.533 0.000415 ***
    ## porcentaje_pob_servicios_salud    3.030e-02  5.962e-03   5.083 3.86e-07 ***
    ## pob_6.14_no_edu                  -3.109e+00  6.397e-01  -4.860 1.21e-06 ***
    ## rezago_social                    -5.825e+01  5.187e+00 -11.231  < 2e-16 ***
    ## poblacion_2022_2                 -3.896e-10  3.199e-11 -12.180  < 2e-16 ***
    ## inclusion_fin_2019_2             -5.317e-04  4.376e-05 -12.150  < 2e-16 ***
    ## porcentaje_pob_pobreza_2         -6.118e-07  4.755e-07  -1.287 0.198224    
    ## rezago_social_2                   8.846e+00  7.202e-01  12.283  < 2e-16 ***
    ## porcentaje_pob_servicios_salud_2 -6.724e-06  9.794e-07  -6.865 7.49e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 119.3 on 4808 degrees of freedom
    ## Multiple R-squared:  0.396,  Adjusted R-squared:  0.3944 
    ## F-statistic: 242.5 on 13 and 4808 DF,  p-value: < 2.2e-16

Podemos observar que la mayoria de variables fueron significativas,
tanto polinomiales como no polinomiales, siendo en este caso poblacion e
inclusion financiera asi como rezago social las que mas tuvieron impacto
positivo en la variable dependiente, acceso a seguro social y servicios
de salud, asi como pobreza tienen un impacto negativo en los casos de
covid.

el p value de este modelo es menor a 0.05 por lo tanto se rechaza la
hipotesis nula y se concluye que el modelo puede explicar la variable
dependiente.

## Modelos de regresion espaciales

### LM moran

``` r
mapa_completo_uno <- na.omit(mapa_completo_uno)
mapa_completo.link_a_rook<-poly2nb(mapa_completo_uno,queen=T)
mapa_completo.linkW_a_rook<-nb2listw(mapa_completo.link_a_rook, style="W")

lm_model_ <- lm(tasa_covid ~ poblacion_2022 + inclusion_fin_2019 + porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext + porcentaje_pob_acceso_ss +porcentaje_pob_servicios_salud + pob_6.14_no_edu +rezago_social, data = mapa_completo_uno)
```

``` r
lm.morantest(lm_model_, nb2listw(mapa_completo.link_a_rook))
```

    ## 
    ##  Global Moran I for regression residuals
    ## 
    ## data:  
    ## model: lm(formula = tasa_covid ~ poblacion_2022 + inclusion_fin_2019 +
    ## porcentaje_pob_pobreza + porcentaje_pob_pobreza_ext +
    ## porcentaje_pob_acceso_ss + porcentaje_pob_servicios_salud +
    ## pob_6.14_no_edu + rezago_social, data = mapa_completo_uno)
    ## weights: nb2listw(mapa_completo.link_a_rook)
    ## 
    ## Moran I statistic standard deviate = 109.48, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Observed Moran I      Expectation         Variance 
    ##     0.6671897711    -0.0013393420     0.0000372889

Al tener un p-value significativo y valor Moran positivo de 0.66 se
puede confirmar la existencia de autocorrelación espacial positiva, por
lo tanto es posible y seria recomendable hacer modelos autoregresivos
para nuestro analisis.

### Spatial Autoregressive model

``` r
covid19_map <- readShapePoly("C:/Users/chico/Downloads/shp_mx_mpios/mx_mpios.shp", IDvar = "IDUNICO", proj4string = CRS("+proj=longlat"), sf_use_s2(FALSE))
```

    ## Shapefile type: Polygon, (5), # of Shapes: 2456

``` r
lmat_c<-coordinates(covid19_map)
map.centroid_c<-coordinates(covid19_map)
```

``` r
spatial_autoreg <- lagsarlm(tasa_covid ~ gini2015 + porcentaje_pob_pobreza + rezago_social + popden2020 + porcentaje_pob_acceso_ss + popnoafmed2015, data = mapa_completo_uno, nb2listw(mapa_completo.link_a_rook), method="Matrix")
summary(spatial_autoreg)
```

    ## 
    ## Call:lagsarlm(formula = tasa_covid ~ gini2015 + porcentaje_pob_pobreza + 
    ##     rezago_social + popden2020 + porcentaje_pob_acceso_ss + popnoafmed2015, 
    ##     data = mapa_completo_uno, listw = nb2listw(mapa_completo.link_a_rook), 
    ##     method = "Matrix")
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -796.4375  -18.5794   -6.6124   10.3021 1097.7026 
    ## 
    ## Type: lag 
    ## Coefficients: (numerical Hessian approximate standard errors) 
    ##                             Estimate  Std. Error  z value  Pr(>|z|)
    ## (Intercept)              -1.3951e+00         NaN      NaN       NaN
    ## gini2015                  4.2355e+02  1.9844e+01  21.3438 < 2.2e-16
    ## porcentaje_pob_pobreza   -3.6797e-03  8.4203e-04  -4.3700 1.242e-05
    ## rezago_social            -7.0279e+00  1.1336e+00  -6.1994 5.667e-10
    ## popden2020                1.3159e-02  1.0424e-03  12.6229 < 2.2e-16
    ## porcentaje_pob_acceso_ss -1.4121e-02  1.1486e-03 -12.2939 < 2.2e-16
    ## popnoafmed2015           -9.9739e-03  1.7796e-03  -5.6045 2.089e-08
    ## 
    ## Rho: 0.76376, LR test value: 3386, p-value: < 2.22e-16
    ## Approximate (numerical Hessian) standard error: 0.010161
    ##     z-value: 75.169, p-value: < 2.22e-16
    ## Wald statistic: 5650.4, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -28143.16 for lag model
    ## ML residual variance (sigma squared): 6420.7, (sigma: 80.129)
    ## Number of observations: 4821 
    ## Number of parameters estimated: 9 
    ## AIC: 56304, (AIC for lm: 59688)

Podemos ver que ahora en el caso del modelo espacial autoregresivo la
variable de gini2015 tiene el mayor impacto positivo en la variable
dependeinte, y la poblacion no afiliada a servicios medicos y el rezago
social el mayor impacto negativo.

el p value de este modelo es menor a 0.05 por lo tanto se rechaza la
hipotesis nula y se concluye que el modelo puede explicar la variable
dependiente.

### Spatial Lag model

``` r
spatial_lag_model <- lagsarlm(tasa_covid ~ gini2015 + inclusion_fin_2019+ popden2020 + porcentaje_pob_acceso_ss, data=mapa_completo_uno, method="Matrix", listw=nb2listw(mapa_completo.link_a_rook))
summary(spatial_lag_model)
```

    ## 
    ## Call:lagsarlm(formula = tasa_covid ~ gini2015 + inclusion_fin_2019 + 
    ##     popden2020 + porcentaje_pob_acceso_ss, data = mapa_completo_uno, 
    ##     listw = nb2listw(mapa_completo.link_a_rook), method = "Matrix")
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -816.1012  -23.7507   -5.7084   12.5485 1080.9271 
    ## 
    ## Type: lag 
    ## Coefficients: (numerical Hessian approximate standard errors) 
    ##                             Estimate  Std. Error  z value  Pr(>|z|)
    ## (Intercept)               38.7946178  12.6655068   3.0630  0.002191
    ## gini2015                 303.6312979  31.1297485   9.7537 < 2.2e-16
    ## inclusion_fin_2019         0.0329896   0.0108069   3.0527  0.002268
    ## popden2020                 0.0111464   0.0010184  10.9448 < 2.2e-16
    ## porcentaje_pob_acceso_ss  -0.0191127   0.0010255 -18.6377 < 2.2e-16
    ## 
    ## Rho: 0.79004, LR test value: 3763, p-value: < 2.22e-16
    ## Approximate (numerical Hessian) standard error: 0.0093593
    ##     z-value: 84.412, p-value: < 2.22e-16
    ## Wald statistic: 7125.4, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -28187.58 for lag model
    ## ML residual variance (sigma squared): 6494.7, (sigma: 80.59)
    ## Number of observations: 4821 
    ## Number of parameters estimated: 7 
    ## AIC: 56389, (AIC for lm: 60150)

De igual forma el modelo de spatial lag da al mayor importancia positiva
a la variable Gini2015 tambien el p value de este modelo fue menor a
0.05 por lo tanto se rechaza la hipotesis nula y se concluye que el
modelo puede explicar la variable dependiente.

### Spatial Error model

``` r
spatial_error_model <- errorsarlm(tasa_covid ~ porcentaje_pob_pobreza + porcentaje_pob_servicios_salud + popden2020 + inclusion_fin_2019, data = mapa_completo_uno, nb2listw(mapa_completo.link_a_rook), method="Matrix")
summary(spatial_error_model)
```

    ## 
    ## Call:
    ## errorsarlm(formula = tasa_covid ~ porcentaje_pob_pobreza + porcentaje_pob_servicios_salud + 
    ##     popden2020 + inclusion_fin_2019, data = mapa_completo_uno, 
    ##     listw = nb2listw(mapa_completo.link_a_rook), method = "Matrix")
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

En el modelo Spatial Error podemos observar que las variables inclusion
financiera y densidad de poblacion tienen una relacion positiva mientras
que servicios de salud y polacion en pobreza tienen una relacion
negativa.

### RMSE de los modelos

``` r
rmse_lm= sqrt(mean((mapa_completo_uno$tasa_covid - model$fitted.values)^2))
rmse_pr = sqrt(mean((mapa_completo_uno$tasa_covid - regresorpoly$fitted.values)^2))
rmse_sam=sqrt(mean((mapa_completo_uno$tasa_covid- spatial_autoreg$fitted.values)^2))
rmse_lag = sqrt(mean((mapa_completo_uno$tasa_covid- spatial_lag_model$fitted.values)^2))
rmse_sem= sqrt(mean((mapa_completo_uno$tasa_covid - spatial_error_model$fitted.values)^2))
```

``` r
modelos=c('Linear regression',"polynomial regression", 'Spatial autoregressive', "spatial lag model",'Spatial error model')
valor_rmse = c(rmse_lm, rmse_pr, rmse_sam, rmse_lag, rmse_sem)
df_rmse<-data.frame(modelos,valor_rmse)
df_rmse <- df_rmse %>% arrange(valor_rmse)
df_rmse
```

    ##                  modelos valor_rmse
    ## 1    Spatial error model   75.53421
    ## 2 Spatial autoregressive   80.12901
    ## 3      spatial lag model   80.59000
    ## 4  polynomial regression  120.59499
    ## 5      Linear regression  128.79765

De acuerdo con los valores de RMSE obtenidos los 3 modelos de regresion
espacial tienen un RMSE mcuho menor a aquellos no espaciales, siendo en
este caso el modelo de error espacial el que tiene el menor valor de
RMSE; y por lo tanto el modelo que mejor explica la variable
dependiente.

## Preguntas detonantes

### ¿Cuáles son los principales factores socioeconómicos que propician un incremento / Disminución de los casos confirmados de COVID-19?

De acuerdo con cada modelo hubo diversas variables que explicaron los
casos de covid con significancia, así que tomando los resultados del
modelo con mejor RMSE y los otros modelos que tambien resultaron
significativos, se concluyo que las variables de mayor importancia son
la densidad de poblacion,el Porcentaje de Pobreza, el Porcentaje de
Población sin Acceso a Servicios de Salud, Porcentaje de la Población
sin Acceso a Seguro Social y la tasa de inclusion financiera de la
poblacion.
