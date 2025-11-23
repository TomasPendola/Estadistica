pacman::p_load(tidyverse, 
               haven, 
               readstata13,
               cluster,
               factoextra, 
               NbClust, 
               psych, 
               GPArotation, 
               FactoMineR, 
               ca, 
               corrplot, 
               reshape2, 
               sjPlot,
               ggrepel,
               broom,
               gtsummary,
               DescTools,
               gginference,
               rempsyc)


options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

Latinobarometro_2024 <- read_sav("Estadistica 4/Latinobarometro_2024_Spss_esp_v20250817.sav")

data <- Latinobarometro_2024 %>% select(P14ST.E, #Confianza en el gobierno  
                                        P14ST.F, #Confianza en el poder judicial
                                        P14ST.G, #Confianza en los partidos políticos
                                        P14STGBS.B, #Confianza en la policía
                                        P14ST.K, #Confianza en los medios de comunicación 
                                        P12STGBS.A, # Satisfaccion con la Democracia
                                        IDENPA) #País


# Filtrar la base completa para 4 paises, que es Chile, Argentina, Peru y El Salvador

data <- data[data$IDENPA %in% c(152,32,222,604),]

# Renombramos las Variables

data <- data %>%
  rename(
    conf_gobierno      = P14ST.E,
    conf_poder_judicial = P14ST.F,
    conf_partidos      = P14ST.G,
    conf_policia       = P14STGBS.B,
    conf_medios        = P14ST.K,
    satisf_democracia  = P12STGBS.A)


# Tratamiento de los Na

data <- data %>%
  mutate(across(c(conf_gobierno, conf_poder_judicial, conf_partidos, 
                  conf_policia, conf_medios, satisf_democracia),
                ~ ifelse(. %in% c(-1, -2), NA, .)))

# Omitimos los Na

data_limpia <- na.omit(data)

# Recodificar la variable de Pais

data_limpia <- data_limpia %>%
  mutate(Pais = case_when(
    IDENPA == 32 ~ "Argentina",
    IDENPA == 152 ~ "Chile",
    IDENPA == 222 ~ "El Salvador",
    IDENPA == 604 ~ "Peru", 
    TRUE ~ NA_character_))

# Recodificacion de las Variables de confianza 

# Recodificar cada variable con etiquetas específicas
data_limpia_categorica <- data_limpia %>%
  mutate(
    # Recodificar conf_gobierno
    conf_gobierno = case_when(
      conf_gobierno == 1 ~ "Mucha confianza",
      conf_gobierno == 2 ~ "Algo de confianza", 
      conf_gobierno == 3 ~ "Poca confianza",
      conf_gobierno == 4 ~ "Ninguna confianza",
      conf_gobierno %in% c(-1, -2) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Recodificar conf_poder_judicial
    conf_poder_judicial = case_when(
      conf_poder_judicial == 1 ~ "Mucha confianza",
      conf_poder_judicial == 2 ~ "Algo de confianza",
      conf_poder_judicial == 3 ~ "Poca confianza", 
      conf_poder_judicial == 4 ~ "Ninguna confianza",
      conf_poder_judicial %in% c(-1, -2) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Recodificar conf_partidos
    conf_partidos = case_when(
      conf_partidos == 1 ~ "Mucha confianza",
      conf_partidos == 2 ~ "Algo de confianza",
      conf_partidos == 3 ~ "Poca confianza",
      conf_partidos == 4 ~ "Ninguna confianza", 
      conf_partidos %in% c(-1, -2) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Recodificar conf_policia
    conf_policia = case_when(
      conf_policia == 1 ~ "Mucha confianza",
      conf_policia == 2 ~ "Algo de confianza",
      conf_policia == 3 ~ "Poca confianza",
      conf_policia == 4 ~ "Ninguna confianza",
      conf_policia %in% c(-1, -2) ~ NA_character_, 
      TRUE ~ NA_character_
    ),
    
    # Recodificar conf_medios
    conf_medios = case_when(
      conf_medios == 1 ~ "Mucha confianza",
      conf_medios == 2 ~ "Algo de confianza",
      conf_medios == 3 ~ "Poca confianza",
      conf_medios == 4 ~ "Ninguna confianza",
      conf_medios %in% c(-1, -2) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Recodificar satisf_democracia (si usa la misma escala)
    satisf_democracia = case_when(
      satisf_democracia == 1 ~ "Muy Satisfecho",
      satisf_democracia == 2 ~ "Algo Satisfecho", 
      satisf_democracia == 3 ~ "Poca Satisfecho",
      satisf_democracia == 4 ~ "Nada Satisfecho",
      satisf_democracia %in% c(-1, -2) ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>%
  # Convertir a factores ordenados
  mutate(across(c(conf_gobierno, conf_poder_judicial, conf_partidos,
                  conf_policia, conf_medios),
                ~ factor(., levels = c("Mucha confianza", "Algo de confianza", 
                                       "Poca confianza", "Ninguna confianza"))))

# Primer Modelo Analsis Factoria Exploratorio (EFA)

# Seleccionar solo las variables para AFE

variables_afe <- data_limpia %>%
  select(conf_gobierno, conf_poder_judicial, conf_partidos, conf_policia, conf_medios, satisf_democracia) %>%
  na.omit()  # Eliminar missing values para el análisis

# 1. Verificar adecuación del AFE

KMO(variables_afe)  # Test de Kaiser-Meyer-Olkin
cortest.bartlett(variables_afe)  # Test de esfericidad de Bartlett

# 2. Determinar número de factores
# Scree plot

scree(variables_afe, factors = FALSE)

# Análisis paralelo

fa.parallel(variables_afe, fa = "fa")

# 3. Análisis factorial exploratorio

afe <- fa(variables_afe, 
          nfactors = 3,  # Ajustar según scree plot y análisis paralelo
          rotate = "varimax",  # Rotación ortogonal
          fm = "minres")  # Método de extracción

# 4. Ver resultados

print(afe, digits = 3, sort = TRUE)
fa.diagram(afe)

# 5. Confiabilidad de escalas

alpha(variables_afe)


# Segundo Modelo Analisis de Conglomerados o Clusters *hacer un dendogram??*

# 1. Preparar datos para clustering

datos_cluster <- data_limpia %>%
  select(conf_gobierno, conf_poder_judicial, conf_partidos, conf_policia, conf_medios, satisf_democracia, IDENPA) %>%
  na.omit() %>%  # Eliminar missing values
  scale()         # Estandarizar variables (importante para clustering)

# 2. Determinar número óptimo de clusters

fviz_nbclust(datos_cluster, kmeans, method = "wss") + 
  ggtitle("Método del Codo")

fviz_nbclust(datos_cluster, kmeans, method = "silhouette") +
  ggtitle("Método de la Silueta")

# 3. K-means clustering (ejemplo con 3 clusters)

set.seed(123)  # Para reproducibilidad
kmeans_result <- kmeans(datos_cluster, centers = 4, nstart = 25)

# 4. Visualizar resultados

fviz_cluster(kmeans_result, data = datos_cluster, 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF0000"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# 5. Caracterizar los clusters

# Añadir asignación de clusters al dataset original

latinobarometro_cluster <- data %>%
  na.omit() %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Ver perfiles de clusters

perfiles_cluster <- latinobarometro_cluster %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    conf_gobierno = mean(conf_gobierno, na.rm = TRUE),
    conf_judicial = mean(conf_poder_judicial, na.rm = TRUE),
    conf_partidos = mean(conf_partidos, na.rm = TRUE),
    conf_medios = mean(conf_medios, na.rm = TRUE),
    conf_policia = mean(conf_policia, na.rm = TRUE),
    satis_democracia = mean(satisf_democracia, na.rm = TRUE))

print(perfiles_cluster)


# Tercer Modelos: Analisis de Correspondencia Multiple 


# 1 Creación de tablas de contingencia # 

# Confianza en el Gobierno por Pais

sjPlot::sjt.xtab(var.row = data_limpia_categorica$Pais, # Variable independiente
                 var.col = data_limpia_categorica$conf_gobierno, # Variable dependiente
                 show.summary = F, # para que no enseñe información del tipo summary
                 emph.total = T, # para destacar visualmente los totales de la fila
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T, # porcentaje columna
                 encoding= "UTF-8") # para que lea los archivos bajo el alfabeto español

# Pais por Confianza en el poder judicial

sjPlot::sjt.xtab(var.row = data_limpia_categorica$Pais, # Variable independiente
                 var.col = data_limpia_categorica$conf_poder_judicial, # Variable dependiente
                 show.summary = F, # para que no enseñe información del tipo summary
                 emph.total = T, # para destacar visualmente los totales de la fila como suma
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T, # porcentaje columna
                 encoding= "UTF-8") # para que lea los archivos bajo el alfabeto español

# Pais por Confianza en Partdos

sjPlot::sjt.xtab(var.row = data_limpia_categorica$Pais, # Variable independiente
                 var.col = data_limpia_categorica$conf_partidos, # Variable dependiente
                 show.summary = F, # para que no enseñe información del tipo summary
                 emph.total = T, # para destacar visualmente los totales de la fila como suma
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T, # porcentaje columna
                 encoding= "UTF-8") # para que lea los archivos bajo el alfabeto español

# Pais por Confianza en la Policia 

sjPlot::sjt.xtab(var.row = data_limpia_categorica$Pais, # Variable independiente
                 var.col = data_limpia_categorica$conf_policia, # Variable dependiente
                 show.summary = F, # para que no enseñe información del tipo summary
                 emph.total = T, # para destacar visualmente los totales de la fila como suma
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T, # porcentaje columna
                 encoding= "UTF-8") # para que lea los archivos bajo el alfabeto español 

# Pais por Confiaza en los Medios de Comunicacion 

sjPlot::sjt.xtab(var.row = data_limpia_categorica$Pais, # Variable independiente 
                 var.col = data_limpia_categorica$conf_medios, # Variable dependiente
                 show.summary = F,  # para que no enseñe información del tipo summary
                 emph.total = T, # para destacar visualmente los totales de la fila como suma
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T, # porcentaje columna
                 encoding= "UTF-8") # para que lea los archivos bajo el alfabeto español

# Pais por Satisfaccion con la democracia 

sjPlot::sjt.xtab(var.row = data_limpia_categorica$Pais, # Variable independiente 
                 var.col = data_limpia_categorica$satisf_democracia, # Variable dependiente
                 show.summary = F,  # para que no enseñe información del tipo summary
                 emph.total = T, # para destacar visualmente los totales de la fila como suma
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T, # porcentaje columna
                 encoding= "UTF-8") # para que lea los archivos bajo el alfabeto español



# 2. Prueba de hipotesis con Chi-Cuadrado #

# Prueba de hipotesis con Chi-cuadrado 1: Variable independiente Pais y variable dependiente conf_gobierno

chi_result_1 <- chisq.test(table(data_limpia_categorica$Pais, data_limpia$conf_gobierno))

stats.table_1 <- tidy(chi_result_1, conf_int = T)

nice_table(stats.table_1)

# Prueba de hipotesis con Chi-cuadrado 2: Variable independiente Pais y variable dependiente conf_poder_judicial

chi_result_2 <- chisq.test(table(data_limpia_categorica$Pais, data_limpia$conf_poder_judicial))

stats.table_2 <- tidy(chi_result_2)

nice_table(stats.table_2)

# Prueba de hipotesis con Chi-cuadrado 3: Variable independiente Pais y variable dependiente conf_partidos

chi_result_3 <- chisq.test(table(data_limpia_categorica$Pais, data_limpia$conf_partidos))

stats.table_3 <- tidy(chi_result_3)

nice_table(stats.table_3)

# Prueba de hipotesis con Chi-cuadrado 4: Variable independiente Pais y variable dependiente conf_policia

chi_result_4 <- chisq.test(table(data_limpia_categorica$Pais, data_limpia$conf_policia))

stats.table_4 <- tidy(chi_result_4)

nice_table(stats.table_4)

# Prueba de hipotesis con Chi-cuadrado 5: Variable independiente Pais y variable dependiente conf_medios

chi_result_5 <- chisq.test(table(data_limpia_categorica$Pais, data_limpia$conf_medios))

stats.table_5 <- tidy(chi_result_5)

nice_table(stats.table_5)

# Prueba de hipotesis con Chi-cuadrado 6: Variable independiente Pais y variable dependiente satisf_democracia

chi_result_6 <- chisq.test(table(data_limpia_categorica$Pais, data_limpia$satisf_democracia))

stats.table_6 <- tidy(chi_result_6)

nice_table(stats.table_6)


# 3. Análisis de correspondencia múltiple

ACM <- data_limpia_categorica %>% 
  dplyr::select(conf_gobierno, conf_poder_judicial, conf_partidos,
                conf_policia, conf_medios, satisf_democracia, Pais) %>%
  na.omit() %>%
  as.data.frame()

ACM <- FactoMineR::MCA(datos_mca, graph = FALSE)

# 4. Eigen values / Varianza

eig_val <- factoextra::get_eigenvalue(ACM)
head(eig_val, 10)

fviz_screeplot(ACM, addlabels = TRUE)


# 4. Grafico de las Dispersiones 

fviz_mca_var(ACM, #objeto tipo lista con resultados mca
             col.var = "contrib", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07", "#458B74", "#104E8B", "#FFFF00"), #definición de la paleta de colores
             repel = TRUE, # evitar solapamientos de etiquetas,
             max.overlaps = "ggrepel.max.overlaps", #aumentar el tamaño de solapamientos
             ggtheme = theme_minimal())

