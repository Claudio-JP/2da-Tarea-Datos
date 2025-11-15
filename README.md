## Tarea Análisis de Datos II
### Que hace popular una canción?

¿Que es lo que hace que una canción sea exitosa? será su ritmo? su melodía? cuan baliable es la canción? estas y más preguntas me surgieron cuando encontré la base de datos en keggle, que une información entregada por las API de Spotify y Youtube sobre más de 15.000 canciones seleccionadas. Por tanto, en este trabajo se buscará realizar un breve análisis sobre la relación entre las características de una canción y su éxito/popularidad.

Para esto, en primer lugar, se importan los paquetes que serán relevantes a lo largo del trabajo
```{r importación paquetes}
library(tidyverse)
library(readxl)
library(writexl)
library(ggcorrplot)
```
En segundo lugar, se importa la base de datos
```{r descarga base de datos relevante}
base_sp_yt <- read_csv("Spotify_Youtube.csv")
```

Posteriormente, se seleccionan varias variables que podrían ser relevantes para el análisis planteado, y se hacen ajustes varios de formato y estandarización
```{r selección y limpieza de variables relevantes}

#se seleccionan las variables a analizar
base_sp_yt_select <- base_sp_yt %>% select(Track, Artist, Album_type, Danceability, Energy, Loudness, Valence, Tempo, Duration_ms, Stream, Views, Likes, Comments)
```
* Track es la canción; Artist el artista principal; el tipo de album indica si es un conjunto de canciones o un single; Danceability es un valor numérico determinado por Spotify que indica cuan bailable es una canción, medida del 0 al 1; Energy es una medida, también determinada por Spotify, que indica el nivel perceptual de intensidad, con una escala de 0 a 1; Loudness mide los decibeles promedio de una canción; Valence es una medida de operacionalización de la "positividad" o alegría de una canción, también se escala del 0 al 1; Tempo es la medición de los bpm de la canción; Duration_ms es el largo de la canción medido en milisegundos; Stream se refiere al conteo de las reproducciones de la canción en Spotify; Views refiere a la cantidad de visualizaciones de la respectiva canción en youtube; Likes y Comments refieren, respectivamente, a la cantidad de likes y comentarios que recibió el video con la canción en youtube.
```
#Se elimina la base original en pos del orden del ambiente con el que se trabaja
rm(base_sp_yt)

#Se estandarizan los nombres de las variables a lower case y a español
base_sp_yt_select <- base_sp_yt_select %>% rename(cancion = Track,
                                                  autor = Artist,
                                                  tipo_album = Album_type,
                                                  bailable = Danceability,
                                                  intensidad = Energy,
                                                  decibeles = Loudness,
                                                  positividad = Valence,
                                                  tempo = Tempo,
                                                  duracion = Duration_ms,
                                                  reprod_spt = Stream,
                                                  visualizaciones = Views,
                                                  likes = Likes,
                                                  comentarios = Comments)

#La duración de las canciones está medida en milisegundos. Para facilitar la comprensión de los análisis, se transformará a segundos

base_sp_yt_select <- base_sp_yt_select %>% mutate(duracion = duracion / 1000)
```

Con la base ya más ordenada, se procede a realizar algunos cálculos de correlación de interés. Se utilizará likes y reproducciones como variables de operacionalización para la popularidad/éxito. Visualizaciones en youtube no se utiliza porque la variable likes ya funge como variable de operacionalización desde youtube.

```{r cálculos iniciales de correlación entre variables de interés}

cor(base_sp_yt_select$positividad, base_sp_yt_select$likes, use = "complete.obs")
# 0.01186376
cor(base_sp_yt_select$positividad, base_sp_yt_select$reprod_spt, use = "complete.obs")
# -0.01210929
```
Como se puede observar, pareciera existir una muy baja relación de afectación entre la positividad de una canción y las reproducciones en spotify o los likes que la misma recibe en Youtube. Más extraño aún, y si bien el valor es bajo, pareciera existir una correlación negativa entre la positividad de la canción y la cantidad de reproducciones en Spotify de la misma. Esto nos indicaría que la positividad de una canción no afectaría su popularidad

```
cor(base_sp_yt_select$intensidad, base_sp_yt_select$likes, use = "complete.obs")
# 0.06282433
cor(base_sp_yt_select$intensidad, base_sp_yt_select$reprod_spt, use = "complete.obs")
# 0.04423912
```
Extrañamente, la intensidad de una canción pareciera tampoco afectar en mayor medida la cantidad de reproducciones o likes que recibe la canción respectiva. Si bien presentan una relación más fuerte que la positividad, y en ambos casos sería una relación de influencia positiva, los valores son relativamente bajos, con lo que nuevamente se podría plantear que la intensidad de una canción pareciera no influir en su popularidad/éxito

```  
cor(base_sp_yt_select$bailable, base_sp_yt_select$reprod_spt, use = "complete.obs")
# 0.07337514
cor(base_sp_yt_select$bailable, base_sp_yt_select$likes, use = "complete.obs")
# 0.09939559
```
Nuevamente, el resultado es inesperado. Si bien la relación de influencia de la "bailabilidad" de una canción para con su éxito pareciera ser la más fuerte de las relaciones hasta ahora vistas, los valores todavía resaltan por su bajo valor, lo que llama la atención con respecto a qué, exactamente, devendría en que una canción sea popular o no.

---O---

A partir de lo anterior, y con tal de realizar un análisis más completo y holístico, se procede a construir un heatmap de correlaciones para todas las variables. Esto es, un cuadro que ilustre la correlación entre cada una de las variables en la base de datos. Empero, se crea en primer lugar una base nueva, pero sin las variables de cancion, autor y tipo de album, por ser de tipo character

```{r heatmap de correlaciones}
base_cor <- base_sp_yt_select %>% select(-cancion, -autor, -tipo_album)

base_cor_data <- cor(base_cor, use = "complete.obs")

ggcorrplot(
  base_cor_data,
  hc.order = TRUE,       # ordena variables por clustering
  type = "lower",        # solo mitad inferior
  lab = TRUE,            # muestra los valores
  lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726")  # azul → blanco → rojo)
```

<img width="875" height="540" alt="image" src="https://github.com/user-attachments/assets/b22558e3-35de-49fc-b0dc-f3aa7c1243bc" />

Estos resultados estarían evidenciando una dinámica poco esperada al inicio de la investigación: una baja relación general entre las características de una canción y su éxito a través de múltiples plataformas. Debido a esto, se construye otro mapa, pero utilizando una correlación bajo el método spearman, buscando analizar desde otros ángulos la información y encontrar posibles ejes explicativos para lo observado.


```{r Spearson Headmap}
base_cor_sp <- cor(base_cor, use = "complete.obs", method = "spearman")

ggcorrplot(
  base_cor_sp,
  hc.order = TRUE,       # ordena variables por clustering
  type = "lower",        # solo mitad inferior
  lab = TRUE,            # muestra los valores
  lab_size = 3,
  colors = c("#6D9EC1", "white", "#E46726")  # azul → blanco → rojo)
```
<img width="875" height="540" alt="image" src="https://github.com/user-attachments/assets/c835b52f-dc23-460d-8530-b467a5da0374" />

Los resultados observados son prácticamente iguales, indicando que la relación entre las variables se caracterizan por ser lineales y monótonas, sin grandes outliers. Además, nos ilustra que el análisis obtenido previamente pareciera ser correcto: las características propias de una canción parecieran no afectar su popularidad/éxito. Esto pareciera indicar que son otras variables que afectarían el éxito de una canción, como la popularidad del cantante, el marketing y publicidad, entre otros. Sin embargo, la presente base de datos no nos permite estudiar este punto.

Sin embargo, se procede a construir otro gráfico que pudiese ser de utilidad para analizar la relación características - éxito de una canción. Se realizará un gráfico que permitirá observar al top 10 de canciones en ambas plataformas, sumando por cada canción, las columnas de visualización de youtube y reproducciones de spotify. A partir de esta tabla, se observará el valor que toma cada característica para estas canciones, con tal de evaluar si hay algún patrón o característica que resalte en las canciones con más reproducciones en ambas plataformas.


```{r obtención del top 10}
top_10 <- base_sp_yt_select %>% mutate(suma = visualizaciones + reprod_spt)

top_10 <- top_10 %>% arrange(desc(suma)) %>% head(10)
  
# se observan canciones repetidas, por lo que se vuelve a trabajar sobre la base original, eliminando las repeticiones, antes de ordenar y cortar nuevamente

base_sp_yt_select <- base_sp_yt_select %>% distinct(cancion, .keep_all = T)

top_10 <- base_sp_yt_select %>% mutate(suma = visualizaciones + reprod_spt)

top_10 <- top_10 %>% arrange(desc(suma)) %>% head(10)
```
---O---

```{r construcción del gráfico}

top10_2 <- top_10 %>%
  select(cancion, bailable, positividad, intensidad) %>% 
  pivot_longer(
    cols = -cancion,
    names_to = "caracteristica",
    values_to = "valor"
  )

ggplot(top10_2, aes(x = reorder(cancion, valor), y = valor, fill = caracteristica)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Características de las top 10 canciones cross platform",
    x = "Canción",
    y = "Valor de la característica"
  ) +
  theme_minimal()
```
<img width="875" height="540" alt="image" src="https://github.com/user-attachments/assets/bef09191-e828-4571-a59e-4afdff33509e" />

--

En pos de facilitar el análisis, también se hace un cálculo de los promedios y desviación estándar de cada característica


```
options(scipen = 999)

tabla_res <- skimr::skim(top_10)

#se ordena la tabla en pos de claridad, eliminando las columnas no relevantes y eliminando filas con valores que no son de interés
tabla_res <- tabla_res %>% select(-skim_type, -n_missing, -complete_rate, -character.min, -character.max, -character.empty, -character.n_unique, -character.whitespace)

tabla_res <- tabla_res[-(1:3),]
```
<img width="641" height="299" alt="Captura de pantalla (196)" src="https://github.com/user-attachments/assets/fb2c7925-0ee3-4707-abd5-36f80e409074" />

A partir del gráfico y la tabla, son varias las conclusiones relevantes que pueden obtenerse: En primer lugar, es posible percibir que, para que una canción sea exitosa, requiere de niveles de bailabilidad e intensidad mayores, en tanto que el promedio de ambas características para el top 10 es superior al 0,5. Además, la variable de bailabilidad pareciera ser un poco más estable que la intensidad, experimentando una desviación estándar ligeramente menor. En segundo lugar, si bien la variable positividad también pareciera "requerir" de niveles por sobre el promedio técnico (0.5) para que una canción sea exitosa, una desviación estándar más amplia indicaría que la positividad de una canción no sería tan necesaria en cuanto a ser de altos niveles para que una canción sea popular. En tercer lugar, la desviación estándar de la variable tiempo nos estaría indicando que las canciones se pueden permitir una mayor variabilidad en cuanto a esta característica, en tanto que las top 10 canciones no se caracterizarían por cierto nivel más específico de un tempo determinado.
Así, a partir del trabajo realizado, es posible percibir que, si bien las características técnicas de una canción parecieran no tener relación con el éxito de la misma en plataformas de "streaming musical" como spotify y youtube, si es cierto que las canciones más populares parecieran distingirse, entre otras cosas, por ser relativamente bailables e intensas, mientras que la positividad no pareciera ser un factor tan distintivo, y el tempo pareciera permitir un nivel de variabilidad mucho mayor.
Ciertamente, este eje de investigación requiere de un trabajo mucho más profundo. A fin de cuentas, no logramos responder la pregunta "¿qué hace que una canción sea exitosa/popular?". Sin embargo, logramos ilustrar ciertas dinámicas relevantes al respecto, que permitirán avanzar en investigaciones posteriores.
