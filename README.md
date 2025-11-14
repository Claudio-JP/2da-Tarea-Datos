## Tarea Análisis de Datos II

```{r importación bases}

library(tidyverse)
library(readxl)
library(writexl)
```

```{r descarga base de datos relevante}
spotify_songs <- read_csv("tracks_features.csv")
```

```{r selección de variables relevantes}
spotify_songs_select <- spotify_songs %>% select()

```