# ==============================================================================
# 02_limpieza.R
# Normalización del texto y corrección ortográfica
# ------------------------------------------------------------------------------
# Entrada : docs/data/raw/comentarios_*.xlsx
#           docs/data/dictionaries/kaikki_es.jsonl  (diccionario de Kaikki.org,
#           https://kaikki.org/dictionary/Spanish/, basado en Wiktionary)
# Salida  : docs/data/dictionaries/diccionario_total.rds
#           docs/data/processed/comentarios_procesado_1.xlsx
#           docs/data/processed/comentarios_procesado_2.xlsx
#           docs/data/processed/correcciones_1.xlsx
#
# NOTA: la corrección recorre más de un millón de comentarios y demora varias
# horas; en la tesis se ejecutó por tramos.
# ==============================================================================

library(here)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(stopwords)
library(tm)
library(jsonlite)
library(stringdist)

dir_raw  <- here("docs", "data", "raw")
dir_proc <- here("docs", "data", "processed")
dir_dic  <- here("docs", "data", "dictionaries")

# 1. Carga y normalización -------------------------------------------------------

archivos <- list.files(dir_raw, pattern = "^comentarios_.*\\.xlsx$", full.names = TRUE)

comentarios <- bind_rows(lapply(archivos, read_excel)) %>%
  filter(publishedAt <= as.Date("05/09/2025", "%d/%m/%Y")) %>%
  distinct() %>%
  mutate(
    # 0. Eliminar enlaces (http, https, www, href, etc.)
    textDisplay = str_remove_all(textDisplay, "https?://\\S+|www\\.\\S+"),
    textDisplay = str_remove_all(textDisplay, "href\\s*"),

    # 1. Minúsculas
    textDisplay = tolower(textDisplay),

    # 2. Colapso de letras repetidas (amooo -> amo, encantaaa -> encanta)
    textDisplay = gsub("([a-záéíóúñ])\\1+", "\\1", textDisplay),

    # 3. Filtrado de caracteres (solo letras, tildes y ñ)
    textDisplay = gsub("[^a-záéíóúñü ]", " ", textDisplay),

    # 4. Normalización de espacios
    textDisplay = str_squish(textDisplay),

    # 5. Eliminación de stopwords en español
    textDisplay = str_squish(removeWords(textDisplay, stopwords("es")))
  )

# 2. Diccionario del corpus (20.000 palabras más frecuentes) ---------------------

palabras <- str_split(comentarios$textDisplay, " ") %>%
  unlist() %>%
  subset(. != "")

diccionario_propio <- sort(table(palabras), decreasing = TRUE)

diccionario_propio <- data.frame(
  Palabra = names(diccionario_propio),
  Frecuencia = as.integer(diccionario_propio),
  row.names = NULL
) %>%
  arrange(desc(Frecuencia)) %>%
  slice_head(n = 20000)

# 3. Diccionario externo (Kaikki.org) --------------------------------------------

con <- file(file.path(dir_dic, "kaikki_es.jsonl"), "r", encoding = "UTF-8")
diccionario_externo <- stream_in(con, verbose = FALSE)
close(con)

diccionario_externo <- data.frame(
  Palabra = unique(diccionario_externo$word),
  Frecuencia = 1,
  stringsAsFactors = FALSE
)

# 4. Diccionario total con bigramas de caracteres --------------------------------

bigramas <- function(palabra) {
  letras <- unlist(strsplit(palabra, ""))
  if (length(letras) < 2) return(palabra)
  paste0(letras[-length(letras)], letras[-1])
}

diccionario <- bind_rows(diccionario_propio, diccionario_externo) %>%
  group_by(Palabra) %>%
  summarise(Frecuencia = max(Frecuencia), .groups = "drop") %>%
  arrange(desc(Frecuencia)) %>%
  mutate(bigramas = lapply(Palabra, bigramas))

saveRDS(diccionario, file.path(dir_dic, "diccionario_total.rds"))

# 5. Corrección ortográfica (Damerau-Levenshtein, máximo 3 ediciones) ------------

elegir_mejor_candidato <- function(token, candidatos, umbral = 3L) {
  if (is.na(token) || token == "" || nrow(candidatos) == 0) return(token)

  d <- stringdist(token, candidatos$Palabra, method = "dl")
  idx <- which(d <= umbral)
  if (length(idx) == 0) return(token)  # sin candidato aceptable

  sub <- candidatos[idx, , drop = FALSE]
  dsub <- d[idx]

  # Distancia mínima y desempate por mayor frecuencia
  sub[dsub == min(dsub), , drop = FALSE] %>%
    arrange(desc(Frecuencia), Palabra) %>%
    slice(1) %>%
    pull(Palabra)
}

correcciones <- data.frame(token = character(), correccion = character(), frecuencia = numeric())
comentarios$correccion <- NA_character_

for (i in seq_len(nrow(comentarios))) {
  comentario <- comentarios$textDisplay[i]
  if (is.na(comentario)) next

  tokens <- unlist(strsplit(comentario, "\\s+"))
  palabras_fuera <- tokens[!tokens %in% diccionario$Palabra]
  palabras_correctas <- palabras_fuera

  if (length(palabras_fuera) == 0) {
    comentarios$correccion[i] <- comentario
    next
  }

  for (j in seq_along(palabras_fuera)) {
    token <- palabras_fuera[j]

    if (token %in% correcciones$token) {
      # Corrección ya calculada: se reutiliza
      k <- which(correcciones$token == token)
      correcciones$frecuencia[k] <- correcciones$frecuencia[k] + 1
      palabras_correctas[j] <- correcciones$correccion[k]
    } else {
      # Filtro previo: misma letra inicial y al menos dos bigramas en común
      bigramas_token <- bigramas(token)
      candidatos <- diccionario[
        substr(diccionario$Palabra, 1, 1) == substr(token, 1, 1) &
          sapply(diccionario$bigramas, function(bg) length(intersect(bg, bigramas_token)) >= 2),
      ]
      correccion <- elegir_mejor_candidato(token, candidatos, umbral = 3)
      correcciones <- rbind(correcciones, data.frame(token = token, correccion = correccion, frecuencia = 1))
      palabras_correctas[j] <- correccion
    }
  }

  comentarios$correccion[i] <- str_replace_all(comentario, setNames(palabras_correctas, palabras_fuera))
}

# 6. Guardado (en dos archivos por el límite de filas de Excel) ------------------

write_xlsx(comentarios[1:999999, ], file.path(dir_proc, "comentarios_procesado_1.xlsx"))
write_xlsx(comentarios[1000000:nrow(comentarios), ], file.path(dir_proc, "comentarios_procesado_2.xlsx"))
write_xlsx(correcciones, file.path(dir_proc, "correcciones_1.xlsx"))
