# ==============================================================================
# 03_dtm.R
# Limpieza residual, partición entrenamiento/evaluación y matrices documento-término
# ------------------------------------------------------------------------------
# Entrada : docs/data/processed/comentarios_procesado_{1,2}.xlsx
#           docs/data/processed/correcciones_1.xlsx
# Salida  : docs/data/dtm.rds  (lista con dtm_train y dtm_test)
#
# Reproduce exactamente las matrices usadas en la tesis
# (dtm_train: 1.028.406 x 37.009; dtm_test: 257.097 x 36.472).
# ==============================================================================

library(here)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidytext)
library(tm)

dir_proc <- here("docs", "data", "processed")

comentarios_raw <- bind_rows(
  read_excel(file.path(dir_proc, "comentarios_procesado_1.xlsx")),
  read_excel(file.path(dir_proc, "comentarios_procesado_2.xlsx"))
) %>%
  distinct()

correcciones <- read_excel(file.path(dir_proc, "correcciones_1.xlsx"))

# 1. Limpieza residual -----------------------------------------------------------

patron_ruido <- "\\b(uckszu|wh|gy|mb|dv|ujg|hcmfy|xsify|jpgfy)\\b"

comentarios <- comentarios_raw %>%
  mutate(
    # Entidades y etiquetas HTML
    correccion = str_remove_all(correccion, regex("&[a-z]+;|<[^>]+>", ignore_case = TRUE)),
    correccion = str_remove_all(correccion, regex("\\b(quot|amp|br|lt|gt|nbsp|na)\\b", ignore_case = TRUE)),
    # Normalización de la risa escrita
    correccion = str_replace_all(correccion, regex("(?i)\\b[jaskh]{3,}\\b"), "risa"),
    correccion = str_replace_all(correccion, regex("\\bq\\b", ignore_case = TRUE), ""),
    # Ruido de procesamiento y tokens de un carácter
    correccion = str_remove_all(correccion, regex(patron_ruido, ignore_case = TRUE)),
    correccion = str_replace_all(correccion, "\\b[a-zA-Z]\\b", ""),
    correccion = str_squish(correccion)
  ) %>%
  filter(!is.na(correccion), correccion != "", str_to_lower(correccion) != "na")

# 2. Tokens anómalos (más de 10 apariciones, ausentes del texto original y de
#    las correcciones) --------------------------------------------------------------

tokens_vocabulario <- comentarios %>%
  unnest_tokens(word, correccion) %>%
  filter(nchar(word) > 1) %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)

tokens_originales <- comentarios_raw %>%
  unnest_tokens(word, textOriginal) %>%
  distinct(word)

tokens_a_eliminar <- tokens_vocabulario %>%
  anti_join(tokens_originales, by = "word") %>%
  anti_join(correcciones %>% distinct(correccion) %>% rename(word = correccion), by = "word") %>%
  pull(word)

comentarios <- comentarios %>%
  mutate(
    correccion = map_chr(
      str_split(correccion, "\\s+"),
      ~ paste(.x[!.x %in% tokens_a_eliminar], collapse = " ")
    ),
    correccion = str_squish(correccion)
  ) %>%
  filter(!is.na(correccion), correccion != "")

# 3. Partición 80/20 ----------------------------------------------------------------

set.seed(14062001)
train_idx <- sample(1:nrow(comentarios), 0.8 * nrow(comentarios))
train_data <- comentarios[train_idx, ]
test_data  <- comentarios[-train_idx, ]

# 4. Vocabulario común (más de un carácter y más de 10 apariciones) ---------------

vocabulario_comun <- comentarios %>%
  unnest_tokens(word, correccion) %>%
  filter(nchar(word) > 1) %>%
  count(word) %>%
  filter(n > 10) %>%
  pull(word)

# 5. Matrices documento-término -------------------------------------------------------

construir_dtm <- function(datos) {
  datos %>%
    unnest_tokens(word, correccion) %>%
    filter(word %in% vocabulario_comun) %>%
    count(id, word) %>%
    cast_dtm(id, word, n)
}

dtm_train <- construir_dtm(train_data)
dtm_test  <- construir_dtm(test_data)

message("dtm_train: ", nrow(dtm_train), " x ", ncol(dtm_train))
message("dtm_test : ", nrow(dtm_test), " x ", ncol(dtm_test))

saveRDS(list(dtm_train = dtm_train, dtm_test = dtm_test), here("docs", "data", "dtm.rds"))
