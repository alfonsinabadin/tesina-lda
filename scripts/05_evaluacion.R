# ==============================================================================
# 05_evaluacion.R
# Perplejidad (entrenamiento y evaluación) y coherencia UMass de cada modelo
# ------------------------------------------------------------------------------
# Entrada : docs/data/dtm.rds, docs/data/modelos/lda_*.rds
# Salida  : docs/data/resultados/evaluacion_<configuracion>.rds
#           docs/data/resultados/resumen_evaluaciones.rds
# ==============================================================================

library(here)
library(dplyr)
library(tibble)
library(topicmodels)
library(slam)

dtm <- readRDS(here("docs", "data", "dtm.rds"))
dtm_train <- dtm$dtm_train
dtm_test_eval <- dtm$dtm_test[row_sums(dtm$dtm_test) > 0, ]

dir_resultados <- here("docs", "data", "resultados")
dir.create(dir_resultados, showWarnings = FALSE, recursive = TRUE)

modelos <- c(
  "k4_a1_b1",   "k8_a1_b1",   "k12_a1_b1",
  "k4_a01_b01", "k8_a01_b01", "k12_a01_b01",
  "k4_ak_b01",  "k8_ak_b01",  "k12_ak_b01"
)

# Matriz binaria (presencia/ausencia) para contar coocurrencias por documento
dtm_binaria <- dtm_train
dtm_binaria$v[] <- 1
n_top_words <- 10

evaluar_modelo <- function(nombre) {
  modelo <- readRDS(here("docs", "data", "modelos", paste0("lda_", nombre, ".rds")))
  K <- modelo@k

  # 1. Perplejidad
  perp <- function(datos) tryCatch(perplexity(modelo, newdata = datos), error = function(e) NA_real_)
  tabla_perplejidad <- tibble(
    Muestra = c("Entrenamiento", "Validación (Test)"),
    Perplejidad = c(perp(dtm_train), perp(dtm_test_eval))
  )

  # 2. Coherencia UMass sobre las 10 palabras principales de cada tópico
  top_words <- apply(modelo@beta, 1, function(x) modelo@terms[order(x, decreasing = TRUE)[1:n_top_words]])

  coherencias <- sapply(seq_len(K), function(k) {
    sub_dtm <- dtm_binaria[, top_words[, k]]
    D_j  <- col_sums(sub_dtm)
    D_ij <- as.matrix(crossprod_simple_triplet_matrix(sub_dtm))
    score <- 0
    for (i in 2:n_top_words) for (j in 1:(i - 1)) score <- score + log((D_ij[i, j] + 1) / D_j[j])
    score / choose(n_top_words, 2)
  })

  tabla_coherencia <- tibble(Topico = paste("Tópico", seq_len(K)), Coherencia = coherencias)

  saveRDS(
    list(perplejidad = tabla_perplejidad, coherencia = tabla_coherencia),
    file.path(dir_resultados, paste0("evaluacion_", nombre, ".rds"))
  )

  tibble(
    modelo = nombre, K = K, filas_coherencia = K,
    perp_train = tabla_perplejidad$Perplejidad[1],
    perp_test = tabla_perplejidad$Perplejidad[2],
    coherencia_media = mean(coherencias)
  )
}

resumen_evaluaciones <- bind_rows(lapply(modelos, evaluar_modelo))
saveRDS(resumen_evaluaciones, file.path(dir_resultados, "resumen_evaluaciones.rds"))
print(resumen_evaluaciones)
