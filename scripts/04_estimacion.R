# ==============================================================================
# 04_estimacion.R
# Estimación de las nueve configuraciones de LDA (muestreo de Gibbs colapsado)
# ------------------------------------------------------------------------------
# Entrada : docs/data/dtm.rds
# Salida  : docs/data/modelos/lda_<configuracion>.rds
#
# Configuraciones: K = 4, 8, 12 combinadas con
#   - referencia       : alpha = 1,    beta = 1    (a1_b1)
#   - alta dispersión  : alpha = 0.1,  beta = 0.1  (a01_b01)
#   - heurística G&S   : alpha = 50/K, beta = 0.1  (ak_b01)
# En topicmodels el hiperparámetro beta se denomina `delta`.
# Cada modelo demora entre 15 y 20 minutos; se estiman en paralelo.
# ==============================================================================

library(here)
library(topicmodels)
library(parallel)

dtm_train <- readRDS(here("docs", "data", "dtm.rds"))$dtm_train
dir_modelos <- here("docs", "data", "modelos")
dir.create(dir_modelos, showWarnings = FALSE, recursive = TRUE)

configuraciones <- list(
  list(nombre = "k4_a1_b1",    K = 4,  alpha = 1,     delta = 1),
  list(nombre = "k8_a1_b1",    K = 8,  alpha = 1,     delta = 1),
  list(nombre = "k12_a1_b1",   K = 12, alpha = 1,     delta = 1),
  list(nombre = "k4_a01_b01",  K = 4,  alpha = 0.1,   delta = 0.1),
  list(nombre = "k8_a01_b01",  K = 8,  alpha = 0.1,   delta = 0.1),
  list(nombre = "k12_a01_b01", K = 12, alpha = 0.1,   delta = 0.1),
  list(nombre = "k4_ak_b01",   K = 4,  alpha = 50/4,  delta = 0.1),
  list(nombre = "k8_ak_b01",   K = 8,  alpha = 50/8,  delta = 0.1),
  list(nombre = "k12_ak_b01",  K = 12, alpha = 50/12, delta = 0.1)
)

estimar <- function(cfg) {
  modelo <- LDA(
    dtm_train,
    k = cfg$K,
    method = "Gibbs",
    control = list(
      alpha  = cfg$alpha,
      delta  = cfg$delta,
      seed   = 14062001,
      iter   = 3000,
      burnin = 1000,
      thin   = 100
    )
  )
  saveRDS(modelo, file.path(dir_modelos, paste0("lda_", cfg$nombre, ".rds")))
  cfg$nombre
}

listos <- mclapply(configuraciones, estimar, mc.cores = 4, mc.preschedule = FALSE)
print(unlist(listos))
