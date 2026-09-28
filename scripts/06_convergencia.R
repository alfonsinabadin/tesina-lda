# ==============================================================================
# 06_convergencia.R
# Diagnóstico de convergencia del muestreo de Gibbs (Anexo de la tesis)
# ------------------------------------------------------------------------------
# Entrada : docs/data/dtm.rds, docs/data/modelos/lda_*.rds
# Salida  : docs/data/convergencia/cadena_<configuracion>_<cadena>.rds
#           docs/data/convergencia/diagnosticos.rds
#
# Para cada configuración se estiman 3 cadenas (semillas 14062001, 2024 y 777)
# con 1000 iteraciones de burn-in + 3000 iteraciones, registrando la
# log-verosimilitud cada 10 iteraciones. Se calculan:
#   - R-hat de Gelman-Rubin y tamaño efectivo de muestra sobre la log-verosimilitud
#   - similitud coseno entre tópicos de distintas cadenas (emparejados con el
#     algoritmo húngaro) y con el modelo usado en la tesis.
# No modifica los modelos de la tesis. Demora aprox. 1 h 40 min con 5 núcleos.
# ==============================================================================

library(here)
log_msg <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), ..., "\n"); flush.console() }
suppressPackageStartupMessages({ library(topicmodels); library(parallel); library(dplyr); library(tibble); library(tidyr) })
log_msg("Inicio")
dtm_train <- readRDS(here("docs", "data", "dtm.rds"))$dtm_train
dir_conv <- here("docs", "data", "convergencia")
dir.create(dir_conv, showWarnings = FALSE, recursive = TRUE)

configs <- tibble(
  modelo = c("k4_a1_b1","k8_a1_b1","k12_a1_b1","k4_a01_b01","k8_a01_b01","k12_a01_b01","k4_ak_b01","k8_ak_b01","k12_ak_b01"),
  K      = c(4, 8, 12, 4, 8, 12, 4, 8, 12),
  alpha  = c(1, 1, 1, 0.1, 0.1, 0.1, 50/4, 50/8, 50/12),
  delta  = c(1, 1, 1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
)
semillas <- c(14062001, 2024, 777)
trabajos <- expand_grid(configs, cadena = 1:3) %>% mutate(seed = semillas[cadena])

correr <- function(i) {
  tr <- trabajos[i, ]
  out <- file.path(dir_conv, sprintf("cadena_%s_%d.rds", tr$modelo, tr$cadena))
  if (file.exists(out)) return(paste(basename(out), "ya existia"))
  t0 <- Sys.time()
  m <- LDA(dtm_train, k = tr$K, method = "Gibbs",
           control = list(alpha = tr$alpha, delta = tr$delta, seed = tr$seed,
                          burnin = 1000, iter = 3000, thin = 3000, keep = 10, best = TRUE))
  saveRDS(list(modelo = tr$modelo, cadena = tr$cadena, seed = tr$seed,
               logLiks = m@logLiks, beta = m@beta, terms = m@terms), out)
  paste(basename(out), "listo en", round(difftime(Sys.time(), t0, units = "mins"), 1), "min")
}

# ---- Diagnosticos ----
coseno <- function(a, b) { a <- exp(a); b <- exp(b); (a %*% t(b)) / (sqrt(rowSums(a^2)) %o% sqrt(rowSums(b^2))) }
emparejar <- function(S) { asig <- clue::solve_LSAP(S, maximum = TRUE); S[cbind(seq_len(nrow(S)), as.integer(asig))] }
resumen <- list(); trazas <- list()
for (mo in configs$modelo) {
  cad <- lapply(1:3, function(c) readRDS(file.path(dir_conv, sprintf("cadena_%s_%d.rds", mo, c))))
  ll <- lapply(cad, `[[`, "logLiks")
  n <- min(lengths(ll)); it <- 4000 - (n - seq_len(n)) * 10; ll <- lapply(ll, function(x) tail(x, n))
  trazas[[mo]] <- bind_rows(lapply(1:3, function(c) tibble(modelo = mo, cadena = c, iter = it, logLik = ll[[c]][1:n])))
  post <- it > 1000
  rhat <- coda::gelman.diag(coda::mcmc.list(lapply(ll, function(x) coda::mcmc(x[1:n][post]))), autoburnin = FALSE)$psrf[1, 1]
  ess  <- sum(sapply(ll, function(x) coda::effectiveSize(x[1:n][post])))
  sims <- c(emparejar(coseno(cad[[1]]$beta, cad[[2]]$beta)), emparejar(coseno(cad[[1]]$beta, cad[[3]]$beta)),
            emparejar(coseno(cad[[2]]$beta, cad[[3]]$beta)))
  tesis <- readRDS(here("docs", "data", "modelos", paste0("lda_", mo, ".rds")))
  stopifnot(identical(tesis@terms, cad[[1]]$terms))
  sim_tesis <- unlist(lapply(cad, function(cc) emparejar(coseno(tesis@beta, cc$beta)))); rm(tesis)
  resumen[[mo]] <- tibble(modelo = mo, n_traza = n, rhat_loglik = rhat, ess_loglik = ess,
                          sim_cadenas_media = mean(sims), sim_cadenas_min = min(sims),
                          sim_tesis_media = mean(sim_tesis), sim_tesis_min = min(sim_tesis))
  log_msg(mo, "Rhat =", round(rhat, 3), "| sim media =", round(mean(sims), 3), "| sim tesis =", round(mean(sim_tesis), 3))
}
resumen <- bind_rows(resumen); trazas <- bind_rows(trazas)
saveRDS(list(resumen = resumen, trazas = trazas, semillas = semillas,
             version_R = R.version.string, version_topicmodels = as.character(packageVersion("topicmodels"))),
        file.path(dir_conv, "diagnosticos.rds"))
print(resumen, width = 200)
log_msg("FIN")
