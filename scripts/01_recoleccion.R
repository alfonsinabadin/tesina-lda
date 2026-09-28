# ==============================================================================
# 01_recoleccion.R
# Descarga de comentarios de YouTube mediante la API (paquete tuber)
# ------------------------------------------------------------------------------
# Entrada : docs/data/raw/videos.xlsx
#           Listado de videos publicados entre el 01/01/2023 y el 21/12/2024 por
#           los ocho canales, obtenido con la API de YouTube desde Google Sheets
#           (Apps Script). Columnas: channel_id, canal, video_id, url, segundos,
#           comentarios_api, etc.
# Salida  : docs/data/raw/comentarios_<canal>.xlsx (un archivo por canal)
#
# NOTA: la descarga se realizó en varias etapas por las cuotas de la API y se
# conservaron los comentarios publicados hasta el 05/09/2025. Volver a ejecutar
# este script hoy devolverá un conjunto de comentarios distinto al analizado.
# Requiere credenciales OAuth de Google Cloud con la YouTube Data API v3 activa.
# ==============================================================================

library(here)
library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(janitor)
library(tuber)

# Credenciales
yt_oauth(
  app_id = Sys.getenv("YT_APP_ID"),
  app_secret = Sys.getenv("YT_APP_SECRET"),
  token = ""
)

# Videos de más de diez minutos (se excluyen shorts y fragmentos) --------------
videos <- read_excel(here("docs", "data", "raw", "videos.xlsx")) %>%
  distinct(video_id, .keep_all = TRUE) %>%
  filter(segundos > 600)

# Descarga de comentarios --------------------------------------------------------
descargar_comentarios <- function(id) {
  tryCatch(
    get_all_comments(video_id = id),
    error = function(e) {
      message("Error en el video ", id, ": ", conditionMessage(e))
      NULL
    }
  )
}

for (cn in unique(videos$canal)) {
  ids <- videos %>% filter(canal == cn) %>% pull(video_id)
  comentarios_canal <- map(ids, descargar_comentarios) %>% bind_rows()
  archivo <- paste0("comentarios_", make_clean_names(cn), ".xlsx")
  write_xlsx(comentarios_canal, here("docs", "data", "raw", archivo))
  message(cn, ": ", nrow(comentarios_canal), " comentarios")
}
