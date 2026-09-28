# Análisis de comentarios en redes sociales con *latent Dirichlet allocation*

Tesina de grado de la Licenciatura en Estadística (Facultad de Ciencias Económicas y Estadística, Universidad Nacional de Rosario, 2026).

**Autora:** Alfonsina Badin - **Director:** Ignacio Evangelista

📄 **Informe final:** [`docs/informe.pdf`](docs/informe.pdf)

## Resumen

Se aplica *latent Dirichlet allocation* (LDA) a más de 1,3 millones de comentarios de YouTube publicados en videos de ocho canales argentinos de *streaming* (LuzuTV, Olga, Un Poco de Ruido, La Casa Streaming, Bondi Live, Vorterix, Urbana Play y Blender) entre 2023 y 2024. El trabajo desarrolla los fundamentos de estadística bayesiana, procesamiento de lenguaje natural y muestreo de Gibbs colapsado, y compara nueve configuraciones del modelo (K = 4, 8 y 12 tópicos con tres especificaciones de hiperparámetros) mediante perplejidad fuera de muestra, coherencia UMass e interpretación cualitativa. El modelo final (K = 8, α = β = 1) identifica tópicos asociados a comunidades específicas (LuzuTV, Vorterix), contenidos puntuales (entrevista a Cazzu, política) y formas generales de interacción (apoyo, consumo, componentes discursivos).

## Estructura del repositorio

```
tesina-lda/
├── README.md
├── CITATION.cff
├── requirements.txt          # paquetes de R necesarios
├── tesina-lda.Rproj          # proyecto de RStudio
├── scripts/                  
│   ├── 01_recoleccion.R      # descarga de comentarios con la API de YouTube (tuber)
│   ├── 02_limpieza.R         # normalización y corrección ortográfica
│   ├── 03_dtm.R              # limpieza residual, partición 80/20 y matrices documento-término
│   ├── 04_estimacion.R       # estimación de las nueve configuraciones de LDA
│   ├── 05_evaluacion.R       # perplejidad y coherencia
│   └── 06_convergencia.R     # diagnóstico de convergencia (Anexo)
└── docs/
    ├── informe.Rmd           
    ├── informe.pdf           
    ├── referencias.bib, apa.csl
    ├── figs/                 # figuras estáticas
    └── data/
        ├── raw/              # comentarios y videos descargados         (Release)
        ├── processed/        # comentarios normalizados y corregidos    (Release)
        ├── dictionaries/     # diccionarios para la corrección ortográfica
        ├── modelos/          # modelos LDA estimados (.rds)             (Release)
        ├── resultados/       # perplejidad y coherencia de cada modelo
        └── convergencia/     # diagnósticos de convergencia
```

## Datos

Los comentarios se obtuvieron con la **YouTube Data API v3**: el listado de videos se generó desde _Google Sheets_ (_Apps Script_) y los comentarios se descargaron desde R con el paquete `tuber`. Se consideraron los videos de más de diez minutos publicados entre el 1 de enero de 2023 y el 21 de diciembre de 2024, y los comentarios disponibles al 5 de septiembre de 2025.

Por su tamaño, las bases de datos y los modelos estimados no se incluyen en el
repositorio y se publican como archivos adjuntos en la sección [**Releases**](https://github.com/alfonsinabadin/tesina-lda/releases).

El diccionario externo utilizado en la corrección ortográfica proviene de
[Kaikki.org](https://kaikki.org/dictionary/Spanish/) (datos extraídos de Wiktionary)
y debe guardarse como `docs/data/dictionaries/kaikki_es.jsonl`.

## Reproducibilidad

Requisitos: R ≥ 4.4 y los paquetes listados en `requirements.txt`.

```r
install.packages(readLines("requirements.txt"))
```

Abrir `tesina-lda.Rproj` y ejecutar los scripts en orden:

| Paso | Script | Genera | Tiempo aprox. |
|---|---|---|---|
| 1 | `01_recoleccion.R` | `data/raw/comentarios_*.xlsx` | horas (cuotas de la API) |
| 2 | `02_limpieza.R` | `data/processed/*.xlsx` | varias horas |
| 3 | `03_dtm.R` | `data/dtm.rds` | minutos |
| 4 | `04_estimacion.R` | `data/modelos/lda_*.rds` | ~1 h (4 núcleos) |
| 5 | `05_evaluacion.R` | `data/resultados/*.rds` | ~1 h |
| 6 | `06_convergencia.R` | `data/convergencia/*.rds` | ~1 h 40 min (5 núcleos) |

Los pasos 1 y 2 pueden omitirse descargando las bases procesadas. Todas las estimaciones usan la semilla `14062001`.