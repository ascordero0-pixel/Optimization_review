############################################################
# STEP 4 — Final Corpus (Semantic analysis) — INDEPENDENT
# Input:  Step_3_final_corpus_M.RData  (98 records)
# Output: Step_4_final_corpus_84_fullcols.RData
#         Step_4_final_corpus_84_fullcols_TALL.csv
# Author: Antonio Sánchez Cordero
############################################################

rm(list = ls())
graphics.off()

library(bibliometrix)
library(dplyr)
library(stringr)
library(readr)

############################################################
# Step_4.0 — Freeze inputs
############################################################

src_csv   <- "Step_3_final_corpus.csv"
src_rdata <- "Step_3_final_corpus_M.RData"

step4_csv   <- "Step_4_input_corpus.csv"
step4_rdata <- "Step_4_input_corpus_M.RData"

stopifnot(file.exists(src_csv))
stopifnot(file.exists(src_rdata))

file.copy(from = src_csv,   to = step4_csv,   overwrite = TRUE)
file.copy(from = src_rdata, to = step4_rdata, overwrite = TRUE)

cat("Step 4 input created:", step4_csv,   "\n")
cat("Step 4 input created:", step4_rdata, "\n")

############################################################
# Step_4.1 — Load and validate bibliometrix object M
############################################################

obj_names <- load(step4_rdata)

if ("M" %in% obj_names) {
  M <- get("M")
} else if (length(obj_names) == 1) {
  M <- get(obj_names[1])
} else {
  stop(paste0("No suitable object found in ", step4_rdata,
              ". Found: ", paste(obj_names, collapse = ", ")))
}

M <- as.data.frame(M, stringsAsFactors = FALSE)
class(M) <- c("bibliometrixDB", "data.frame")
row.names(M) <- NULL

stopifnot(nrow(M) > 0)
cat("Rows loaded:", nrow(M), "| Cols:", ncol(M), "\n")

############################################################
# Step_4.2 — Exclusión manual por DOI (fuera de scope)
# Criterio: edificios enterrados u otra temática no relevante
# identificada tras revisión manual del corpus de 98
############################################################

dois_excluir <- c(
  "10.1007/s12273-023-1092-3",
  "10.3390/su15129168",
  "10.3390/buildings15091536",
  "10.23967/j.rimni.2025.10.56233",
  "10.3390/buildings14092785",
  "10.26437/ajar.v11i2.1080",
  "10.1016/j.apenergy.2017.09.032",
  "10.3390/buildings14040878",
  "10.1016/j.apenergy.2021.116954",
  "10.3390/su16020766",
  "10.1016/j.applthermaleng.2025.125572",
  "10.1016/j.buildenv.2024.111552",
  "10.1016/j.tust.2019.103065",
  "10.1108/ecam-01-2020-0075"
)

# Normalizar DOIs para comparación robusta
M <- M %>%
  mutate(DI_norm = str_squish(str_to_lower(as.character(DI))))

n_antes <- nrow(M)
coincidencias <- sum(M$DI_norm %in% str_to_lower(dois_excluir))
cat("DOIs a excluir encontrados en corpus:", coincidencias, "de",
    length(dois_excluir), "\n")

M <- M %>%
  filter(!DI_norm %in% str_to_lower(dois_excluir)) %>%
  select(-DI_norm)

row.names(M) <- NULL
cat("Registros eliminados:", n_antes - nrow(M), "\n")
cat("Registros tras exclusión:", nrow(M), "\n")

# Verificar tamaño esperado
stopifnot(nrow(M) == 84)
cat("✓ Corpus validado: 84 documentos para TALL\n")

############################################################
# Step_4.3 — Crear columna `text` (Título + Keywords + Abstract)
############################################################

clean_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- enc2utf8(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- str_replace_all(x, "[[:cntrl:]]+", " ")
  x <- str_squish(x)
  x
}

title_col <- if ("TI" %in% names(M)) M$TI else ""
abst_col  <- if ("AB" %in% names(M)) M$AB else ""
keyw_col  <- if ("DE" %in% names(M)) M$DE else
  if ("ID" %in% names(M)) M$ID else ""

M$text <- paste(
  clean_text(title_col),
  clean_text(keyw_col),
  clean_text(abst_col),
  sep = ", "
)

M$text <- str_replace_all(M$text, ";", ",")
M$text <- str_squish(M$text)
M$text <- str_to_lower(M$text)

# Restaurar acrónimos en mayúsculas
acronyms <- c(
  "iaq", "hvac", "co2", "pm2.5", "pm10", "voc", "tvoc",
  "mvhr", "nv", "dcv", "bems", "ems",
  "ep", "cfd", "ml", "ai",
  "ashrae", "iso", "en", "une",
  "wos", "scopus"
)

for (a in acronyms) {
  a_rx <- str_replace_all(a, "\\.", "\\\\.")
  M$text <- str_replace_all(
    M$text,
    regex(paste0("\\b", a_rx, "\\b")),
    toupper(a)
  )
}

M$text <- str_replace_all(
  M$text,
  regex("\\bco₂\\b", ignore_case = TRUE),
  "CO2"
)

# Mantener clase bibliometrix
M <- as.data.frame(M, stringsAsFactors = FALSE)
class(M) <- c("bibliometrixDB", "data.frame")
row.names(M) <- NULL

stopifnot("text" %in% names(M))
stopifnot(nrow(M) == 84)

############################################################
# Step_4.4 — Export RData (Biblioshiny ready)
############################################################

out_rdata <- "Step_4_final_corpus_84_fullcols.RData"
save(M, file = out_rdata)
cat("Saved RData:", out_rdata, "\n")

############################################################
# Step_4.5 — Export CSV para TALL
############################################################

out_csv_tall <- "Step_4_final_corpus_84_fullcols_TALL.csv"
write_delim(M, out_csv_tall, delim = ";", na = "", quote = "needed")
cat("Saved CSV (TALL):", out_csv_tall, "\n")

# Verificación de integridad del CSV
test <- read_delim(out_csv_tall, delim = ";", show_col_types = FALSE)
stopifnot("text" %in% names(test))
stopifnot(nrow(test) == 84)
cat("✓ CSV verificado:", nrow(test), "registros,",
    ncol(test), "columnas\n")

############################################################
# Step_4.6 — Resumen final del pipeline
############################################################

cat("\n========== RESUMEN PIPELINE ==========\n")
cat("Step 1 — Merge WoS + Scopus + DOI cleaning:  427 registros\n")
cat("Step 2 — Title harmonization JW:             421 registros\n")
cat("Step 3 — Eligibility screening + manual:      98 registros\n")
cat("Step 4 — Exclusión manual fuera de scope:     84 registros\n")
cat("=======================================\n")

############################################################
# Step_4.7 — Launch Biblioshiny
############################################################

biblioshiny()
