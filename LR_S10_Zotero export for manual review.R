############################################################
# STEP 10 — Export corpus to Zotero for full-text
#           access verification
#
# Input:  Step_6_TALL_depurated.csv  (82 records)
#         Step_4_final_corpus_84_fullcols.RData (metadata)
#
# Outputs:
#   - S10_corpus_zotero_export.ris   (import into Zotero)
#   - S10_corpus_access_form.xlsx    (manual access tracking)
#   - S10_corpus_doi_list.txt        (DOI list for quick check)
#
# Workflow:
#   1. Export RIS → import into Zotero
#   2. In Zotero: right-click each record → Find Available PDF
#   3. Fill S10_corpus_access_form.xlsx with access status
#   4. Run S10b script to analyse access coverage
#
# Author: Antonio Sánchez Cordero
# Pipeline: M_Step 10 — Full-text access verification
############################################################

library(readr); library(dplyr); library(stringr); library(writexl)

file_tall  <- "Step_6_TALL_depurated.csv"
file_ris   <- "S10_corpus_zotero_export.ris"
file_form  <- "S10_corpus_access_form.xlsx"
file_dois  <- "S10_corpus_doi_list.txt"

# ── Cargar corpus directamente desde TALL CSV ─────────────
df_tall <- read_delim(file_tall, delim = ";",
                      show_col_types = FALSE)
cat("Records loaded:", nrow(df_tall), "\n")

# ── Añadir doc_id y normalizar ────────────────────────────
corpus <- df_tall %>%
  mutate(
    doc_id = paste0("doc_", sprintf("%02d", row_number())),
    DI     = str_squish(str_to_lower(as.character(DI)))
  ) %>%
  select(doc_id, DI, AU, TI, SO, PY, VL, IS,
         BP, EP, AB, UT, C1, RP, URL)

cat("Columns available:", paste(names(corpus), collapse=", "), "\n")
cat("Records with DOI:", sum(nzchar(corpus$DI, keepNA=FALSE)), "\n")
cat("Records with abstract:", sum(nzchar(corpus$AB, keepNA=FALSE)), "\n")

# ── Generar RIS para Zotero ───────────────────────────────
safe_val <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | !nzchar(trimws(x)), "", trimws(x))
}

generate_ris <- function(df) {
  out <- character(0)
  for (i in seq_len(nrow(df))) {
    row   <- df[i, , drop = FALSE]
    entry <- character(0)
    entry <- c(entry, "TY  - JOUR")
    entry <- c(entry, paste0("ID  - ", safe_val(row$doc_id)))
    
    # Authors
    au_raw <- safe_val(row$AU)
    if (nzchar(au_raw)) {
      au_list <- trimws(strsplit(au_raw, ";")[[1]])
      au_list <- au_list[nzchar(au_list)]
      entry   <- c(entry, paste0("AU  - ", au_list))
    }
    
    ti <- safe_val(row$TI)
    if (nzchar(ti))  entry <- c(entry, paste0("TI  - ", ti))
    
    so <- safe_val(row$SO)
    if (nzchar(so))  entry <- c(entry, paste0("JO  - ", so))
    
    py <- safe_val(row$PY)
    if (nzchar(py))  entry <- c(entry, paste0("PY  - ", py))
    
    vl <- safe_val(row$VL)
    if (nzchar(vl))  entry <- c(entry, paste0("VL  - ", vl))
    
    is_ <- safe_val(row$IS)
    if (nzchar(is_)) entry <- c(entry, paste0("IS  - ", is_))
    
    bp <- safe_val(row$BP); ep <- safe_val(row$EP)
    if (nzchar(bp))  entry <- c(entry, paste0("SP  - ", bp))
    if (nzchar(ep))  entry <- c(entry, paste0("EP  - ", ep))
    
    di <- safe_val(row$DI)
    if (nzchar(di))  entry <- c(entry, paste0("DO  - ", di))
    
    url <- safe_val(row$URL)
    if (nzchar(url)) entry <- c(entry, paste0("UR  - ", url))
    
    ab <- safe_val(row$AB)
    if (nzchar(ab)) {
      if (nchar(ab) > 2000) ab <- substr(ab, 1, 2000)
      entry <- c(entry, paste0("AB  - ", ab))
    }
    
    c1 <- safe_val(row$C1)
    if (nzchar(c1))  entry <- c(entry, paste0("AD  - ", c1))
    
    entry <- c(entry, "ER  - ", "")
    out   <- c(out, entry)
  }
  out
}

writeLines(generate_ris(corpus), file_ris, useBytes = FALSE)
cat("[Saved]", file_ris, "—", nrow(corpus), "records\n")

# ── Formulario de seguimiento de acceso ──────────────────
access_form <- corpus %>%
  select(doc_id, DI, TI, SO, PY, AU) %>%
  mutate(
    full_text_access = NA_character_,  # YES / NO / PARTIAL
    access_source    = NA_character_,  # Institutional / OA /
    # ResearchGate / Request
    pdf_available    = NA_character_,  # YES / NO
    notes            = NA_character_
  )

instructions <- data.frame(
  Step = c(
    "1 — Import RIS into Zotero",
    "2 — Find PDFs automatically",
    "3 — Fill full_text_access",
    "4 — Fill access_source",
    "5 — Fill pdf_available",
    "6 — Run S10b in RStudio"
  ),
  Instructions = c(
    "File → Import → S10_corpus_zotero_export.ris",
    "Select all 82 → right-click → Find Available PDF",
    "YES = full text | NO = not available | PARTIAL = abstract only",
    "Institutional / Open Access / ResearchGate / Request / Other",
    "YES = PDF downloaded | NO = not downloaded",
    "Run S10b block to generate coverage report"
  ),
  stringsAsFactors = FALSE
)

write_xlsx(list("Access_tracking" = access_form,
                "Instructions"    = instructions),
           file_form)
cat("[Saved]", file_form, "\n")

# ── Lista de DOIs ─────────────────────────────────────────
doi_lines <- corpus %>%
  mutate(line = paste0(doc_id, " | ", PY, " | ",
                       str_trunc(TI, 60), " | ",
                       "https://doi.org/", DI)) %>%
  pull(line)

writeLines(c(
  "===========================================",
  "CORPUS DOI LIST — Full-text access check",
  paste("Generated:", Sys.time()),
  paste("Total records:", length(doi_lines)),
  "===========================================", "",
  doi_lines), file_dois)
cat("[Saved]", file_dois, "\n")

cat("\n========== STEP 10 SUMMARY ==========\n")
cat("Records:             ", nrow(corpus), "\n")
cat("With DOI:            ",
    sum(nzchar(corpus$DI, keepNA=FALSE)), "\n")
cat("With abstract:       ",
    sum(nzchar(corpus$AB, keepNA=FALSE)), "\n")
cat("Outputs: RIS + Excel form + DOI list\n")
cat("=====================================\n")
cat("\nNext steps:\n")
cat("  1. Import", file_ris, "into Zotero\n")
cat("  2. Find Available PDF for all 82 records\n")
cat("  3. Fill", file_form, "with access status\n")
cat("  4. Run S10b to generate coverage report\n")

# ── S10b — Ejecutar después de completar el formulario ────
cat('\n--- S10b: Run after filling access form ---\n')
cat('
library(readxl); library(dplyr)
form <- read_excel("S10_corpus_access_form.xlsx",
                   sheet = "Access_tracking")
cat("=== ACCESS COVERAGE REPORT ===\\n")
cat("Total:        ", nrow(form), "\\n")
cat("Full text YES:", sum(form$full_text_access=="YES", na.rm=TRUE), "\\n")
cat("Partial:      ", sum(form$full_text_access=="PARTIAL", na.rm=TRUE), "\\n")
cat("No access:    ", sum(form$full_text_access=="NO", na.rm=TRUE), "\\n")
cat("Not assessed: ", sum(is.na(form$full_text_access)), "\\n")
cat("Coverage:     ",
    round(sum(form$full_text_access=="YES", na.rm=TRUE)/nrow(form)*100,1),
    "%\\n")
cat("\\nBy source:\\n")
print(table(form$access_source, useNA="always"))
cat("\\nJournals with no access:\\n")
form %>% filter(full_text_access=="NO") %>%
  count(SO, sort=TRUE) %>% print()
')

