############################################################
# STEP 6 — Final Corpus TALL Depurated
# Combines: (1) out-of-scope document removal by DOI
#           (2) residual term cleaning directly in text field
#           (3) final depuration from Reinert cluster analysis
#
# Input:  Step_5_TALL_clean.csv   (84 records)
# Output: Step_6_TALL_depurated.csv  (82 records)
#         Step_6_TALL_depurated_QC.csv
#
# Author: Antonio Sánchez Cordero
# Pipeline: M_Step 6 — Final depurated corpus for TALL
#
# Documents removed (2 total):
#   doc_02: deep learning for window detection
#           DOI: 10.1016/j.renene.2021.05.155
#           Reason: dominant vocabulary (layout, ratio, shape,
#           aperture) collapses all clustering solutions
#   doc_43: holistic overview NV warm climates
#           DOI: 10.1016/j.buildenv.2023.110942
#           Reason: dominant vocabulary (furniture, o/a, p.m.,
#           arrangement) collapses all clustering solutions
#
# NOTE ON DEPURATION STRATEGY:
# TALL's Custom Term List does not reliably filter terms
# when the lemmatiser tokenises them differently from the
# stoplist entries. All noise terms are therefore removed
# directly from the text field before TALL import.
# Only terms confirmed as non-technical and non-discriminant
# are removed. Technical vocabulary (airflow, dynamic,
# neural, degree, floor, direction, exhaust, angle) is
# preserved to maintain semantic integrity of clusters.
############################################################

rm(list = ls())
graphics.off()

library(readr)
library(dplyr)
library(stringr)

############################################################
# S6.0 — Input / output parameters
############################################################

file_input  <- "Step_5_TALL_clean.csv"
file_output <- "Step_6_TALL_depurated.csv"
file_qc     <- "Step_6_TALL_depurated_QC.csv"

stopifnot(file.exists(file_input))
cat("Input:", file_input, "\n")

############################################################
# S6.1 — Load corpus
############################################################

df <- read_delim(file_input, delim = ";",
                 show_col_types = FALSE,
                 locale = locale(encoding = "UTF-8"))

cat("Records loaded:", nrow(df), "| Columns:", ncol(df), "\n")
stopifnot(nrow(df) > 0)
stopifnot("text" %in% names(df))

# Preserve original text for QC
df$text_original <- df$text

############################################################
# S6.2 — Remove out-of-scope documents by DOI
############################################################

dois_remove <- c(
  "10.1016/j.renene.2021.05.155",     # doc_02 — window detection DL
  "10.1016/j.buildenv.2023.110942"    # doc_43 — holistic overview NV
)

cat("\n--- Documents to remove ---\n")
df <- df %>%
  mutate(DI_norm = str_squish(str_to_lower(as.character(DI))))

df_check <- df %>%
  filter(DI_norm %in% str_to_lower(dois_remove)) %>%
  select(DI, TI, PY, SO)
print(df_check)

# Verify all DOIs were found
n_found <- nrow(df_check)
cat("\nDOIs found for removal:", n_found, "of", length(dois_remove), "\n")

if (n_found < length(dois_remove)) {
  cat("\n--- Partial match diagnostic ---\n")
  for (doi in dois_remove) {
    prefix <- str_extract(doi, "10\\.[0-9]+/[a-z\\.]+")
    matches <- df %>%
      filter(str_detect(DI_norm,
                        fixed(prefix, ignore_case = TRUE))) %>%
      select(DI, TI)
    if (nrow(matches) > 0) {
      cat("Prefix", prefix, "found:\n")
      print(matches)
    } else {
      cat("✗ NOT FOUND — prefix:", prefix, "\n")
    }
  }
  warning("Not all DOIs were found. Check DOI spelling.")
}

n_before <- nrow(df)

df <- df %>%
  filter(!DI_norm %in% str_to_lower(dois_remove)) %>%
  select(-DI_norm)

row.names(df) <- NULL
cat("Records before removal:", n_before, "\n")
cat("Records after removal: ", nrow(df), "\n")
cat("Documents removed:     ", n_before - nrow(df), "\n")

############################################################
# S6.3 — Residual term cleaning (pass 1): boilerplate and
# dominant vocabulary from removed documents
############################################################

residual_terms_p1 <- list(
  
  # ── Editorial boilerplate not caught by S5 ───────────
  "\\bauthor\\b",        "\\bbuilder\\b",
  "\\binitial\\b",       "\\bplan\\b",
  "\\bregard\\b",        "\\bperform\\b",
  "\\bpublish\\b",       "\\bpublisher\\b",
  "\\bpublication\\b",   "\\b2016\\b",
  "\\b2017\\b",
  
  # ── Doc_02 dominant vocabulary (window detection DL) ─
  "\\bfurniture\\b",     "\\barrangement\\b",
  "\\baperture\\b",      "\\blayout\\b",
  "\\bshape\\b",         "o/a",
  "p\\.m\\.",            "p\\.pm\\.",
  
  # ── Doc_43 dominant vocabulary (holistic overview) ───
  "\\brespectively\\b",  "\\bsecond\\b",
  "\\bearly\\b",         "\\bstage\\b",
  "\\bmetric\\b",        "\\blife\\b",
  
  # ── Generic academic terms not caught by stoplist ────
  "\\bwhen\\b",          "\\bautomatic\\b",
  "\\bcycle\\b",         "\\bfamily\\b",
  "\\bcost\\b",          "\\bexplor\\b",
  
  # ── Geographic/demographic terms out of scope ─────────
  "\\bnorway\\b",        "\\bnorwegian\\b",
  "\\bnzeb\\b",
  
  # ── Physiological/tropical comfort terms ─────────────
  "\\bcognitive\\b",     "\\bphysiological\\b",
  "\\bsensation\\b",     "\\bsweat\\b",
  "\\bclothing\\b",      "\\btropical\\b",
  "\\bsingapore\\b",     "\\bheart\\b",
  "\\bskin\\b",          "\\bfan\\b",
  "\\bnaturally\\b",     "\\bassist\\b",
  
  # ── Adverbs and generic connectors ───────────────────
  "\\brespect\\b",       "\\bdegre\\b",
  "\\bcharacteristic\\b"
)

############################################################
# S6.3b — Final depuration (pass 2): noise terms identified
# from iterative Reinert cluster inspection in TALL.
#
# CONSERVATIVE APPROACH: only confirmed non-technical and
# non-discriminant terms are removed. Technical vocabulary
# that structures valid clusters is explicitly preserved:
#   PRESERVED: airflow, dynamic, neural, deep, degree,
#              floor, direction, exhaust, angle, story,
#              velocity, network, framework, fluid
############################################################

residual_terms_p2 <- list(
  
  # ── Cluster 4 — modal verbs and generic adjectives ───
  "\\bwill\\b",          "\\bcan\\b",
  "\\bdifferent\\b",     "\\bevaluate\\b",
  "\\bhour\\b",          "\\bclimatic\\b",
  "\\bpresent\\b",       "\\bclimate\\s+zone\\b",
  
  # ── Cluster 5 — generic academic terms ───────────────
  "\\bvaluable\\b",      "\\binsight\\b",
  "\\bdensely\\b",       "\\bprovide\\b",
  "\\belementary\\b",    "\\bpopulate\\b",
  "\\bpolicymaker\\b",
  
  # ── Cluster 2 — sports facilities and off-scope ──────
  "\\bpool\\b",          "\\bswim\\b",
  "\\bhygrothermal\\b",  "\\bglaze\\b",
  "\\binstallation\\b",  "\\banalyse\\b",
  "\\bsemi[- ]arid\\b",  "\\bbehaviour\\b",
  
  # ── Cluster 4 — specific geographic contamination ────
  "\\brural\\b",         "\\blyon\\b",
  "\\bbenguerir\\b",     "\\bresidenc\\b",
  
  # ── Cluster 6 — document-specific abbreviations only ─
  # (technical terms in this cluster are preserved)
  "\\bwwc\\b",           "\\bssv\\b",
  "\\bmultill\\b"
)

############################################################
# S6.3c — Cleaning function and application
############################################################

clean_residual <- function(x, patterns) {
  for (pat in patterns) {
    x <- str_replace_all(x, regex(pat, ignore_case = TRUE), " ")
  }
  str_squish(x)
}

df$text <- clean_residual(df$text, residual_terms_p1)
cat("\nPass 1 cleaning (S6.3) completed —",
    length(residual_terms_p1), "patterns\n")

df$text <- clean_residual(df$text, residual_terms_p2)
cat("Pass 2 cleaning (S6.3b) completed —",
    length(residual_terms_p2), "patterns\n")

cat("Total term patterns removed:",
    length(residual_terms_p1) + length(residual_terms_p2), "\n")

############################################################
# S6.4 — Quality control
############################################################

check_terms <- c(
  # Pass 1 checks
  "author", "builder", "furniture", "arrangement",
  "aperture", "layout", "shape", "o/a", "p\\.m\\.",
  "norway", "singapore", "tropical", "sensation",
  "2016", "regard", "perform",
  # Pass 2 checks
  "will", "can", "valuable", "insight",
  "pool", "swim", "hygrothermal",
  "rural", "lyon", "benguerir", "residenc",
  "wwc", "ssv", "multill"
)

cat("\n--- Quality control: residual terms ---\n")
any_found <- FALSE
for (term in check_terms) {
  n <- sum(str_detect(df$text,
                      regex(paste0("\\b", term, "\\b"),
                            ignore_case = TRUE)), na.rm = TRUE)
  if (n > 0) {
    cat(term, "->", n, "documents still contain this term\n")
    any_found <- TRUE
  }
}
if (!any_found) cat("✓ No residual terms detected\n")

# Text length statistics
cat("\nMean text length ORIGINAL:",
    round(mean(nchar(df$text_original), na.rm = TRUE)), "chars\n")
cat("Mean text length CLEAN:   ",
    round(mean(nchar(df$text),          na.rm = TRUE)), "chars\n")

# Empty documents check
n_empty <- sum(nchar(df$text) < 50, na.rm = TRUE)
cat("Documents with text < 50 chars:", n_empty, "\n")
if (n_empty > 0) {
  warning("Short documents detected — manual review recommended")
  df %>%
    filter(nchar(text) < 50) %>%
    select(any_of(c("UT", "DI", "TI", "text"))) %>%
    print()
}

# Verify expected corpus size
cat("\nExpected corpus size: 82 records\n")
cat("Actual corpus size:  ", nrow(df), "records\n")
if (nrow(df) != 82) {
  warning("Unexpected corpus size. Check DOIs in dois_remove vector.")
}

############################################################
# S6.5 — Export depurated CSV for TALL
############################################################

df_export <- df %>% select(-text_original)

write_delim(df_export, file_output,
            delim = ";", na = "", quote = "needed")
cat("\n[Saved]", file_output, "—",
    nrow(df_export), "records,",
    ncol(df_export), "columns\n")

# Integrity check
test <- read_delim(file_output, delim = ";",
                   show_col_types = FALSE)
stopifnot("text" %in% names(test))
stopifnot(nrow(test) == nrow(df))
cat("✓ CSV integrity verified\n")

############################################################
# S6.6 — Export QC log
############################################################

qc_log <- df %>%
  select(any_of(c("UT", "DI", "TI"))) %>%
  mutate(
    chars_original = nchar(df$text_original),
    chars_clean    = nchar(df$text),
    reduction_pct  = round(
      (chars_original - nchar(df$text)) / chars_original * 100, 1)
  )

write.csv(qc_log, file_qc, row.names = FALSE)
cat("[Saved]", file_qc, "— QC log\n")

############################################################
# S6.7 — Final summary
############################################################

cat("\n========== STEP 6 SUMMARY ==========\n")
cat("Input:               ", file_input,  "\n")
cat("Output TALL:         ", file_output, "\n")
cat("Output QC log:       ", file_qc,     "\n")
cat("Records input:        84\n")
cat("Records removed:      2  (doc_02, doc_43 — out of scope)\n")
cat("Records output:      ", nrow(df_export), "\n")
cat("Cleaning passes:      2  (S6.3 + S6.3b)\n")
cat("Total term patterns: ",
    length(residual_terms_p1) + length(residual_terms_p2), "\n")
cat("=====================================\n")
cat("\nImport into TALL:   ", file_output,
    " | Text column: 'text'\n")
cat("\n========== UPDATED PIPELINE ==========\n")
cat("Step 1 — Merge WoS + Scopus + DOI cleaning:   427 records\n")
cat("Step 2 — Title harmonization JW:              421 records\n")
cat("Step 3 — Eligibility screening + manual:       98 records\n")
cat("Step 4 — Manual out-of-scope exclusion:        84 records\n")
cat("Step 5 — Boilerplate cleaning:                 84 records\n")
cat("Step 6 — Depurated corpus for TALL:            82 records\n")
cat("=======================================\n")

library(readr); library(dplyr)

df <- read_delim("Step_6_TALL_depurated.csv",
                 delim = ";", show_col_types = FALSE)

# Opción 1 — Exportar con coma como separador
write_csv(df, "Step_6_TALL_depurated_comma.csv", na = "")
cat("Saved with comma separator\n")

# Opción 2 — Exportar con tabulador
write_tsv(df, "Step_6_TALL_depurated_tab.csv", na = "")
cat("Saved with tab separator\n")
