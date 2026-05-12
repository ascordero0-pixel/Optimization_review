############################################################
# PIPELINE BIBLIOMETRIC REVIEW
# STEP 1: Structural cleaning (DOI-based)
# Produces: Step_1_struct.RData  →  M_Step 1 (target: ~427u)
# Author: Antonio Sánchez Cordero
############################################################

rm(list = ls()); graphics.off()

library(bibliometrix)
library(dplyr)
library(stringr)

wos_file    <- "WOS_400.txt"
scopus_file <- "SCO_181.csv"

# ── 1.1  Convert ──────────────────────────────────────────
M_wos    <- convert2df(wos_file,    dbsource = "wos",    format = "plaintext")
M_scopus <- convert2df(scopus_file, dbsource = "scopus", format = "csv")
cat("WoS:", nrow(M_wos), " | Scopus:", nrow(M_scopus), "\n")

# ── 1.2  Metadata quality BEFORE merge ───────────────────
cat("\n--- Metadata WoS ---\n");    print(missingData(M_wos))
cat("\n--- Metadata Scopus ---\n"); print(missingData(M_scopus))

# ── 1.3  Merge ───────────────────────────────────────────
Step_1 <- mergeDbSources(M_wos, M_scopus,
                         remove.duplicated = TRUE, verbose = TRUE)
cat("\nPost-merge:", nrow(Step_1), "records\n")

# Restaurar CR si fue renombrado
if ("CR_raw" %in% names(Step_1)) {
  Step_1$CR <- Step_1$CR_raw
  cat("[OK] CR restaurado desde CR_raw\n")
} else {
  cat("[INFO] CR_raw no encontrado — CR se mantiene como está\n")
}

# ── 1.4  DOI cleaning + deduplication ────────────────────
stopifnot("DI" %in% names(Step_1))

clean_doi <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\s+", " ") %>% str_trim() %>%
    str_remove("^doi\\s*[:]?\\s*") %>%
    str_remove("^https?://(dx\\.)?doi\\.org/") %>%
    str_remove("^urn:doi:") %>%
    str_trim() %>%
    { ifelse(str_detect(., "10\\."),
             str_extract(., "10\\.[^\\s\";>]+"),
             NA_character_) } %>%
    str_replace("[\\.,;\\)]+$", "") %>%
    str_trim()
}

n_before <- nrow(Step_1)
Step_1 <- Step_1 %>%
  mutate(DI = clean_doi(as.character(DI))) %>%
  filter(!is.na(DI) & DI != "")
cat("Registros eliminados por DOI inválido:", n_before - nrow(Step_1), "\n")

Step_1 <- Step_1 %>%
  arrange(DI) %>%
  distinct(DI, .keep_all = TRUE)
cat("Post-deduplicación DOI:", nrow(Step_1), "records  [target: ~427]\n")

# ── 1.5  Metadata quality AFTER cleaning ─────────────────
cat("\n--- Metadata post-cleaning ---\n")
print(missingData(Step_1))

# ── 1.6  Save ─────────────────────────────────────────────
M <- Step_1
save(M, file = "Step_1_struct.RData")
cat("\n[Saved] Step_1_struct.RData —", nrow(M), "records\n")

biblioshiny()
