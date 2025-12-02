############################################################
# STEP 1 — Structural cleaning (DOI-based)
# Author: Antonio Sánchez Cordero
############################################################

#===========================================================
# Step_1.0 — Clean environment
#===========================================================

rm(list = ls())
graphics.off()

#===========================================================
# Step_1.1 — Load bibliometrix
#===========================================================

if (!requireNamespace("bibliometrix", quietly = TRUE)) {
  install.packages("bibliometrix", dependencies = TRUE)
}
library(bibliometrix)

#===========================================================
# Step_1.2 — File paths
#===========================================================

wos_file    <- "DB_WOS_173_Full.txt"
scopus_file <- "DB_SCO_134_Full.csv"

file.exists(wos_file)
file.exists(scopus_file)

#===========================================================
# Step_1.3 — Convert WoS file
#===========================================================

M_wos <- convert2df(
  file     = wos_file,
  dbsource = "wos",
  format   = "plaintext"
)

cat("WoS dim (rows, cols):\n")
print(dim(M_wos))

#===========================================================
# Step_1.4 — Convert Scopus file
#===========================================================

M_scopus <- convert2df(
  file     = scopus_file,
  dbsource = "scopus",
  format   = "csv"
)

cat("Scopus dim (rows, cols):\n")
print(dim(M_scopus))

#===========================================================
# Step_1.5 — Metadata completeness BEFORE merging
#===========================================================

md_wos    <- missingData(M_wos)
md_scopus <- missingData(M_scopus)

cat("\n==== METADATA QUALITY: WoS ====\n")
print(md_wos)

cat("\n==== METADATA QUALITY: Scopus ====\n")
print(md_scopus)

#===========================================================
# Step_1.6.1 — Merge WoS + Scopus
#===========================================================

Step_1 <- mergeDbSources(
  M_wos,
  M_scopus,
  remove.duplicated = TRUE,
  verbose           = TRUE
)

cat("\nMerged dim (rows, cols):\n")
print(dim(Step_1))

#===========================================================
# Step_1.6.2 — Restore CR from CR_raw (if present)
#===========================================================

if ("CR_raw" %in% names(Step_1)) {
  Step_1$CR <- Step_1$CR_raw
}

#===========================================================
# Step_1.6.3 — DOI cleaning + strict DOI-based deduplication
#===========================================================

library(dplyr)
library(stringr)

cat("\n==== STARTING DOI CLEANING PROCESS (STRUCTURAL) ====\n")

if (!"DI" %in% names(Step_1)) {
  stop("No DOI column found (expected 'DI').")
}

# 1) Robust DOI normalisation
Step_1 <- Step_1 %>%
  mutate(
    DI_raw = as.character(DI),
    DI_clean = DI_raw %>%
      str_to_lower() %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_remove("^doi\\s*[:]?\\s*") %>%                 # "doi: 10..."
      str_remove("^https?://(dx\\.)?doi\\.org/") %>%     # URLs
      str_remove("^urn:doi:") %>%                        # URNs
      str_trim() %>%
      { ifelse(str_detect(., "10\\."), 
               str_extract(., "10\\.[^\\s\";>]+"),
               NA_character_) } %>%
      str_replace("[\\.,;\\)]+$", "") %>%                # trailing punctuation
      str_trim()
  )

# 2) Remove records without valid DOI
Step_1 <- Step_1 %>%
  filter(!is.na(DI_clean) & DI_clean != "")

cat("Rows AFTER removing missing DOI:", nrow(Step_1), "\n")

# 3) Overwrite DI with cleaned version
Step_1$DI <- Step_1$DI_clean

# 4) Remove exact DOI duplicates (keep first occurrence)
Step_1 <- Step_1 %>%
  arrange(DI) %>%
  distinct(DI, .keep_all = TRUE)

cat("Rows AFTER DOI-based deduplication:", nrow(Step_1), "\n")

# 5) Remove helper columns
Step_1 <- Step_1 %>%
  select(-DI_raw, -DI_clean)

#===========================================================
# Step_1.7 — Metadata completeness AFTER structural cleaning
#===========================================================

md_clean_struct <- missingData(Step_1)

cat("\n==== METADATA QUALITY: STRUCTURALLY CLEANED DATA ====\n")
print(md_clean_struct)

#===========================================================
# Step_1.8 — Save structural-clean dataset for Step 2
#===========================================================

M <- Step_1  # bibliometrixDB object required by Biblioshiny and next steps

step1_path <- "Step_1_struct.RData"

save(M, file = step1_path)

cat("\nStructural-clean object 'M' saved to:\n", step1_path, "\n")

# Optional working copy
Step_2_input <- M

grep("ti|title", colnames(Step_2_input), ignore.case = TRUE, value = TRUE)

