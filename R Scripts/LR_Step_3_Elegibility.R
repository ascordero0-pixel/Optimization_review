############################################################
# STEP_3 — Export to RIS and manual exclusion of records
############################################################

# -----------------------------------------------------------
# STEP_3.1 — Clean environment and load RData
# -----------------------------------------------------------

rm(list = ls())
graphics.off()

library(dplyr)

step2_path <- "Step_2_semantic_JW.RData"

load(step2_path)  # loads M
ls()
class(M)
nrow(M)

df <- M

# Comprobación de campos estándar RIS
intersect(
  c("AU","TI","SO","PY","VL","IS","DI","AB"),
  colnames(df)
)

# -----------------------------------------------------------
# STEP_3.2 — Function to convert dataframe to RIS
# -----------------------------------------------------------

convert_to_ris <- function(df,
                           col_auth   = "AU",
                           col_title  = "TI",
                           col_jour   = "SO",
                           col_year   = "PY",
                           col_vol    = "VL",
                           col_issue  = "IS",
                           col_doi    = "DI",
                           col_abs    = "AB") {
  
  if (!is.data.frame(df)) stop("Object is not a data.frame.")
  if (nrow(df) == 0)      stop("The data.frame has no rows.")
  
  safe_val <- function(row, col_name) {
    if (!is.null(col_name) &&
        !is.na(col_name) &&
        col_name %in% names(row)) {
      
      v <- as.character(row[[col_name]])
      ifelse(is.na(v), "", v)
      
    } else {
      ""
    }
  }
  
  entries <- lapply(seq_len(nrow(df)), function(i) {
    row   <- df[i, ]
    entry <- c("TY  - JOUR")
    
    v <- safe_val(row, col_auth);  if (nzchar(v)) entry <- c(entry, paste0("AU  - ", v))
    v <- safe_val(row, col_title); if (nzchar(v)) entry <- c(entry, paste0("TI  - ", v))
    v <- safe_val(row, col_jour);  if (nzchar(v)) entry <- c(entry, paste0("JO  - ", v))
    v <- safe_val(row, col_year);  if (nzchar(v)) entry <- c(entry, paste0("PY  - ", v))
    v <- safe_val(row, col_vol);   if (nzchar(v)) entry <- c(entry, paste0("VL  - ", v))
    v <- safe_val(row, col_issue); if (nzchar(v)) entry <- c(entry, paste0("IS  - ", v))
    v <- safe_val(row, col_doi);   if (nzchar(v)) entry <- c(entry, paste0("DO  - ", v))
    v <- safe_val(row, col_abs);   if (nzchar(v)) entry <- c(entry, paste0("AB  - ", v))
    
    c(entry, "ER  - ")
  })
  
  unlist(entries)
}

# -----------------------------------------------------------
# STEP_3.3 — Export FULL Step_2 to RIS (before manual exclusion)
# -----------------------------------------------------------

ris_output_full <- convert_to_ris(df)

ris_full_path <- "Step_2_semantic_JW.ris"

writeLines(ris_output_full, con = ris_full_path)

cat("RIS file (full Step_2) generated:\n", ris_full_path, "\n")

