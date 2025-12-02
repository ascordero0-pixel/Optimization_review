############################################################
# STEP_4 — Remove irrelevant records by DOI (final screening)
############################################################

rm(list = ls())
graphics.off()

library(bibliometrix)
library(dplyr)

# -----------------------------------------------------------
# STEP_4.1 — Load Step_2 (208 articles)
# -----------------------------------------------------------

load("Step_2_semantic_JW.RData")   # This must load 'M'
df <- M

nrow(df)   # should be 208

#-----------------------------------------------------------
# 4.2 Índices de fila a eliminar (1-based, obtenidos de los RIS)
#-----------------------------------------------------------

rows_to_remove <- c(
  1, 3, 4, 5, 8, 9, 10, 13, 14, 15,
  17, 20, 28, 30, 31, 32, 34, 39, 40, 42,
  45, 51, 57, 58, 64, 70, 74, 75, 76, 78,
  79, 80, 85, 87, 88, 90, 95, 99, 102, 106,
  108, 110, 113, 117, 122, 129, 138, 142, 146, 148,
  150, 151, 152, 156, 162, 165, 173, 177, 179, 183,
  187, 190, 194, 195, 196, 198, 207
)

length(rows_to_remove)  # 67


#-----------------------------------------------------------
# 4.3 Line removal
#-----------------------------------------------------------

Step_4 <- df[-rows_to_remove, ]

nrow(Step_4)  # debería ser 208 - 67 = 141


#-----------------------------------------------------------
# 4.4 Biblioshiny final object
#-----------------------------------------------------------

M_step4 <- Step_4
class(M_step4) <- c("bibliometrixDB", "data.frame")

save(M_step4, file = "Step_4_final_screened.RData")

cat("Saved: Step_4_final_screened.RData with", nrow(M_step4), "records\n")

#-----------------------------------------------------------
# 4.5 Biblioshiny analysis
#-----------------------------------------------------------


biblioshiny()


#-----------------------------------------------------------
# 4.6 Zotero final verification
#-----------------------------------------------------------

rm(list = ls())
graphics.off()

library(dplyr)

load("Step_4_final_screened.RData")  # load M_step4
df <- as.data.frame(M_step4)

nrow(df)  # must be your line number

#-----------------------------------------------------------
# 4.7. Function to convert dataframe to RIS
#-----------------------------------------------------------

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


#-----------------------------------------------------------
# 4.8. Export to RIS (human verification)
#-----------------------------------------------------------

ris_output <- convert_to_ris(df)

ris_path <- "Step_4_Irrelevant_excluded_verification.ris"
writeLines(ris_output, con = ris_path)

cat("RIS file successfully generated:\n", ris_path, "\n")
