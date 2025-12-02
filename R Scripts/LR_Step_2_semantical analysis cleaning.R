############################################################
# STEP 2 — Semantic title cleaning, deduplication and harmonisation
############################################################

#===========================================================
# Step_2.0 — Clean environment and load packages
#===========================================================

rm(list = ls())
graphics.off()

library(dplyr)
library(stringr)
library(stringdist)
library(bibliometrix)

#===========================================================
# Step_2.1 — Load structural-clean dataset from Step 1
#===========================================================

step1_path <- "Step_1_struct.RData"

load(step1_path)   # loads object M
ls()
class(M)
nrow(M)

Step_2 <- M

if (!"TI" %in% colnames(Step_2)) {
  stop("No 'TI' column found in Step_2.")
}

#===========================================================
# Step_2.2 — Exact title-based deduplication
#===========================================================

cat("\n==== STEP 2.2: EXACT TITLE DEDUPLICATION ====\n")

Step_2 <- Step_2 %>%
  mutate(
    TI_clean_exact = TI %>%
      str_to_lower() %>%
      str_squish()
  )

dup_exact <- Step_2 %>%
  group_by(TI_clean_exact) %>%
  filter(n() > 1)

cat("Exact duplicate title groups:", n_distinct(dup_exact$TI_clean_exact), "\n")

Step_2 <- Step_2 %>%
  arrange(TI_clean_exact) %>%
  distinct(TI_clean_exact, .keep_all = TRUE) %>%
  select(-TI_clean_exact)

cat("Rows AFTER exact-title deduplication:", nrow(Step_2), "\n")

#===========================================================
# Step_2.3 — Semantic title dedup (Jaro–Winkler ≥ 0.95)
#===========================================================

cat("\n==== STEP 2.3: JW TITLE DEDUPLICATION (≥ 0.95) ====\n")

Step_2 <- Step_2 %>%
  mutate(
    TI_norm = TI %>%
      str_to_lower() %>%
      str_replace_all("&", "and") %>%  # & vs "and"
      str_squish()
  )

titles    <- Step_2$TI_norm
valid_idx <- which(!is.na(titles) & titles != "")
vals      <- titles[valid_idx]

n_total <- nrow(Step_2)
n_valid <- length(vals)

cat("Total records:", n_total, "\n")
cat("Records with non-empty normalised titles:", n_valid, "\n")

if (n_valid <= 1) {
  Step_2_dedup <- Step_2
  
} else {
  
  dmat        <- stringdistmatrix(vals, vals, method = "jw", p = 0.1)
  sim_matrix  <- 1 - dmat
  threshold   <- 0.95
  cluster_id  <- rep(NA_integer_, n_valid)
  current_cl  <- 1L
  
  for (i in seq_along(vals)) {
    if (!is.na(cluster_id[i])) next
    sim_i    <- sim_matrix[i, ]
    same_grp <- which(sim_i >= threshold)
    cluster_id[same_grp] <- current_cl
    current_cl           <- current_cl + 1L
  }
  
  TI_cluster_sim <- rep(NA_integer_, n_total)
  TI_cluster_sim[valid_idx] <- cluster_id
  
  Step_2 <- Step_2 %>%
    mutate(TI_cluster_sim = TI_cluster_sim)
  
  dup_sim <- Step_2 %>%
    filter(!is.na(TI_cluster_sim)) %>%
    group_by(TI_cluster_sim) %>%
    filter(n() > 1) %>%
    ungroup()
  
  cat("JW duplicate clusters (≥ 0.95):",
      n_distinct(dup_sim$TI_cluster_sim), "\n")
  
  # Keep first record of each JW cluster; keep all non-clustered records
  Step_2_dedup <- Step_2 %>%
    arrange(TI_cluster_sim) %>%
    mutate(
      keep_sim = case_when(
        is.na(TI_cluster_sim) ~ TRUE,
        !duplicated(TI_cluster_sim) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(keep_sim) %>%
    select(-keep_sim, -TI_cluster_sim)
}

cat("Rows AFTER JW-title deduplication (≥ 0.95):", nrow(Step_2_dedup), "\n")

#===========================================================
# Step_2.4 — Title harmonisation (Jaro–Winkler = 0.85, no deletions)
#===========================================================

cat("\n==== STEP 2.4: TITLE HARMONISATION (JW = 0.85, NO DELETIONS) ====\n")

jw_clean_titles <- function(titles, threshold = 0.85) {
  t_clean <- tolower(titles)
  t_clean <- trimws(t_clean)
  
  t_clean[t_clean == ""] <- NA_character_
  valid_idx <- which(!is.na(t_clean))
  vals      <- t_clean[valid_idx]
  
  if (length(vals) <= 1) {
    return(list(
      cleaned_vector = t_clean,
      mapping        = tibble(
        original = titles,
        cleaned  = t_clean
      )
    ))
  }
  
  dmat <- stringdistmatrix(vals, vals, method = "jw", p = 0.1)
  
  cluster_id      <- rep(NA_integer_, length(vals))
  current_cluster <- 1L
  
  for (i in seq_along(vals)) {
    if (!is.na(cluster_id[i])) next
    sim_i    <- 1 - dmat[i, ]
    same_grp <- which(sim_i >= threshold)
    cluster_id[same_grp] <- current_cluster
    current_cluster      <- current_cluster + 1L
  }
  
  df_clusters <- tibble(
    orig_index = valid_idx,
    title      = vals,
    cluster    = cluster_id
  )
  
  reps <- df_clusters %>%
    group_by(cluster, title) %>%
    tally(name = "n") %>%
    arrange(cluster, desc(n), title) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(cluster, rep_title = title)
  
  df_clusters <- df_clusters %>%
    left_join(reps, by = "cluster")
  
  t_out <- t_clean
  t_out[df_clusters$orig_index] <- df_clusters$rep_title
  
  list(
    cleaned_vector = t_out,
    mapping = tibble(
      original = titles,
      cleaned  = t_out
    )
  )
}

titles_raw <- Step_2_dedup$TI

jw_res <- jw_clean_titles(titles_raw, threshold = 0.85)

Step_2_dedup$TI_JW <- jw_res$cleaned_vector

mapping_titles <- data.frame(
  original = titles_raw,
  cleaned  = jw_res$cleaned_vector,
  stringsAsFactors = FALSE
)

changed_titles <- subset(
  mapping_titles,
  !is.na(cleaned) & original != cleaned
)

cat("Number of titles harmonised (changed):", nrow(changed_titles), "\n")
head(changed_titles, 20)

#===========================================================
# Step_2.5 — Save semantic-clean dataset for Step 3 / Biblioshiny
#===========================================================

M <- Step_2_dedup   # final semantic-clean bibliometrixDB

# Ensure that M remains a valid bibliometrixDB object
class(M) <- c("bibliometrixDB", "data.frame")

step2_path <- "Step_2_semantic_JW.RData"

save(M, file = step2_path)

cat("\nSemantic-clean object 'M' saved to:\n", step2_path, "\n")

# Optional: open Biblioshiny directly with this dataset
biblioshiny()
