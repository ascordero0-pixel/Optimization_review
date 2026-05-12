############################################################
# STEP 2 — Semantic title cleaning, deduplication, harmonisation
# Input:  Step_1_struct.RData
# Output: Step_2_semantic_JW.RData  →  M_Step 2 (target: ~421u)
# Author: Antonio Sánchez Cordero
############################################################

library(bibliometrix); library(dplyr)
library(stringr);      library(stringdist)

# ── Carga explícita desde disco (no depende del entorno) ──
if (!exists("Step_1")) {
  load("Step_1_struct.RData")   # carga objeto M
  Step_1 <- M; rm(M)
}
Step_2 <- Step_1
stopifnot("TI" %in% colnames(Step_2))
cat("Input records:", nrow(Step_2), "\n")

# ── 2.1  Exact title deduplication ───────────────────────
Step_2 <- Step_2 %>%
  mutate(TI_norm = TI %>% str_to_lower() %>%
           str_replace_all("&", "and") %>%
           str_squish())

n_before <- nrow(Step_2)
Step_2 <- Step_2 %>%
  arrange(TI_norm) %>%
  distinct(TI_norm, .keep_all = TRUE)
cat("Exact duplicates removed:", n_before - nrow(Step_2),
    " | Remaining:", nrow(Step_2), "\n")

# ── Helper JW clustering ──────────────────────────────────
jw_cluster <- function(vals, threshold, p = 0.1) {
  if (length(vals) <= 1 || length(vals) > 2500) return(seq_along(vals))
  dmat <- stringdistmatrix(vals, vals, method = "jw", p = p)
  sim  <- 1 - dmat
  cid  <- rep(NA_integer_, length(vals))
  k    <- 1L
  for (i in seq_along(vals)) {
    if (!is.na(cid[i])) next
    grp     <- which(sim[i, ] >= threshold)
    cid[grp] <- k; k <- k + 1L
  }
  cid
}

# ── 2.2  JW deduplication (≥ 0.95, with deletion) ────────
vals <- Step_2$TI_norm
cid  <- jw_cluster(vals, threshold = 0.95)
Step_2$TI_cluster <- cid

n_before <- nrow(Step_2)
Step_2 <- Step_2 %>%
  arrange(TI_cluster) %>%
  distinct(TI_cluster, .keep_all = TRUE)
cat("JW≥0.95 duplicates removed:", n_before - nrow(Step_2),
    " | Remaining:", nrow(Step_2), "\n")

# ── 2.3  JW harmonisation (= 0.85, NO deletion) ──────────
# Reemplaza TI con el título canónico del cluster (más frecuente)
vals  <- Step_2$TI_norm
cid   <- jw_cluster(vals, threshold = 0.85)

rep_titles <- data.frame(cluster = cid, title = vals,
                         stringsAsFactors = FALSE) %>%
  count(cluster, title) %>%
  arrange(cluster, desc(n), title) %>%
  group_by(cluster) %>% slice_head(n = 1) %>% ungroup() %>%
  select(cluster, rep_title = title)

Step_2$TI_cluster <- cid
Step_2 <- Step_2 %>%
  left_join(rep_titles, by = c("TI_cluster" = "cluster")) %>%
  mutate(
    TI_orig = TI,           # preservar original para auditoría
    TI      = rep_title     # ← TI sustituido con título canónico
  ) %>%
  select(-rep_title, -TI_cluster, -TI_norm)

changed <- sum(Step_2$TI != Step_2$TI_orig, na.rm = TRUE)
cat("Titles harmonised (JW=0.85):", changed, "\n")
cat("Final records Step_2:", nrow(Step_2), " [target: ~421]\n")

# ── 2.4  Save ─────────────────────────────────────────────
M <- Step_2
class(M) <- c("bibliometrixDB", "data.frame")
save(M, file = "Step_2_semantic_JW.RData")
cat("[Saved] Step_2_semantic_JW.RData\n")

biblioshiny()
