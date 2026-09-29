############################################################
# STEP 8 — Hybrid taxonomy derivation from TALL exports
#
# METHODOLOGY:
#   - Cluster labels + RQ:
#     expert interpretation of chi-square terms (TALL)
#   - Top chi-square terms:
#     chi-square ranked positive terms (TALL Terms export)
#   - Key optimization objectives:
#     segment-level pattern matching (TALL Segments export)
#   - Ventilation types + Building typologies:
#     segment-level pattern matching (TALL Segments export)
#   - Optimisation methods:
#     segment-level pattern matching for all clusters (C1-C6)
#
# REVISION (2026-09): the C1 exception (methods derived from
#   chi-square terms) is removed so that the whole Methods
#   column is expressed in a single unit (pattern-match counts)
#   and is comparable across clusters. See S8.6 and S8.14.
#
# Inputs:
#   - S9_Segments_by_Cluster_TALL.xlsx
#   - S9_Terms_by_Cluster_TALL.xlsx
#
# Outputs (S9_* prefix, consistent with S9_Segments/S9_Terms inputs
#   and with S10-S12 downstream files):
#   - S9_taxonomy_chi2_terms.csv
#   - S9_taxonomy_objectives.csv
#   - S9_taxonomy_ventilation.csv
#   - S9_taxonomy_building.csv
#   - S9_taxonomy_optimisation.csv
#   - S9_taxonomy_hybrid_table.csv
#   - S9_taxonomy_hybrid_summary.xlsx
#
# Author: Antonio Sánchez Cordero
# Pipeline: M_Step 8 — Thematic taxonomy (Table 3)
############################################################

rm(list = ls())
graphics.off()

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

############################################################
# S8.0 — Input / output parameters
############################################################

file_segments <- "S9_Segments_by_Cluster_TALL.xlsx"
file_terms    <- "S9_Terms_by_Cluster_TALL.xlsx"

stopifnot(file.exists(file_segments))
stopifnot(file.exists(file_terms))

cat("Input segments:", file_segments, "\n")
cat("Input terms:   ", file_terms,    "\n")

############################################################
# S8.1 — Load TALL exports
############################################################

# Segments
seg_raw <- read_excel(file_segments, col_names = FALSE)
colnames(seg_raw) <- c("doc_id", "segment", "cluster")
seg <- seg_raw[-1, ] %>%
  mutate(
    doc_id  = as.character(doc_id),
    segment = as.character(segment),
    cluster = as.integer(cluster)
  ) %>%
  filter(!is.na(cluster))

cat("\nSegments loaded:", nrow(seg), "\n")
cat("Documents:      ", n_distinct(seg$doc_id), "\n")
cat("Clusters:       ", paste(sort(unique(seg$cluster)),
                              collapse = ", "), "\n")
cat("\nSegments per cluster:\n")
print(table(seg$cluster))

# Terms
terms_raw <- read_excel(file_terms, col_names = FALSE)
colnames(terms_raw) <- c("term", "freq_cluster", "pct_cluster",
                         "chi2", "pvalue", "sign", "cluster")
terms <- terms_raw[-1, ] %>%
  mutate(
    term         = as.character(term),
    freq_cluster = as.numeric(freq_cluster),
    pct_cluster  = as.numeric(pct_cluster),
    chi2         = as.numeric(chi2),
    sign         = as.character(sign),
    cluster      = as.integer(cluster)
  ) %>%
  filter(!is.na(cluster))

cat("\nTerms loaded:", nrow(terms), "\n")

############################################################
# S8.2 — Pattern dictionaries
############################################################

# ── Key optimization objectives ───────────────────────────
obj_patterns <- list(
  "CO2/IAQ control"              = "\\bco2\\b|concentration|indoor air quality|\\biaq\\b|air quality",
  "Thermal comfort (PMV/PPD)"    = "thermal comfort|\\bpmv\\b|\\bppd\\b|operative temperature|thermal sensation",
  "Energy consumption/saving"    = "energy consumption|energy saving|energy demand|energy efficiency|\\bkwh\\b",
  "Indoor environmental quality" = "indoor environmental quality|\\bieq\\b",
  "Passive cooling"              = "passive cooling|solar chimney|windcatcher|evaporative|earth pipe",
  "Infection/airborne risk"      = "infection risk|airborne|wells.riley|pathogen",
  "Discomfort minimisation"      = "discomfort|overheating|overheating hour|thermal discomfort",
  "Multi-objective performance"  = "multi.objective|pareto|trade.off|multi-objective",
  "Ventilation rate/ACH"         = "ventilation rate|air change|\\bach\\b",
  "Sustainability/policy"        = "sustainability|policy|guideline|certification|standard"
)

# ── Ventilation types ─────────────────────────────────────
vent_patterns <- list(
  "Natural ventilation"    = "natural ventilat",
  "Hybrid/mixed-mode"      = "\\bhybrid\\b|mix.?mode|mixed mode",
  "Mechanical ventilation" = "mechanical ventilat|\\bhvac\\b",
  "Wind-driven"            = "wind.driven|wind tower|windcatcher",
  "Buoyancy-driven"        = "\\bbuoyancy\\b|stack ventilat|solar chimney",
  "Window-based"           = "window open|window control|operable window",
  "Cross ventilation"      = "cross ventilat",
  # "single-" alone also matched single-family / single-zone / single-storey
  "Single-sided"           = "single.sided",
  "Night ventilation"      = "night ventilat",
  "Evaporative cooling"    = "\\bevaporative\\b"
)

# ── Optimisation methods (for C2-C6) ─────────────────────
optim_patterns <- list(
  "CFD simulation"                   = "\\bcfd\\b|computational fluid dynamic",
  "Genetic Algorithm/NSGA"           = "genetic algorithm|\\bnsga\\b|evolutionary",
  "Model Predictive Control (MPC)"   = "model predictive control|\\bmpc\\b",
  "Machine learning / Deep learning" = "machine learning|deep learning|neural network|\\bml\\b|\\bdnn\\b|\\blstm\\b",
  "Parametric optimisation"          = "parametric optim",
  "Multi-objective optimisation"     = "multi.objective|pareto|multi-objective",
  "Rule-based control"               = "rule.based|rule base",
  "Surrogate-based optimisation"     = "\\bsurrogate\\b",
  "Energy simulation"                = "energy simulation|energyplus|designbuilder",
  # Added 2026-09: gap-check categories for the IAQ/TC/EE optimisation
  # literature. Verified document-by-document in S11 (82 records):
  # Bayesian optimisation absent; fuzzy inference in one study
  # (TSK controllers coupled to NSGA-II); reinforcement learning only
  # through a metadata error in one title.
  "Bayesian optimisation"            = "bayesian|gaussian process|kriging",
  "Fuzzy inference/control"          = "\\bfuzzy\\b",
  "Reinforcement learning"           = "reinforcement learning"
)

# ── Building typologies ───────────────────────────────────
building_patterns <- list(
  "Schools / classrooms"   = "\\bschool\\b|classroom|educational building|primary school|secondary school",
  "Office buildings"       = "office building",
  "Residential buildings"  = "residential|dwelling|\\bapartment\\b",
  "High-rise buildings"    = "high.rise|high rise|multistory",
  "University / campus"    = "university|campus",
  "General / unspecified"  = "\\bbuilding\\b"
)

############################################################
# S8.3 — Pattern counting function
############################################################

count_patterns_r <- function(seg_df, patterns, min_count = 1) {
  clusters <- sort(unique(seg_df$cluster))
  results  <- list()
  
  for (c in clusters) {
    text_c <- seg_df %>%
      filter(cluster == c) %>%
      pull(segment) %>%
      str_to_lower() %>%
      paste(collapse = " ")
    
    row <- data.frame(cluster = c)
    for (label in names(patterns)) {
      row[[label]] <- str_count(
        text_c, regex(patterns[[label]], ignore_case = TRUE))
    }
    results[[length(results) + 1]] <- row
  }
  
  bind_rows(results) %>%
    pivot_longer(-cluster,
                 names_to  = "pattern",
                 values_to = "count") %>%
    filter(count >= min_count) %>%
    arrange(cluster, desc(count))
}

############################################################
# S8.4 — Compute all pattern counts
############################################################

cat("\n--- Computing pattern counts ---\n")
obj_counts   <- count_patterns_r(seg, obj_patterns,      min_count = 1)
vent_counts  <- count_patterns_r(seg, vent_patterns,     min_count = 1)
optim_counts <- count_patterns_r(seg, optim_patterns,    min_count = 1)
build_counts <- count_patterns_r(seg, building_patterns, min_count = 3)
cat("All patterns computed\n")

# Top 3 per cluster as formatted string
top3_string <- function(count_df, n = 3) {
  count_df %>%
    group_by(cluster) %>%
    slice_max(count, n = n, with_ties = FALSE) %>%
    summarise(
      summary = paste0(pattern, " (n=", count, ")",
                       collapse = ";\n"),
      .groups = "drop"
    )
}

obj_summary   <- top3_string(obj_counts,   n = 3)
vent_summary  <- top3_string(vent_counts,  n = 3)
optim_summary <- top3_string(optim_counts, n = 3)
build_summary <- top3_string(build_counts, n = 3)

############################################################
# S8.5 — SOURCE 1: Chi-square terms per cluster
############################################################

chi2_positive <- terms %>%
  filter(sign == "positive") %>%
  arrange(cluster, desc(chi2))

# Top 5 per cluster as formatted string
chi2_summary <- chi2_positive %>%
  group_by(cluster) %>%
  slice_max(chi2, n = 5, with_ties = FALSE) %>%
  summarise(
    top5_chi2_terms = paste0(
      term, " (χ²=", round(chi2, 1), ")",
      collapse = "; "),
    .groups = "drop"
  )

# Top 10 per cluster for export
top10_chi2 <- chi2_positive %>%
  group_by(cluster) %>%
  slice_max(chi2, n = 10, with_ties = FALSE) %>%
  ungroup()

############################################################
# S8.6 — Optimisation methods: segment patterns for C1-C6
############################################################

# Until the 2026-09 revision, C1 was overridden with chi-square
# ranked terms. That made the Methods column mix two units
# (chi-square values in C1, pattern-match counts in C2-C6).
# Segment-level counts are now used for every cluster.

optim_hybrid <- optim_summary %>%
  mutate(source = "segment patterns")

############################################################
# S8.7 — Documents per cluster
############################################################

docs_summary <- seg %>%
  group_by(cluster) %>%
  summarise(
    n_segments = n(),
    n_docs     = n_distinct(doc_id),
    doc_ids    = paste(sort(unique(doc_id)), collapse = ", "),
    .groups    = "drop"
  )

############################################################
# S8.8 — Cluster labels and RQ mapping
############################################################

cluster_labels <- tibble(
  cluster    = 1:6,
  label      = c(
    "CFD & computational optimisation",
    "Passive & hybrid cooling strategies",
    "IEQ & energy efficiency in educational buildings",
    "Sustainability policy & design practice",
    "Energy demand & operational control",
    "CO2 monitoring & window control in classrooms"
  ),
  primary_rq = c(
    "RQ3",
    "RQ2",
    "RQ1 + RQ4",
    "RQ4",
    "RQ2 + RQ3",
    "RQ1 + RQ2"
  )
)

############################################################
# S8.9 — Assemble full hybrid taxonomy table
############################################################

taxonomy_hybrid <- docs_summary %>%
  left_join(cluster_labels, by = "cluster") %>%
  left_join(chi2_summary,   by = "cluster") %>%
  left_join(
    obj_summary   %>% rename(key_optim_objectives  = summary),
    by = "cluster") %>%
  left_join(
    vent_summary  %>% rename(ventilation_types     = summary),
    by = "cluster") %>%
  left_join(
    build_summary %>% rename(building_typologies   = summary),
    by = "cluster") %>%
  left_join(
    optim_hybrid  %>% rename(optimisation_methods  = summary,
                             optim_source          = source),
    by = "cluster") %>%
  select(cluster, label, primary_rq,
         n_segments, n_docs,
         top5_chi2_terms,
         key_optim_objectives,
         ventilation_types,
         building_typologies,
         optimisation_methods,
         optim_source,
         doc_ids)

############################################################
# S8.10 — Print summary to console
############################################################

cat("\n========== HYBRID TAXONOMY TABLE ==========\n")
for (c in 1:6) {
  row <- taxonomy_hybrid %>% filter(cluster == c)
  cat("\n=== Cluster", c, "—", row$label, "===\n")
  cat("RQ:", row$primary_rq,
      "| Segments:", row$n_segments,
      "| Docs:", row$n_docs, "\n")
  cat("Top chi-square terms:\n  ",
      row$top5_chi2_terms, "\n")
  cat("Key optimization objectives:\n  ",
      row$key_optim_objectives, "\n")
  cat("Ventilation types:\n  ",
      row$ventilation_types, "\n")
  cat("Building typologies:\n  ",
      row$building_typologies, "\n")
  cat("Optimisation methods (", row$optim_source, "):\n  ",
      row$optimisation_methods, "\n")
}

############################################################
# S8.11 — Source annotation table
############################################################

source_notes <- tibble(
  column    = c(
    "cluster", "label", "primary_rq",
    "top5_chi2_terms",
    "key_optim_objectives",
    "ventilation_types",
    "building_typologies",
    "optimisation_methods"
  ),
  source    = c(
    "TALL Reinert DHC",
    "Expert interpretation of chi-square terms",
    "Expert mapping to RQ1-RQ4",
    "Chi-square ranked positive terms (TALL Terms export)",
    "Segment-level regex pattern matching (TALL Segments export)",
    "Segment-level regex pattern matching (TALL Segments export)",
    "Segment-level regex pattern matching (TALL Segments export)",
    "Segment-level regex pattern matching (TALL Segments export)"
  ),
  rationale = c(
    "Cluster number from Reinert DHC solution (k=6)",
    "Label derived from top chi-square terms and central documents",
    "Mapped to research questions RQ1-RQ4 (Section 1.3)",
    "Top 5 terms by chi-square — statistically discriminant vocabulary",
    "Pattern counts in segments — optimization objective vocabulary",
    "Pattern counts in segments — ventilation terms are transversal",
    "Pattern counts in segments — building terms are transversal",
    "Pattern counts in segments — single unit, comparable across clusters"
  )
)

############################################################
# S8.12 — Export all outputs
############################################################

# Main Excel with all sheets
write_xlsx(
  list(
    "Hybrid_taxonomy"        = taxonomy_hybrid %>% select(-doc_ids),
    "Source_notes"           = source_notes,
    "Chi2_terms_positive"    = chi2_positive,
    "Top10_chi2_per_cluster" = top10_chi2,
    "Objectives_counts"      = obj_counts,
    "Ventilation_counts"     = vent_counts,
    "Optimisation_counts"    = optim_counts,
    "Building_counts"        = build_counts,
    "Docs_per_cluster"       = docs_summary
  ),
  "S9_taxonomy_hybrid_summary.xlsx"
)
cat("\n[Saved] S9_taxonomy_hybrid_summary.xlsx\n")

# Individual CSVs
write.csv(taxonomy_hybrid,  "S9_taxonomy_hybrid_table.csv",   row.names = FALSE)
write.csv(chi2_positive,    "S9_taxonomy_chi2_terms.csv",     row.names = FALSE)
write.csv(obj_counts,       "S9_taxonomy_objectives.csv",     row.names = FALSE)
write.csv(vent_counts,      "S9_taxonomy_ventilation.csv",    row.names = FALSE)
write.csv(optim_counts,     "S9_taxonomy_optimisation.csv",   row.names = FALSE)
write.csv(build_counts,     "S9_taxonomy_building.csv",       row.names = FALSE)

cat("[Saved] S9_taxonomy_hybrid_table.csv\n")
cat("[Saved] S9_taxonomy_chi2_terms.csv\n")
cat("[Saved] S9_taxonomy_objectives.csv\n")
cat("[Saved] S9_taxonomy_ventilation.csv\n")
cat("[Saved] S9_taxonomy_optimisation.csv\n")
cat("[Saved] S9_taxonomy_building.csv\n")

############################################################
# S8.13 — Final summary
############################################################

cat("\n========== STEP 8 SUMMARY ==========\n")
cat("Segments analysed:      ", nrow(seg), "\n")
cat("Documents analysed:     ", n_distinct(seg$doc_id), "\n")
cat("Clusters:                6\n")
cat("\nPattern dictionaries:\n")
cat("  Optimization objectives:", length(obj_patterns),   "patterns\n")
cat("  Ventilation types:      ", length(vent_patterns),  "patterns\n")
cat("  Optimisation methods:   ", length(optim_patterns), "patterns\n")
cat("  Building typologies:    ", length(building_patterns), "patterns\n")
cat("\nColumn sources:\n")
cat("  Cluster labels:          expert interpretation\n")
cat("  Primary RQ:              expert mapping RQ1-RQ4\n")
cat("  Top chi-square terms:    TALL Terms export\n")
cat("  Key optim. objectives:   TALL Segments (pattern matching)\n")
cat("  Ventilation types:       TALL Segments (pattern matching)\n")
cat("  Building typologies:     TALL Segments (pattern matching)\n")
cat("  Optimisation methods:    segment patterns (C1-C6)\n")
cat("\nOutputs:\n")
cat("  S9_taxonomy_hybrid_summary.xlsx  (9 sheets)\n")
cat("  S9_taxonomy_hybrid_table.csv\n")
cat("  S9_taxonomy_chi2_terms.csv\n")
cat("  S9_taxonomy_objectives.csv\n")
cat("  S9_taxonomy_ventilation.csv\n")
cat("  S9_taxonomy_optimisation.csv\n")
cat("  S9_taxonomy_building.csv\n")
cat("=====================================\n")
cat("\nMain output for Table 3:\n")
cat("  Sheet 'Hybrid_taxonomy' in S9_taxonomy_hybrid_summary.xlsx\n")

############################################################
# S8.14 — Regression check against the published Table 3
############################################################
# Anchors verified before and after the 2026-09 revision.
# A FAIL means a pattern or an input file has changed and the
# published values are no longer reproduced.

anchors <- tribble(
  ~cluster, ~block,     ~pattern,                        ~expected,
  3,        "segments", NA_character_,                    129,
  5,        "segments", NA_character_,                    122,
  6,        "segments", NA_character_,                    121,
  3,        "optim",    "Multi-objective optimisation",     8,
  3,        "optim",    "Energy simulation",                6,
  3,        "optim",    "CFD simulation",                   4,
  5,        "optim",    "Energy simulation",               12,
  5,        "optim",    "Rule-based control",               9,
  5,        "optim",    "Model Predictive Control (MPC)",   6,
  6,        "optim",    "CFD simulation",                   6,
  6,        "optim",    "Energy simulation",                5,
  6,        "optim",    "Genetic Algorithm/NSGA",           2,
  3,        "obj",      "CO2/IAQ control",                145,
  6,        "obj",      "CO2/IAQ control",                133,
  3,        "vent",     "Natural ventilation",             66,
  6,        "build",    "Schools / classrooms",            78,
  1,        "optim",    "Bayesian optimisation",            0,
  3,        "optim",    "Bayesian optimisation",            0,
  5,        "optim",    "Bayesian optimisation",            0,
  6,        "optim",    "Bayesian optimisation",            0
)

get_value <- function(block, cl, pat) {
  if (block == "segments") {
    return(docs_summary$n_segments[docs_summary$cluster == cl])
  }
  df <- switch(block,
               obj   = obj_counts,
               vent  = vent_counts,
               optim = optim_counts,
               build = build_counts)
  v <- df$count[df$cluster == cl & df$pattern == pat]
  if (length(v) == 0) 0L else v
}

check <- anchors %>%
  rowwise() %>%
  mutate(observed = get_value(block, cluster, pattern),
         status   = if_else(observed == expected, "OK", "FAIL")) %>%
  ungroup()

cat("\n========== REGRESSION CHECK (Table 3) ==========\n")
print(as.data.frame(check), row.names = FALSE)

if (any(check$status == "FAIL")) {
  warning("Regression check FAILED: published Table 3 values are not reproduced.")
} else {
  cat("All anchors reproduced.\n")
}

############################################################
# S8.15 — Methods cell for every cluster (Table 3, Methods)
############################################################
# Ready-to-paste strings; clusters with no match print as "—".

methods_cells <- tibble(cluster = 1:6) %>%
  left_join(optim_hybrid %>% select(cluster, summary), by = "cluster") %>%
  mutate(
    summary = str_replace_all(summary, ";\n", "; "),
    summary = str_replace_all(summary, " \\(n=", " ("),
    summary = if_else(is.na(summary), "—", summary)
  )

cat("\n========== METHODS CELLS (Table 3) ==========\n")
for (i in seq_len(nrow(methods_cells))) {
  cat("C", methods_cells$cluster[i], ": ",
      methods_cells$summary[i], "\n", sep = "")
}

