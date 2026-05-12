############################################################
# STEP 3 — Eligibility screening (Gates 0–3) + final corpus
# Input:  Step_2_semantic_JW.RData
# Output: Step_3_final_corpus_M.RData  →  M_Step 3 (~94u)
# Author: Antonio Sánchez Cordero
############################################################

library(bibliometrix); library(dplyr); library(stringr)

# ── Carga ─────────────────────────────────────────────────
load("Step_2_semantic_JW.RData")   # objeto M
df <- M; rm(M)
stopifnot("TI" %in% names(df))
if (!"AB" %in% names(df)) df$AB <- NA_character_
cat("Input records:", nrow(df), "\n")

# ── 3.1  Build analysis texts (NA seguro) ─────────────────
df <- df %>%
  mutate(
    TI_txt = str_squish(str_to_lower(as.character(TI))),
    AB_txt = str_squish(str_to_lower(
      ifelse(is.na(AB), "", as.character(AB)))),
    TEXT_CORE  = str_squish(paste(TI_txt, AB_txt)),
    TEXT_QUANT = if_else(nchar(AB_txt) > 0, AB_txt, TEXT_CORE)
  )

# ── 3.2  Pattern dictionaries ─────────────────────────────
pat_V_strict <- regex(
  paste(c(
    "natural\\s+ventilat",        "naturally\\s+ventilat",
    "hybrid\\s+ventilat",         "mixed[- ]mode",
    "mixed\\s+mode",              "free[- ]running",
    "operable\\s+window",         "window\\s+open",
    "window\\s+opening",          "window\\s+operation",
    "window\\s+control",          "controlled\\s+window",
    "window\\s+opening\\s+behavio","cross\\s+ventilat",
    "single[- ]sided",            "stack\\s+ventilat",
    "night\\s+ventilat",          "ventilative\\s+cooling"
  ), collapse = "|"),
  ignore_case = TRUE
)

pat_V_any <- regex(
  paste(c(
    "\\bventilat",      "air\\s*flow",
    "airflow",          "air\\s*exchange",
    "air\\s*change",    "\\bach\\b",
    "ventilation\\s*rate"
  ), collapse = "|"),
  ignore_case = TRUE
)

pat_HVAC <- regex(
  paste(c(
    "mechanical\\s+ventilat",  "\\bhvac\\b",
    "air\\s+conditioning",     "\\bvav\\b",
    "\\bvrf\\b",               "fan\\s*coil",
    "heat\\s+pump",            "chiller",
    "\\bahu\\b",               "\\bhrv\\b",
    "\\berv\\b",               "ventilation\\s+system"
  ), collapse = "|"),
  ignore_case = TRUE
)

pat_CFD <- regex(
  paste(c(
    "\\bcfd\\b",   "computational\\s+fluid\\s+dynamics",
    "fluent",      "openfoam",
    "ansys"
  ), collapse = "|"),
  ignore_case = TRUE
)

pat_O_pos <- regex(
  paste(c(
    "optim",                       "multi[- ]objective",
    "pareto",                      "nsga",
    "genetic\\s+algorithm",        "\\bga\\b",
    "evolutionary\\s+algorithm",   "meta[- ]heuristic",
    "particle\\s+swarm",           "\\bpso\\b",
    "mopcso",                      "bayesian\\s+optim",
    "surrogate",                   "model\\s+predictive\\s+control",
    "\\bmpc\\b",                   "optimal\\s+control",
    "control\\s+strateg",          "supervisory\\s+control",
    "rule[- ]based",               "fuzzy",
    "adaptive\\s+control"
  ), collapse = "|"),
  ignore_case = TRUE
)

pat_P_pos <- regex(
  paste(c(
    "\\biaq\\b",                "indoor\\s+air\\s+quality",
    "\\bco2\\b",                "carbon\\s+dioxide",
    "ventilation\\s+rate",      "air\\s+change",
    "\\bach\\b",                "infection\\s+risk",
    "wells[- ]riley",           "thermal\\s+comfort",
    "\\bpmv\\b",                "\\bppd\\b",
    "operative\\s+temperature", "overheating",
    "adaptive\\s+comfort",      "\\benergy\\b",
    "energy\\s+use",            "energy\\s+consumption",
    "cooling\\s+load",          "heating\\s+load",
    "energy\\s+performance",    "\\bkwh\\b"
  ), collapse = "|"),
  ignore_case = TRUE
)

pat_Q_pos <- regex(
  paste(c(
    "objective\\s+function",   "constraint",
    "trade[- ]off",            "pareto",
    "simulate",                "simulation",
    "modelled",                "modeled",
    "evaluate",                "evaluation",
    "assess",                  "assessment",
    "quantif",                 "computed",
    "calculated",              "predic",
    "\\d+\\s*(ppm|kwh|mwh|wh|kw|w|ach|h\\-1|%|°c|c|k|l/s|ls\\-1|m3/s|m\\^3/s|pa)"
  ), collapse = "|"),
  ignore_case = TRUE
)

# ── 3.3  Compute flags ────────────────────────────────────
Step_3 <- df %>%
  mutate(
    has_V_strict = str_detect(TEXT_CORE, pat_V_strict),
    has_V_any    = str_detect(TEXT_CORE, pat_V_any) |
      str_detect(TEXT_CORE, pat_V_strict),
    has_HVAC     = str_detect(TEXT_CORE, pat_HVAC),
    has_CFD      = str_detect(TEXT_CORE, pat_CFD),
    has_O        = str_detect(TEXT_CORE, pat_O_pos),
    has_P        = str_detect(TEXT_CORE, pat_P_pos),
    has_Q        = str_detect(TEXT_QUANT, pat_Q_pos) |
      str_detect(AB_txt, "\\d")
  )

# ── 3.4  Gate logic ───────────────────────────────────────
Step_3 <- Step_3 %>%
  mutate(
    ex_g0_hvac   = has_HVAC & !has_V_strict,
    ex_g0_cfd    = has_CFD  & !has_O,
    ex_g1_noVent = !has_V_any & !has_V_strict,
    
    Decision = case_when(
      ex_g0_hvac                           ~ "AUTO-EXCLUDE",
      ex_g0_cfd                            ~ "AUTO-EXCLUDE",
      ex_g1_noVent                         ~ "AUTO-EXCLUDE",
      has_V_any & !has_V_strict            ~ "TO_REVIEW",
      !has_O                               ~ "TO_REVIEW",
      !(has_P & has_Q)                     ~ "TO_REVIEW",
      has_V_strict & has_O & has_P & has_Q ~ "AUTO-INCLUDE",
      TRUE                                 ~ "TO_REVIEW"
    ),
    
    Reason = case_when(
      ex_g0_hvac                           ~ "EX-G0.1 HVAC-only",
      ex_g0_cfd                            ~ "EX-G0.2 CFD-only",
      ex_g1_noVent                         ~ "EX-G1.0 No ventilation signal",
      has_V_any & !has_V_strict            ~ "REV-G1 Scope unclear",
      !has_O                               ~ "REV-G2 No optimisation",
      !(has_P & has_Q)                     ~ "REV-G3 Insufficient performance",
      has_V_strict & has_O & has_P & has_Q ~ "IN-G3 Full criteria met",
      TRUE                                 ~ "TO_REVIEW"
    )
  )

cat("AUTO-INCLUDE:", sum(Step_3$Decision == "AUTO-INCLUDE"), "\n")
cat("AUTO-EXCLUDE:", sum(Step_3$Decision == "AUTO-EXCLUDE"), "\n")
cat("TO_REVIEW:   ", sum(Step_3$Decision == "TO_REVIEW"),    "\n")

# ── 3.5  Exportar TO_REVIEW para Zotero (RIS) ────────────
Step_3_to_review <- Step_3 %>% filter(Decision == "TO_REVIEW")

if (nrow(Step_3_to_review) > 0) {
  .safe_val <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x) | !nzchar(x), "", x)
  }
  df_to_ris <- function(df) {
    out <- character(0)
    for (i in seq_len(nrow(df))) {
      row   <- df[i, , drop = FALSE]
      entry <- "TY  - JOUR"
      if ("AU" %in% names(df)) {
        au_raw  <- .safe_val(row[["AU"]])
        if (nzchar(au_raw)) {
          au_list <- trimws(strsplit(au_raw, ";")[[1]])
          au_list <- au_list[nzchar(au_list)]
          entry   <- c(entry, paste0("AU  - ", au_list))
        }
      }
      for (fld in list(c("TI","TI  - "), c("SO","JO  - "),
                       c("PY","PY  - "), c("VL","VL  - "),
                       c("IS","IS  - "), c("DI","DO  - "),
                       c("AB","AB  - "))) {
        if (fld[1] %in% names(df)) {
          v <- .safe_val(row[[fld[1]]])
          if (nzchar(v)) entry <- c(entry, paste0(fld[2], v))
        }
      }
      entry <- c(entry, "ER  - ")
      out   <- c(out, entry)
    }
    out
  }
  writeLines(df_to_ris(Step_3_to_review), "Step_3_to_review.ris")
  cat("RIS exportado:", nrow(Step_3_to_review), "registros TO_REVIEW\n")
}

# ── 3.6  Decisiones manuales POR DOI ─────────────────────
manual_include_dois <- c(
  "10.1016/j.energy.2019.02.035",
  "10.1016/j.solener.2021.07.008",
  "10.1016/j.enbuild.2015.04.033",
  "10.1016/j.renene.2021.05.004",
  "10.3390/su15129168",
  "10.1016/j.buildenv.2020.106994",
  "10.1007/s12273-023-0992-6",
  "10.3390/buildings11120595",
  "10.3390/en12244607",
  "10.1016/j.applthermaleng.2021.116654",
  "10.1016/j.buildenv.2022.109688",
  "10.1016/j.jobe.2021.103108"
)

Step_3 <- Step_3 %>%
  mutate(
    DI_norm = str_squish(str_to_lower(as.character(DI))),
    Include_manual = case_when(
      Decision == "TO_REVIEW" &
        DI_norm %in% str_to_lower(manual_include_dois) ~ 1L,
      Decision == "TO_REVIEW" ~ 0L,
      TRUE ~ NA_integer_
    ),
    Include_final = case_when(
      Decision == "AUTO-INCLUDE" ~ 1L,
      Decision == "AUTO-EXCLUDE" ~ 0L,
      Decision == "TO_REVIEW"    ~ Include_manual,
      TRUE                       ~ NA_integer_
    )
  ) %>%
  select(-DI_norm)

# ── 3.7  Corpus final ─────────────────────────────────────
Step_3_corpus_final <- Step_3 %>% filter(Include_final == 1L)
cat("Final corpus:", nrow(Step_3_corpus_final),
    "records [target: ~94]\n")

unresolved <- Step_3 %>%
  filter(Decision == "TO_REVIEW", is.na(Include_final)) %>%
  nrow()
if (unresolved > 0)
  warning("TO_REVIEW sin decisión manual: ", unresolved,
          " registros. Verificar DOIs en manual_include_dois.")



# ── 3.8  Guardar objeto M limpio para Biblioshiny / TALL ──
cols_auxiliares <- c(
  "TI_txt", "AB_txt", "TEXT_CORE", "TEXT_QUANT",
  "has_V_strict", "has_V_any", "has_HVAC", "has_CFD",
  "has_O", "has_P", "has_Q", "has_Q0",
  "ex_g0_hvac", "ex_g0_cfd", "ex_g1_noVent",
  "Include_manual", "Include_final", "Decision", "Reason",
  "TI_orig"
)

# 1) Eliminar columnas auxiliares
cols_presentes <- intersect(cols_auxiliares, names(Step_3_corpus_final))
M <- Step_3_corpus_final[, !names(Step_3_corpus_final) %in% cols_presentes,
                         drop = FALSE]

# 2) Convertir a data.frame base (elimina tibble/tbl_df)
M <- as.data.frame(M, stringsAsFactors = FALSE)

# 3) Limpiar row names
row.names(M) <- NULL

# 4) Verificar campos mínimos requeridos por bibliometrix
campos_req <- c("AU", "TI", "SO", "DE", "ID", "AB",
                "C1", "RP", "CR", "PY", "VL", "IS",
                "TC", "DI", "DB", "UT")
for (campo in campos_req) {
  if (!campo %in% names(M)) {
    M[[campo]] <- NA_character_
    cat("[INFO] Campo añadido con NA:", campo, "\n")
  }
}

# 5) Asignar clase en el orden exacto que espera bibliometrix
class(M) <- c("bibliometrixDB", "data.frame")

# 6) Verificación antes de guardar
cat("Clase M:", paste(class(M), collapse = ", "), "\n")
cat("Dimensiones M:", nrow(M), "x", ncol(M), "\n")
stopifnot(inherits(M, "bibliometrixDB"))
stopifnot(nrow(M) > 0)

# 7) Guardar
save(M, file = "Step_3_final_corpus_M.RData")
cat("[Saved] Step_3_final_corpus_M.RData —",
    nrow(M), "records,", ncol(M), "cols\n")

# Guardar también log y CSV de auditoría
save(Step_3, Step_3_corpus_final,
     file = "Step_3_gates_outputs.RData")
write.csv(
  Step_3[, intersect(c("DI","TI","PY","SO","Decision","Reason",
                       "Include_manual","Include_final"),
                     names(Step_3))],
  "Step_3_final_decisions_log.csv", row.names = FALSE
)
write.csv(M, "Step_3_final_corpus.csv", row.names = FALSE)

biblioshiny()
