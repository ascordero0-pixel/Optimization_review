############################################################
# SCRIPT S11 — Descriptive analysis of S11 extraction form
# Input:  S11_Paper analysis data extraction_82.xlsx (sheet "S11_Extraction")
# Output: S12_descriptive_results.xlsx  (11 sheets)
#         S12_fig1_optim_methods.png
#         S12_fig2_typology_climate.png
#         S12_fig3_co2_thresholds.png
#         S12_fig4_energy_saving.png
############################################################

library(readxl); library(dplyr); library(ggplot2)
library(tidyr);  library(writexl); library(forcats)

# ── 0. Load and clean ─────────────────────────────────────────────
df <- read_excel("S11_Paper analysis data extraction_82.xlsx",
                 sheet = "S11_Extraction", skip = 1)

# Remove header duplicate row if present
df <- df[!is.na(df$doc_id) & df$doc_id != "doc_id", ]

# Type conversions
df$Year                <- as.integer(df$Year)
df$CO2_threshold_ppm   <- as.numeric(df$CO2_threshold_ppm)
df$PMV_target_min      <- as.numeric(df$PMV_target_min)
df$PMV_target_max      <- as.numeric(df$PMV_target_max)
df$Energy_saving_pct   <- as.numeric(df$Energy_saving_pct)

n_total <- nrow(df)
n_extracted <- sum(!is.na(df$Building_typology))  # filled rows
cat("Total records:", n_total, "| Rows with data:", n_extracted, "\n")

# ── Helper: frequency table → dataframe ──────────────────────────
freq_table <- function(vec, var_name) {
  tbl <- table(vec, useNA = "ifany")
  df_out <- data.frame(
    Value    = names(tbl),
    Count    = as.integer(tbl),
    Pct      = round(100 * as.integer(tbl) / sum(tbl), 1),
    stringsAsFactors = FALSE
  )
  df_out$Value[is.na(df_out$Value)] <- "(Missing)"
  df_out <- df_out[order(-df_out$Count), ]
  df_out <- rbind(
    data.frame(Value = var_name, Count = NA, Pct = NA),
    df_out,
    data.frame(Value = "TOTAL", Count = sum(df_out$Count), Pct = 100)
  )
  return(df_out)
}

# ── Helper: numeric summary → dataframe ──────────────────────────
num_summary <- function(vec, var_name) {
  s <- summary(vec, na.rm = TRUE)
  data.frame(
    Variable = var_name,
    N_valid  = sum(!is.na(vec)),
    N_missing= sum(is.na(vec)),
    Min      = round(min(vec, na.rm=TRUE), 2),
    Q1       = round(quantile(vec, 0.25, na.rm=TRUE), 2),
    Median   = round(median(vec, na.rm=TRUE), 2),
    Mean     = round(mean(vec, na.rm=TRUE), 2),
    Q3       = round(quantile(vec, 0.75, na.rm=TRUE), 2),
    Max      = round(max(vec, na.rm=TRUE), 2),
    SD       = round(sd(vec, na.rm=TRUE), 2)
  )
}

# ══════════════════════════════════════════════════════════════════
# BLOCK 0 — CORPUS OVERVIEW
# ══════════════════════════════════════════════════════════════════
b0_year    <- freq_table(df$Year, "Year of publication")
b0_journal <- freq_table(df$Journal, "Journal")

b0_overview <- data.frame(
  Metric = c("Total records in corpus",
             "Records with extraction data",
             "Records pending extraction",
             "Year range",
             "Unique journals"),
  Value  = c(n_total,
             n_extracted,
             n_total - n_extracted,
             paste0(min(df$Year, na.rm=TRUE), "–", max(df$Year, na.rm=TRUE)),
             length(unique(df$Journal[!is.na(df$Journal)])))
)

# ══════════════════════════════════════════════════════════════════
# BLOCK 1 — CONTEXT
# ══════════════════════════════════════════════════════════════════
b1_typology   <- freq_table(df$Building_typology,  "Building typology")
b1_climate    <- freq_table(df$Climate_zone,        "Climate zone")
b1_country    <- freq_table(df$Country,             "Country")
b1_study_type <- freq_table(df$Study_type,          "Study type")
b1_sim_tool   <- freq_table(df$Simulation_tool,     "Simulation tool")
b1_validation <- freq_table(df$Validation_method,   "Validation method")

b1_all <- bind_rows(
  b1_typology, data.frame(Value=NA, Count=NA, Pct=NA),
  b1_climate,  data.frame(Value=NA, Count=NA, Pct=NA),
  b1_country,  data.frame(Value=NA, Count=NA, Pct=NA),
  b1_study_type, data.frame(Value=NA, Count=NA, Pct=NA),
  b1_sim_tool, data.frame(Value=NA, Count=NA, Pct=NA),
  b1_validation
)

# ══════════════════════════════════════════════════════════════════
# BLOCK 2 — IAQ
# ══════════════════════════════════════════════════════════════════
b2_metric   <- freq_table(df$IAQ_metric_primary,  "IAQ primary metric")
b2_co2_meth <- freq_table(df$CO2_method,           "CO2 measurement method")
b2_standard <- freq_table(df$IAQ_standard_ref,     "IAQ standard referenced")
b2_sec_met  <- freq_table(df$IAQ_secondary_metric, "IAQ secondary metric")
b2_freq     <- freq_table(df$IAQ_monitoring_freq,  "Monitoring frequency")
b2_co2_num  <- num_summary(df$CO2_threshold_ppm,   "CO2 threshold (ppm)")

b2_all <- bind_rows(
  b2_metric,   data.frame(Value=NA, Count=NA, Pct=NA),
  b2_co2_meth, data.frame(Value=NA, Count=NA, Pct=NA),
  b2_standard, data.frame(Value=NA, Count=NA, Pct=NA),
  b2_sec_met,  data.frame(Value=NA, Count=NA, Pct=NA),
  b2_freq
)

# ══════════════════════════════════════════════════════════════════
# BLOCK 3 — THERMAL COMFORT
# ══════════════════════════════════════════════════════════════════
b3_metric   <- freq_table(df$TC_metric_primary, "TC primary metric")
b3_model    <- freq_table(df$TC_comfort_model,  "Comfort model / standard")
b3_feedback <- freq_table(df$Occupant_feedback, "Occupant feedback")
b3_season   <- freq_table(df$TC_season_focus,   "Season of analysis")
b3_pmv_min  <- num_summary(df$PMV_target_min,   "PMV target min (or temp °C)")
b3_pmv_max  <- num_summary(df$PMV_target_max,   "PMV target max (or temp °C)")

b3_all <- bind_rows(
  b3_metric,   data.frame(Value=NA, Count=NA, Pct=NA),
  b3_model,    data.frame(Value=NA, Count=NA, Pct=NA),
  b3_feedback, data.frame(Value=NA, Count=NA, Pct=NA),
  b3_season
)

# ══════════════════════════════════════════════════════════════════
# BLOCK 4 — ENERGY PERFORMANCE
# ══════════════════════════════════════════════════════════════════
b4_metric   <- freq_table(df$Energy_metric,   "Energy metric")
b4_baseline <- freq_table(df$Energy_baseline, "Energy baseline")
b4_retrofit <- freq_table(df$Retrofit_included,"Retrofit included")
b4_period   <- freq_table(df$Energy_sim_period,"Simulation period")
b4_saving   <- num_summary(df$Energy_saving_pct, "Energy saving (%)")

b4_all <- bind_rows(
  b4_metric,   data.frame(Value=NA, Count=NA, Pct=NA),
  b4_baseline, data.frame(Value=NA, Count=NA, Pct=NA),
  b4_retrofit, data.frame(Value=NA, Count=NA, Pct=NA),
  b4_period
)

# ══════════════════════════════════════════════════════════════════
# BLOCK 5 — OPTIMISATION & CONTROL
# ══════════════════════════════════════════════════════════════════
b5_vent     <- freq_table(df$Ventilation_type,    "Ventilation type")
b5_method   <- freq_table(df$Optim_method,        "Optimisation method")
b5_formul   <- freq_table(df$Problem_formulation, "Problem formulation")
b5_decvar   <- freq_table(df$Decision_var_primary,"Primary decision variable")
b5_surrogate<- freq_table(df$Surrogate_model,     "Surrogate model")
b5_feedback <- freq_table(df$Performance_feedback,"Performance feedback")
b5_stoch    <- freq_table(df$Stochastic_occupancy,"Stochastic occupancy")

b5_all <- bind_rows(
  b5_vent,      data.frame(Value=NA, Count=NA, Pct=NA),
  b5_method,    data.frame(Value=NA, Count=NA, Pct=NA),
  b5_formul,    data.frame(Value=NA, Count=NA, Pct=NA),
  b5_decvar,    data.frame(Value=NA, Count=NA, Pct=NA),
  b5_surrogate, data.frame(Value=NA, Count=NA, Pct=NA),
  b5_feedback,  data.frame(Value=NA, Count=NA, Pct=NA),
  b5_stoch
)

# ══════════════════════════════════════════════════════════════════
# CROSS-TABLES
# ══════════════════════════════════════════════════════════════════

# Typology × Optimisation method
cross_typo_optim <- df %>%
  filter(!is.na(Building_typology), !is.na(Optim_method)) %>%
  count(Building_typology, Optim_method) %>%
  pivot_wider(names_from = Optim_method, values_from = n, values_fill = 0)

# Climate × Ventilation type
cross_clim_vent <- df %>%
  filter(!is.na(Climate_zone), !is.na(Ventilation_type)) %>%
  count(Climate_zone, Ventilation_type) %>%
  pivot_wider(names_from = Ventilation_type, values_from = n, values_fill = 0)

# Study type × Surrogate model
cross_study_surrogate <- df %>%
  filter(!is.na(Study_type), !is.na(Surrogate_model)) %>%
  count(Study_type, Surrogate_model) %>%
  pivot_wider(names_from = Surrogate_model, values_from = n, values_fill = 0)

# Numeric summaries combined sheet
numeric_summaries <- bind_rows(
  b2_co2_num,
  b3_pmv_min,
  b3_pmv_max,
  b4_saving
)

# ══════════════════════════════════════════════════════════════════
# EXPORT TO EXCEL
# ══════════════════════════════════════════════════════════════════
write_xlsx(
  list(
    "B0_Corpus_Overview"     = b0_overview,
    "B0_Year_Journal"        = bind_rows(b0_year, data.frame(Value=NA,Count=NA,Pct=NA), b0_journal),
    "B1_Context"             = b1_all,
    "B2_IAQ"                 = b2_all,
    "B3_Thermal_Comfort"     = b3_all,
    "B4_Energy"              = b4_all,
    "B5_Optimisation"        = b5_all,
    "Numeric_Summaries"      = numeric_summaries,
    "Cross_Typology_Optim"   = cross_typo_optim,
    "Cross_Climate_Vent"     = cross_clim_vent,
    "Cross_Study_Surrogate"  = cross_study_surrogate
  ),
  "S12_descriptive_results.xlsx"
)
cat("✅ S12_descriptive_results.xlsx saved — 11 sheets\n")

# ══════════════════════════════════════════════════════════════════
# FIGURES
# ══════════════════════════════════════════════════════════════════

# Fig 1 — Optimisation methods frequency
df_p1 <- df %>%
  filter(!is.na(Optim_method)) %>%
  count(Optim_method) %>%
  mutate(Optim_method = fct_reorder(Optim_method, n))

ggplot(df_p1, aes(x = Optim_method, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_gradient(low = "#BDD7EE", high = "#1F5C99") +
  labs(title = "Distribution of optimisation methods (n=82)",
       subtitle = "Corpus: IAQ, TC and energy in natural/hybrid ventilation studies",
       x = NULL, y = "Number of studies") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey50")) +
  expand_limits(y = max(df_p1$n) * 1.15)

ggsave("S12_fig1_optim_methods.png", width = 9, height = 5, dpi = 300)
cat("✅ S12_fig1_optim_methods.png saved\n")

# Fig 2 — Building typology × Climate zone
df_p2 <- df %>%
  filter(!is.na(Building_typology), !is.na(Climate_zone)) %>%
  count(Building_typology, Climate_zone)

ggplot(df_p2, aes(x = Building_typology, y = n, fill = Climate_zone)) +
  geom_col(position = "stack") +
  labs(title = "Building typology by climate zone",
       x = "Building typology", y = "Number of studies",
       fill = "Climate zone") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set2")

ggsave("S12_fig2_typology_climate.png", width = 9, height = 5, dpi = 300)
cat("✅ S12_fig2_typology_climate.png saved\n")

# Fig 3 — CO2 threshold distribution (boxplot + jitter)
df_p3 <- df %>% filter(!is.na(CO2_threshold_ppm))

if(nrow(df_p3) >= 3) {
  ggplot(df_p3, aes(x = "", y = CO2_threshold_ppm)) +
    geom_boxplot(fill = "#FFF2CC", colour = "#7F6000", width = 0.4) +
    geom_jitter(width = 0.1, alpha = 0.6, colour = "#7F6000", size = 2) +
    geom_hline(yintercept = 1000, linetype = "dashed",
               colour = "red", linewidth = 0.7) +
    annotate("text", x = 1.35, y = 1020,
             label = "1000 ppm (EN 16798 Cat. II)", colour = "red", size = 3) +
    labs(title = "Distribution of CO\u2082 thresholds across corpus",
         x = NULL, y = "CO\u2082 threshold (ppm)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  ggsave("S12_fig3_co2_thresholds.png", width = 6, height = 5, dpi = 300)
  cat("✅ S12_fig3_co2_thresholds.png saved\n")
} else {
  cat("⚠️  Insufficient CO2 data for Fig 3 — will generate after more extraction\n")
}

# Fig 4 — Energy saving distribution
df_p4 <- df %>% filter(!is.na(Energy_saving_pct))

if(nrow(df_p4) >= 3) {
  ggplot(df_p4, aes(x = Energy_saving_pct)) +
    geom_histogram(binwidth = 5, fill = "#DAEEF3",
                   colour = "#17375E", boundary = 0) +
    geom_vline(xintercept = median(df_p4$Energy_saving_pct),
               linetype = "dashed", colour = "#17375E", linewidth = 0.8) +
    annotate("text",
             x = median(df_p4$Energy_saving_pct) + 2,
             y = Inf, vjust = 2,
             label = paste0("Median = ", round(median(df_p4$Energy_saving_pct),1), "%"),
             colour = "#17375E", size = 3.5) +
    labs(title = "Distribution of reported energy savings (%)",
         x = "Energy saving (%)", y = "Number of studies") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  ggsave("S12_fig4_energy_saving.png", width = 8, height = 5, dpi = 300)
  cat("✅ S12_fig4_energy_saving.png saved\n")
} else {
  cat("⚠️  Insufficient energy saving data for Fig 4 — will generate after more extraction\n")
}

cat("\n── S12 complete ──\n")
