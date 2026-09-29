############################################################
# ALLUVIAL DIAGRAM — Keyword mapping onto thematic axes
# Illustrates interconnections between:
#   - Top keywords (from Table 2)
#   - Reinert clusters (semantic layer)
#   - Thematic axes (i-vi)
#
# Tools: ggplot2 + ggalluvial
# Output: alluvial_diagram.png / alluvial_diagram.pdf
#
# Author: Antonio Sánchez Cordero
# Corrections v2: CO2 symbol fix + improved label sizes
############################################################

rm(list = ls())
graphics.off()

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(stringr)

############################################################
# DATA — Keyword → Cluster → Thematic Axis mapping
############################################################

data <- tribble(
  ~keyword,                      ~cluster,                                      ~axis,                                      ~freq,
  
  # ── Cluster 1 — CFD and computational optimisation ────
  "Genetic algorithm",           "C1: CFD & computational\noptimisation",       "(i) Optimisation &\ncontrol methods",       9,
  "Optimization",                "C1: CFD & computational\noptimisation",       "(i) Optimisation &\ncontrol methods",       22,
  "Simulation",                  "C1: CFD & computational\noptimisation",       "(v) Modelling &\nsimulation methods",       14,
  "CFD",                         "C1: CFD & computational\noptimisation",       "(v) Modelling &\nsimulation methods",       8,
  "Machine learning",            "C1: CFD & computational\noptimisation",       "(i) Optimisation &\ncontrol methods",       6,
  
  # ── Cluster 2 — Passive and hybrid cooling ────────────
  "Natural ventilation",         "C2: Passive & hybrid\ncooling strategies",    "(ii) Ventilation\nstrategies",              44,
  "Ventilation",                 "C2: Passive & hybrid\ncooling strategies",    "(ii) Ventilation\nstrategies",              11,
  "Design",                      "C2: Passive & hybrid\ncooling strategies",    "(vi) Design &\noperational variables",      11,
  "Performance",                 "C2: Passive & hybrid\ncooling strategies",    "(v) Modelling &\nsimulation methods",       29,
  
  # ── Cluster 3 — IEQ in educational buildings ──────────
  "Indoor Air Quality",          "C3: IEQ & energy efficiency\nin education",   "(iii) Environmental\nperformance indicators", 30,
  "Thermal comfort",             "C3: IEQ & energy efficiency\nin education",   "(iii) Environmental\nperformance indicators", 50,
  "Energy Efficiency",           "C3: IEQ & energy efficiency\nin education",   "(iii) Environmental\nperformance indicators", 16,
  "Buildings",                   "C3: IEQ & energy efficiency\nin education",   "(iv) Building\ntypologies",                 20,
  
  # ── Cluster 4 — Sustainability policy ─────────────────
  "Sustainability",              "C4: Sustainability policy\n& design practice", "(iv) Building\ntypologies",               5,
  "Health",                      "C4: Sustainability policy\n& design practice", "(iii) Environmental\nperformance indicators", 7,
  
  # ── Cluster 5 — Energy demand and control ─────────────
  "Energy consumption",          "C5: Energy demand &\noperational control",    "(iii) Environmental\nperformance indicators", 12,
  "Control strategy",            "C5: Energy demand &\noperational control",    "(i) Optimisation &\ncontrol methods",       8,
  "Model predictive control",    "C5: Energy demand &\noperational control",    "(i) Optimisation &\ncontrol methods",       6,
  "Occupant behavior",           "C5: Energy demand &\noperational control",    "(vi) Design &\noperational variables",      10,
  "Climate change",              "C5: Energy demand &\noperational control",    "(iv) Building\ntypologies",                 8,
  
  # ── Cluster 6 — CO2 monitoring in classrooms ──────────
  # Note: CO2 used instead of CO₂ to avoid rendering issues
  "CO2 concentration",           "C6: CO2 monitoring &\nwindow control",        "(iii) Environmental\nperformance indicators", 15,
  "Window opening",              "C6: CO2 monitoring &\nwindow control",        "(vi) Design &\noperational variables",      12,
  "Classroom",                   "C6: CO2 monitoring &\nwindow control",        "(iv) Building\ntypologies",                 10,
  "Indoor temperature",          "C6: CO2 monitoring &\nwindow control",        "(iii) Environmental\nperformance indicators", 8,
  "Ventilation strategy",        "C6: CO2 monitoring &\nwindow control",        "(ii) Ventilation\nstrategies",              9
)

############################################################
# COLOUR PALETTES
############################################################

cluster_colors <- c(
  "C1: CFD & computational\noptimisation"       = "#1565C0",
  "C2: Passive & hybrid\ncooling strategies"    = "#2E7D32",
  "C3: IEQ & energy efficiency\nin education"   = "#E65100",
  "C4: Sustainability policy\n& design practice" = "#880E4F",
  "C5: Energy demand &\noperational control"    = "#4A148C",
  "C6: CO2 monitoring &\nwindow control"        = "#006064"
)

axis_colors <- c(
  "(i) Optimisation &\ncontrol methods"           = "#1E88E5",
  "(ii) Ventilation\nstrategies"                  = "#43A047",
  "(iii) Environmental\nperformance indicators"   = "#FB8C00",
  "(iv) Building\ntypologies"                     = "#E53935",
  "(v) Modelling &\nsimulation methods"           = "#8E24AA",
  "(vi) Design &\noperational variables"          = "#00ACC1"
)

############################################################
# BUILD ALLUVIAL DATA
# Keywords ordered by frequency (descending) for readability
############################################################

alluvial_data <- data %>%
  mutate(
    keyword = factor(keyword,
                     levels = data %>%
                       arrange(desc(freq)) %>%
                       pull(keyword) %>%
                       unique()),
    cluster = factor(cluster,
                     levels = names(cluster_colors)),
    axis    = factor(axis,
                     levels = names(axis_colors))
  )

############################################################
# PLOT
############################################################

p <- ggplot(alluvial_data,
            aes(axis1 = keyword,
                axis2 = cluster,
                axis3 = axis,
                y     = freq)) +
  
  geom_alluvium(aes(fill = cluster),
                width    = 1/12,
                alpha    = 0.75,
                knot.pos = 0.4) +
  
  geom_stratum(aes(fill = after_stat(stratum)),
               width = 1/4,
               color = "white",
               size  = 0.3) +
  
  geom_label(stat          = "stratum",
             aes(label     = after_stat(stratum)),
             size          = 3.2,        # increased from 2.8
             fontface      = "bold",
             label.padding = unit(0.18, "lines"),
             label.size    = 0,
             fill          = "white",
             alpha         = 0.88) +
  
  scale_fill_manual(values   = c(cluster_colors, axis_colors),
                    na.value = "grey70") +
  
  scale_x_discrete(limits = c("Keywords",
                              "Semantic clusters\n(Reinert DHC)",
                              "Thematic axes"),
                   expand  = c(0.05, 0.05)) +
  
  scale_y_continuous(expand = c(0.02, 0.02)) +
  
  labs(
    title    = "Keyword mapping onto semantic clusters and thematic axes",
    subtitle = "Alluvial diagram — Bibliometric-semantic cross-validation",
    x        = NULL,
    y        = "Keyword frequency",
    caption  = paste0(
      "Sources: Table 2 (keyword frequency); ",
      "Reinert DHC clustering (TALL); ",
      "thematic axes (Section 2.7)\n",
      "Corpus: 84 documents (Bibliometrix) / ",
      "82 documents (TALL)"
    )
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face   = "bold",
                                 size   = 13,
                                 hjust  = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size   = 10,
                                 hjust  = 0.5,
                                 color  = "grey40",
                                 margin = margin(b = 10)),
    plot.caption  = element_text(size  = 8,
                                 color = "grey50",
                                 hjust = 0),
    axis.text.x   = element_text(face = "bold", size = 11),
    axis.text.y   = element_blank(),
    axis.ticks    = element_blank(),
    panel.grid    = element_blank(),
    legend.position = "none",
    plot.margin   = margin(15, 20, 15, 20)
  )

############################################################
# EXPORT
############################################################

# PNG — for article (300 dpi)
ggsave("alluvial_diagram.png", plot = p,
       width = 14, height = 10, dpi = 300,
       bg = "white")
cat("[Saved] alluvial_diagram.png\n")

# PDF — for high-resolution journal submission
ggsave("alluvial_diagram.pdf", plot = p,
       width = 14, height = 10,
       bg = "white")
cat("[Saved] alluvial_diagram.pdf\n")

cat("\nDiagram generated successfully.\n")
cat("Corrections v2: CO2 symbol fix + improved label sizes\n")
cat("Nodes: Keywords (left) -> Reinert clusters (centre) -> Thematic axes (right)\n")
cat("Flow width proportional to keyword frequency.\n")
