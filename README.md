# Optimization_review

Reproducibility repository for the systematic review:

> Sánchez Cordero, A., Gómez Melgar, S., & Andújar Márquez, J. M. *From design optimization to adaptive control: a systematic review of natural and mixed-mode ventilation for indoor air quality, thermal comfort and energy efficiency*. Manuscript in preparation.

The repository contains the R pipeline, auxiliary lists and coded data used to (i) identify, screen and select the corpus (hybrid PRISMA 2020 / SPAR-4-SLR protocol), (ii) prepare the corpus for semantic clustering in TALL (Reinert DHC, k = 6), (iii) derive the taxonomy reported in Table 3 and (iv) compute the descriptive results.

- Bibliometric corpus: **84** documents
- Semantic analysis and full-text extraction: **82** documents (the 84/82 asymmetry is declared in Section 2.5 of the manuscript)

## Repository structure

All scripts read and write files in the repository root through relative paths. The flat layout is kept deliberately so that the archived scripts are identical to those that produced the results reported in the manuscript. Set the working directory to the repository root before running any script.

| Group | Files |
|---|---|
| Pipeline scripts | `LR_S1_Identification.R` … `LR_S11_Descriptive results.R` |
| Auxiliary lists (TALL) | `260511_Multiwords by a list.xlsx`, `260513_Stop list complete.xlsx` |
| Screening log (S3) | `Step_3_final_decisions_log.csv` |
| Alluvial diagram (S7) | `alluvial_diagram.png`, `alluvial_diagram.pdf` |
| TALL exports (S8 inputs) | `S9_Segments_by_Cluster_TALL.xlsx`, `S9_Terms_by_Cluster_TALL.xlsx` |
| Taxonomy outputs (S8) | `S9_taxonomy_*.csv`, `S9_taxonomy_hybrid_summary.xlsx` |
| Full-text access (S10) | `S10_corpus_doi_list.txt`, `S10_corpus_access_form.xlsx` |
| Manual extraction form | `S11_Paper analysis data extraction_82.xlsx` |
| Descriptive results (S11 script) | `S12_descriptive_results.xlsx`, `S12_fig1–4_*.png` |
| Discarded branch | `discarded/Step_7_kmeans_*.csv` |

## Execution order

| Step | Script | Input → Output |
|---|---|---|
| 1 | `LR_S1_Identification.R` | WoS + Scopus exports → `Step_1_struct.RData` |
| 2 | `LR_S2_Inclusion.R` | → `Step_2_semantic_JW.RData` |
| 3 | `LR_S3_Elegibility.R` | → `Step_3_final_corpus*`, `Step_3_final_decisions_log.csv` |
| 4 | `LR_S4_Final Corpus.R` | → `Step_4_final_corpus_84_fullcols*` (84 records) |
| 5 | `LR_S5_Final_Corpus_TALL_Clean.R` | → `Step_5_TALL_clean.csv` |
| 6 | `LR_S6_Depurated.R` | → `Step_6_TALL_depurated.csv` (82 records) |
| 7 | `LR_S7_Alluvial diagram.R` | → `alluvial_diagram.png/.pdf` |
| — | *TALL (external)* | Reinert DHC, k = 6 → `S9_Segments_by_Cluster_TALL.xlsx`, `S9_Terms_by_Cluster_TALL.xlsx` |
| 8 | `LR_S8_Taxonomy.R` | → `S9_taxonomy_*` (Table 3), with regression check (block S8.14) |
| 9 | `LR_S10_Zotero export for manual review.R` | → `S10_corpus_*` |
| — | *Manual full-text extraction* | → `S11_Paper analysis data extraction_82.xlsx` |
| 10 | `LR_S11_Descriptive results.R` | → `S12_descriptive_results.xlsx`, `S12_fig1–4_*.png` |

### Numbering notes

- **There is no `LR_S9` script.** Numbering shifted when the TF-IDF + k-means branch (former Step 7) was discarded.
- Script and data numbering differ by design: `LR_S8_Taxonomy.R` reads and writes files prefixed `S9_*`; `LR_S11_Descriptive results.R` reads `S11_*` and writes `S12_*`.
- Titles in `S11_*` are canonical over those in the S10 export (the latter contain Zotero metadata-collision and truncation artefacts).

## What can be reproduced from this repository

| Result | Reproducible from archived files | How |
|---|---|---|
| Table 3 (taxonomy) | Yes | Run `LR_S8_Taxonomy.R`; block S8.14 checks 20 anchors of Table 3 as reported in the manuscript |
| Descriptive results and Figures (S12) | Yes | Run `LR_S11_Descriptive results.R` |
| Steps 1–7 (search, screening, cleaning) | Requires re-export from WoS and Scopus | See *Third-party content* |

## Third-party content

The following files are **not** distributed, because they contain bibliographic records and abstracts licensed by Clarivate (Web of Science), Elsevier (Scopus) and the respective publishers:

- raw database exports (`WOS_400.txt`, `SCO_181.csv`);
- intermediate files of Steps 1–6 (`Step_*.RData`, `Step_*_corpus*.csv`, `Step_5_*`, `Step_6_*`, `Step 5_TALL file.tall`);
- RIS exports (`Step_3_to_review.ris`, `S10_corpus_zotero_export.ris`);
- full texts (PDF) of the corpus documents.

To regenerate Steps 1–6, re-run the search below in Web of Science Core Collection and Scopus, save the exports as `WOS_400.txt` (plain text, full record) and `SCO_181.csv` (CSV, all fields) in the repository root, and run the scripts in order. Record counts may differ from those reported if the databases have been updated since the extraction date.

- Search date: November 2025
- The same query was run in Scopus and in Web of Science Core Collection, with equivalent field tags (Scopus `TITLE-ABS-KEY`; WoS `TS=` Topic).
- Scopus (`SCO_181.csv`, 181 records):

  ```
  ( TITLE-ABS-KEY ( ventilat* ) AND TITLE-ABS-KEY ( optimi* ) AND TITLE-ABS-KEY ( indoor air quality )
  AND TITLE-ABS-KEY ( thermal comfort ) AND TITLE-ABS-KEY ( energ* ) ) AND PUBYEAR > 2014 AND PUBYEAR < 2027
  AND ( LIMIT-TO ( LANGUAGE , "english" ) ) AND ( LIMIT-TO ( DOCTYPE , "ar" ) )
  ```

- Web of Science Core Collection (`WOS_400.txt`, 400 records):

  ```
  TS=(ventilat*) AND TS=(optimi*) AND TS=("indoor air quality") AND TS=("thermal comfort") AND TS=(energ*)
  Refined by: Publication Years 2015–2026; Languages: English; Document Types: Article
  ```

`S10_corpus_doi_list.txt` and `Step_3_final_decisions_log.csv` identify every record and every screening decision without reproducing abstracts.

## Discarded branch

`discarded/Step_7_kmeans_*.csv`: TF-IDF + k-means clustering tested as an alternative to Reinert DHC. It was discarded because it yielded only two interpretable clusters, whereas the Reinert k = 6 solution was stable and mapped onto RQ1–RQ4. No value reported in the manuscript derives from these files.

## Requirements

- R ≥ 4.3
- Packages: `bibliometrix`, `stringdist`, `readr`, `readxl`, `dplyr`, `stringr`, `tidyr`, `writexl`, `ggplot2`, `ggalluvial`, `forcats`
- TALL (Text Analysis for aLL) for the semantic clustering step

```r
install.packages(c("bibliometrix", "stringdist", "readr", "readxl", "dplyr",
                   "stringr", "tidyr", "writexl", "ggplot2", "ggalluvial", "forcats"))
```

## Licence

- Code (`*.R`): MIT — see `LICENSE`
- Data and outputs: CC BY 4.0 — see `LICENSE-DATA`

## Citation

Please cite the archived version via its Zenodo DOI (see `CITATION.cff`):
`https://doi.org/10.5281/zenodo.XXXXXXX`

## Contact

Antonio Sánchez Cordero — TEP192, Universidad de Huelva — a.cordero@zerocem.es — ORCID 0000-0002-2637-4364
