## Technical Note — Step 2: Jaro-Winkler Title Harmonization

### Issue identified
The JW = 0.85 threshold applied in Step 2 (title harmonization without deletion) 
generated 8 pairs of documents with unified canonical titles despite having distinct 
DOIs, publication years and source journals. This occurred because the algorithm 
identified near-identical titles across different documents and assigned a single 
canonical form to all members of each similarity cluster.

### Affected field
- **Field affected:** `TI` (title) in the Step 6 TALL depurated corpus
- **Fields not affected:** `DI` (DOI), `AB` (abstract), `DE` (author keywords), 
  `ID` (keywords plus) — all preserved in original form
- **Original titles preserved in:** `TI_raw` field (available in all Step 3–6 outputs)

### Impact assessment
| Analysis layer | Impact | Justification |
|---|---|---|
| Reinert DHC clustering | Marginal | `TI` accounts for ≤15% of the `text` field processed by TALL; abstracts dominate (75-85%) |
| Bibliometrix networks | Negligible | Bibliometrix uses `UT` (unique identifier) as primary node label, not `TI` |
| Zotero export (RIS) | Direct | Duplicate titles caused incorrect record identification in Zotero |
| Data extraction form | Moderate | Duplicate titles complicate manual document identification |

### Corrective action applied
- Zotero RIS file (`S10_corpus_zotero_export.ris`) manually corrected using 
  original titles from `TI_raw`
- Reinert DHC clustering results retained without modification given marginal impact
- No reprocessing of the TALL semantic pipeline required

### Recommendation for future replication
When replicating this pipeline, set the JW harmonization threshold to ≥ 0.95 
for title-based deduplication (with deletion) and disable the JW = 0.85 
harmonization step (without deletion), or apply it only after verifying DOI 
uniqueness within each similarity cluster. The `TI_raw` field should always 
be used as the reference title for document identification in downstream outputs.

### Files affected
- `Step_2_semantic_JW.RData` — harmonized titles in `TI` field
- `Step_6_TALL_depurated.csv` — harmonized titles propagated to `text` field
- `S10_corpus_zotero_export.ris` — manually corrected

### Pairs identified (8 pairs, 16 records)
See `S10_duplicated_titles_log.csv` for the complete list of affected DOI pairs.
