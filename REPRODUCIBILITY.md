# Reproducibility

## Tested workflow

From the repository root:

```bash
Rscript R/check_dependencies.R
Rscript R/run_analysis.R
```

The release workflow was tested on macOS with R 4.5.0. The run completed without errors and regenerated all main figures, figure-level datasets, manuscript tables, and supplementary analyses. These generated files are written to the ignored `results/` directory and are not part of the public release.

## Script order

`R/run_analysis.R` executes the scripts in the following order:

1. temporal trends;
2. country maps;
3. rank changes;
4. age- and sex-specific patterns;
5. SDI associations;
6. burden growth;
7. priority matrix;
8. manuscript tables;
9. supplementary sensitivity and robustness analyses; and
10. study flowchart.

The figure scripts use only the processed inputs included in the repository. The raw-data preparation scripts are separate because access to the source GBD and HALE downloads is governed by IHME terms.

## Verification performed for this release

- The complete analysis runner exited successfully.
- Regenerated figure-level CSV files were compared with the pre-release analysis files; all values and fields were equal within a numerical tolerance of `1e-12`.
- All analytical worksheets regenerated from the public minimum inputs matched a fresh raw-input run. The public country crosswalk adds the explicit `Türkiye`/`TUR` record missing from the source-name crosswalk; this changes only the completeness of the mapping worksheet, not an analytical result.
- The repository contains no file larger than 25 MB.
- All public scripts use relative paths and contain no workstation-specific path.
- Generated result files, contract files, draft manuscripts, reviewer-response documents, temporary files, and duplicate folders are excluded.

## Numerical considerations

CSV serialization may change insignificant decimal formatting while preserving numeric values. PDF and XLSX file checksums can change after regeneration because those formats can contain creation metadata. Reproducibility should therefore be assessed from the numerical CSV outputs and workbook cell contents, not binary identity alone.
