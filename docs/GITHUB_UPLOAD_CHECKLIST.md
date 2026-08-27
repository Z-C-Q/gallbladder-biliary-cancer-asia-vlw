# GitHub publication checklist

## Before upload

- Replace the placeholder GitHub URL in `CITATION.cff`.
- Replace placeholders in `docs/RESOURCE_AVAILABILITY_TEXT.md` and `docs/CELL_PRESS_RESPONSE.md` only after the links are live.
- Confirm that the corresponding author approves the MIT license for code and the public release of the analytical input datasets.
- Confirm that the included GBD-derived processed files are consistent with the IHME agreement accepted by the data user.
- Do not add contracts, draft manuscripts, response letters, credentials, or the source downloads listed in `data/raw/README.md`.

## Recommended Git commands

```bash
git init
git add .
git status
git commit -m "Release analysis code and processed data"
git branch -M main
git remote add origin <GITHUB_REPOSITORY_URL>
git push -u origin main
```

All files in the prepared release are below 25 MB, so Git LFS is not required. Use Git on the command line or GitHub Desktop for the initial upload.

## After upload

- Open the repository in a private/incognito browser window and verify that it is public.
- Confirm that `README.md` renders correctly and that the large compressed CSV can be downloaded.
- Create a numbered GitHub release, for example `v1.0.0`.
- Confirm that the Zenodo record resolves at https://doi.org/10.5281/zenodo.22133469.
- Confirm that the GitHub URL and archive DOI appear in the manuscript Resource availability section and Key resources table.
- Re-run `Rscript R/run_analysis.R` from a fresh clone before final resubmission.
