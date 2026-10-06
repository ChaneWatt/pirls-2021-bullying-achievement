# PIRLS 2021 bullying and reading achievement

Analysis of the association between student bullying and reading achievement in
South Africa and Brazil using the PIRLS 2021 international database.

## Project structure

- `data/raw/`: unmodified PIRLS 2021 R data files for Brazil (`BRA`) and
  South Africa (`ZAF`).
- `scripts/00_packages.R`: checks the small set of R packages used by the
  project and can install any that are missing.
- `scripts/01_data_audit.R`: checks the two main student files, their identifiers,
  survey-design fields, bullying measures, and reading plausible values.
- `output/`: generated tables and figures (do not edit these by hand).

The main analysis file has not yet been added to the repository. Add it under
`scripts/` before substantive code cleaning or model estimation.

## R setup

The project uses R 4.4 or later and this deliberately small package set:

| Package | Use |
|---|---|
| `here` | project-relative file paths |
| `haven` | PIRLS/SPSS labels and user-defined missing values |
| `dplyr` | data preparation |
| `survey` | weighted estimation with PIRLS jackknife replication |
| `broom` | tidy model output |
| `ggplot2` | figures |

Check the packages from the project root:

```r
source("scripts/00_packages.R")
```

To install missing packages on a new computer:

```powershell
Rscript scripts/00_packages.R --install
```

Then audit the input files:

```powershell
Rscript scripts/01_data_audit.R
```

## Data notes

`ASGBRAR5.Rdata` and `ASGZAFR5.Rdata` are the compact student background files
and already contain the fields needed for the core analysis:

- identifiers: `IDSTUD`, `IDSCHOOL`, `IDCLASS`;
- full-sample weight: `TOTWGT`;
- jackknife fields: `JKZONE`, `JKREP`;
- reading plausible values: `ASRREA01` through `ASRREA05`;
- bullying scale and index: `ASBGSB` and `ASDGSB`;
- bullying questionnaire items: `ASBG11A` through `ASBG11J`.

The current files contain 4,941 students in 187 schools for Brazil and 12,422
students in 321 schools for South Africa. These counts differ from the classroom
Stata example (12,810 students and 293 schools), so that do-file should be used
as a conceptual reference rather than as code or a count benchmark.

## Methodological guardrails

- Keep identifiers as identifiers; do not interpret their means.
- Convert SPSS user-defined missing values to `NA` before analysis (for example,
  with `haven::zap_missing()`).
- Use `TOTWGT` for population-representative estimates.
- Use the PIRLS jackknife procedure based on `JKZONE` and `JKREP`; ordinary
  standard errors or simple school clustering are not substitutes.
- Use all five reading plausible values and combine both sampling and imputation
  uncertainty. Do not use only `ASRREA01` for final inference.
- Analyse countries separately or ensure that jackknife zones are uniquely
  identified by country before fitting a pooled country-interaction model.

Before finalising the estimation code, check it against the
[PIRLS 2021 International Database and User Guide](https://www.iea.nl/publications/user-guides/pirls-2021-international-database-and-user-guide).
The full international database does not need to be downloaded again for the
current Brazil-South Africa analysis because the relevant country files are
already present.
