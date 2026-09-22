# Are informality traps real?

State dependence in informal and vulnerable employment across Ghana's
cost-of-living crisis, 2022-2024. AHIES panel data, dynamic
random-effects probit with the Wooldridge (2005) initial-conditions
device.

This repository contains **code and dataset-handling logic only**, kept
in one folder ([`reproducibility/`](reproducibility)) so it's clear
exactly what you need to reproduce the estimation stage. It does not
ship a manuscript, raw microdata, or any generated result -- everything
below regenerates from the code against your own copy of the data.

## Layout

```
reproducibility/
  R/            The R pipeline and estimator (see "The estimator" below).
  Rmd/          R Markdown source: 01 (data management), 02 (results).
                Knit these yourself -- rendered output isn't committed.
  config/
    variable_map.csv   AHIES-code -> role mapping the loaders read from.
  run_all.sh    One-command driver: R estimation stage -> knitted HTML.
  renv.lock     R package versions.
  .here         Marks this folder as the project root for the `here`
                package, so every script/notebook resolves paths
                correctly regardless of where you invoke R from.
```

Everything the code produces or expects as input -- `data/`, `output/`
-- lives under `reproducibility/` too, but isn't committed (see
`.gitignore`): raw data isn't redistributable, and every table/figure
regenerates in minutes by running the pipeline.

## The estimator

The core specification is a dynamic random-intercept probit with the
Wooldridge (2005) initial-conditions device, fit by a custom
Gauss-Hermite-quadrature maximum-likelihood engine
(`reproducibility/R/re_probit.R`): analytic score, `ucminf` optimizer,
cluster-robust (EA) sandwich standard errors built from the same
per-person score contributions. `lme4::glmer` was tried first and did
not return after 47+ minutes of CPU time on this specification
(apparently a pathological interaction with the data's many
region/sector dummy columns); the custom engine fits the same model in
seconds and is what every `run_*.R` script uses. Average partial effects
use the closed-form population-averaged probability implied by a normal
random intercept -- Φ(x'β/√(1+σ_α²)) -- evaluated as a **discrete**
difference (P(1)-P(0)), not the continuous partial derivative
`marginaleffects`/`statsmodels` compute by default for a variable not
flagged as a dummy (that distinction changes the reported effect by
roughly 50% on this data, so it matters).

**The estimator is deterministic.** Every fit starts from a fixed,
data-derived starting value (not a random draw), so every table
regenerates bit-for-bit from the same input data. The one genuinely
stochastic piece is an optional Bayesian (brms/Stan) cross-check
(`R/run_brms_ape_check.R`) of a single flagship model -- it only
recomputes an APE from previously-saved posterior draws and does not
refit; a full multi-chain fit on this specification is expensive (a
single bounded, single-chain run took most of a session), so don't
launch one casually. If you ever do refit it, pass an explicit
`seed = 20220222` to `brms::brm()` for reproducibility.

## Data

**AHIES 2022-2024** (Annual Household Income and Expenditure Survey),
Ghana Statistical Service (GSS) -- three independent within-year panels
(each year draws a fresh sample; no individual is linked across years).
Public, anonymised microdata, free registration required via the GSS
microdata catalogue. **Not redistributed here** -- register with GSS,
download the three year extracts (2022/2023 arrive as CSV-in-ZIP, 2024
as Stata `.dta`), and place them under
`reproducibility/data/raw/{2022,2023,2024}/` exactly as
`config/variable_map.csv` and `R/io_loaders.R` expect.

The within-2022 inflation analysis also needs GSS's monthly CPI release,
placed at `reproducibility/data/raw/cpi/ghana_cpi_quarterly.csv` (see
`R/cpi_build.R` for the exact construction -- national, headline,
year-on-year, averaged to quarters).

## Run order

**0. Prerequisites**

- R ≥ 4.5 with the packages in
  [`reproducibility/renv.lock`](reproducibility/renv.lock) installed
  (`here`, `dplyr`, `tidyr`, `readr`, `stringr`, `haven`, `jsonlite`,
  `scales`, `ggplot2`, `flextable`, `DT`, `MASS`, `pracma`, `ucminf`,
  `sandwich`, `marginaleffects`, `brms`, `rstan`, `rmarkdown`, `knitr`).
  With [`renv`](https://rstudio.github.io/renv/): `renv::init()` then
  `renv::restore()`. Otherwise, `install.packages()` the listed
  versions.
- Pandoc (bundled with RStudio, or install separately) for
  `rmarkdown::render()`.
- Your own copy of the raw AHIES + CPI data, placed as described above.

**1. `cd reproducibility` -- every command below assumes this directory
as your working directory.**

**2. Build the analysis-ready panels from raw data**

```bash
Rscript -e 'source("R/pipeline.R"); build_and_save_all_years()'
```

Reads `data/raw/2022/`, `data/raw/2023/`, `data/raw/2024/` via
`R/io_loaders.R` and `config/variable_map.csv`; builds the dependent
variables, dynamics, recodes, and feasibility-gate evidence; writes
`data/processed_r/{2022,2023,2024}_person_quarter_{unbalanced,balanced}.csv`
and `data/processed_r/pooled_person_quarter.csv`.

**3. Run the R estimation stage, in this order** (each step's output
feeds a later step -- `output/tables_r/MANIFEST.md`, generated in the
last step, maps every file to the results-notebook section that reads
it):

```bash
Rscript R/run_step0_descriptives.R       # DV base rates, lead-DV decision
Rscript R/run_transitions_attrition.R    # transition matrices, gross flows
Rscript R/run_core_models.R              # the 6 core RE-probit models
Rscript R/run_rq3_crisis.R               # pooled year x lag crisis comparison
Rscript R/run_rq2_event_study.R          # within-2022 inflation/entry
Rscript R/run_rq4_heterogeneity.R        # heterogeneity models
Rscript R/run_robustness.R               # attrition, Mundlak/CRE, SSNIT
Rscript R/run_stratum_sensitivity.R      # region x urbrur robustness cut
Rscript R/run_node_stability.R           # quadrature node-count sweep
Rscript R/run_attrition_full_coefs.R     # full attrition-probit coef table
Rscript R/run_subgroup_export.R          # tidy heterogeneity exports
Rscript R/run_reconciliation.R           # sigma_alpha-vs-beta decomposition + Mundlak scaling
Rscript R/run_full_coef_tables.R         # full coefficient tables + fit stats
Rscript R/run_descriptives.R             # covariate descriptives by year x status
Rscript R/run_brms_ape_check.R           # only if brms_crosscheck_*.rds exists
Rscript R/run_manifest.R                 # regenerate output/tables_r/MANIFEST.md
```

**4. Knit the results notebook**

```bash
Rscript -e "rmarkdown::render('Rmd/02_results.Rmd', quiet = TRUE)"
```

Produces a single self-contained `Rmd/02_results.html`.

**Or, do steps 3-4 in one command:**

```bash
./run_all.sh              # assumes data/processed_r/ already exists
./run_all.sh --from-raw   # also runs step 2 first
```

## Expected runtime

- Build panels from raw: a few minutes.
- R estimation stage: roughly 15-30 minutes total (single-core R; each
  quadrature-MLE fit takes ~20-40 seconds, and the stage above fits on
  the order of 35-40 models).
- Knit: well under a minute (reads already-computed CSV/JSON only).

## Every R file, one line each

| File | What it does |
|---|---|
| `io_loaders.R` | Raw AHIES file -> data frame (CSV-in-ZIP vs. `.dta`, column selection via `config/variable_map.csv`). |
| `panel_keys.R` | Within-year panel key + balance validation. |
| `roster.R` | Household size / composition aggregates. |
| `construct.R` | `informal_it` / `vulnerable_it` / `labour_status4` / recodes. |
| `dynamics.R` | Lags, Wooldridge initial conditions, within-person means, attrition flags. |
| `gates.R` | Feasibility-gate evidence (called from `pipeline.R`). |
| `transitions.R` | 4-state transition matrices / gross flows. |
| `attrition.R` | Non-random-attrition (Welch t-test) diagnostic. |
| `pipeline.R` | Orchestrates the above -> `data/processed_r/*`. |
| `cpi_build.R` | Quarterly CPI/inflation series from the raw GSS release. |
| `re_probit.R` | **The estimator.** Gauss-Hermite-quadrature dynamic RE-probit MLE, analytic score, EA-clustered sandwich SEs, closed-form APE. |
| `design.R` | Shared estimation-universe filter and design-matrix builder, used by nearly every `run_*.R` script. |
| `run_step0_descriptives.R` | DV base rates, raw state-dependence signal -> lead-DV decision. |
| `run_transitions_attrition.R` | Transition matrices, gross flows, attrition diagnostics per year. |
| `run_core_models.R` | The 6 core dynamic RE-probit models (year x DV) + naive-probit comparator + instability flags. |
| `run_rq3_crisis.R` | Pooled year x lag interaction model; tests state dependence equal across years. |
| `run_rq2_event_study.R` | Within-2022 entry-vs-inflation probits + early-entrant persistence. |
| `run_rq4_heterogeneity.R` | Heterogeneity models (sex/age/education/locality x year). |
| `run_robustness.R` | Attrition (balanced vs. unbalanced), non-random-attrition test, Mundlak/CRE cross-check, SSNIT-route redefinition. |
| `run_stratum_sensitivity.R` | Adds the `region x urbrur` interaction as a robustness cut. |
| `run_node_stability.R` | Gauss-Hermite node-count (K=12/24/48/96) sensitivity sweep. |
| `run_attrition_full_coefs.R` | Regenerates the full attrition-probit coefficient table without re-running the whole robustness battery. |
| `run_subgroup_export.R` | Tidies the heterogeneity models into flat CSV/JSON + formal interaction Wald tests. |
| `run_reconciliation.R` | Sigma_alpha-vs-beta decomposition of the per-year-vs-pooled gap; Mundlak/RE-probit scaling check. |
| `run_full_coef_tables.R` | Assembles fit statistics and full long/wide coefficient tables for all 6 core models. |
| `run_descriptives.R` | Covariate means/SDs/shares by year x `vulnerable_it` status. |
| `run_brms_ape_check.R` | Recomputes the Bayesian (brms) cross-check APE from saved posterior draws (no refitting). |
| `run_manifest.R` | Regenerates `output/tables_r/MANIFEST.md` from the actual directory listing. |

`Rmd/01_data_management.Rmd` knits the data-management steps above into
a report; `Rmd/02_results.Rmd` is the full results notebook (every
table, figure, and diagnostic the R estimation stage produces).

## Reproducibility conventions

- No estimator, SE, or APE-definition change is ever made silently. If
  you touch `R/re_probit.R` or `R/design.R`, re-run the full sequence in
  step 3 above rather than selectively rebuilding individual tables.
- `Rmd/02_results.Rmd`'s feasibility-gate section (§0.3) expects an
  optional `docs/gates_report.md` (i.e.
  `reproducibility/docs/gates_report.md`) record of Gates 1-5 evidence,
  in the format described in that section's text; if you don't produce
  one, that subsection renders a short note instead of erroring, and
  every other section is unaffected.

## Known scope decisions (none change any reported estimate)

- No explicit design-stratum variable exists in the raw AHIES extract;
  `region x urbrur` is used as a practical proxy (robustness-checked in
  `run_stratum_sensitivity.R`).
- The SSNIT-specific informality route (`ssnit_flag`) is populated for
  only ~8-18% of the employee subsample -- kept as a robustness-only cut
  (`run_robustness.R`), not the headline informality rule.
- Secondary-job informality is not constructed; the dependent variables
  are defined on the main job only, standard practice in this
  literature.
