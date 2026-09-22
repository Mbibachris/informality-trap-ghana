#!/usr/bin/env bash
# Reproduces the R estimation stage and the final results notebook from
# already-built processed panels (data/processed_r/) through to
# Rmd/02_results.html and the output/tables_r/ exports, in one command.
#
#   ./run_all.sh            # full R pipeline: data build -> models -> knit
#   ./run_all.sh --from-raw # also (re)builds data/processed_r/ from data/raw/
#   ./run_all.sh --skip-build   # skip step 1, assume data/processed_r/ exists
#
# See README.md for what each step does and why it's ordered this way.
#
# This script is self-sufficient (reads data/raw/ directly) and is the
# one Rmd/02_results.Rmd's numbers come from. It does NOT refit the
# brms/Stan flagship cross-check (R/run_brms_ape_check.R only recomputes
# the APE from an already-saved posterior-draws object, if one exists at
# output/tables_r/brms_crosscheck_2022_vulnerable_it.rds) -- fitting that
# model from scratch is expensive (see README.md), so this script skips
# that step automatically if the file isn't present.

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

# --- locate Rscript -----------------------------------------------------
if [[ -n "${RSCRIPT:-}" ]]; then
  RSCRIPT_BIN="$RSCRIPT"
elif command -v Rscript >/dev/null 2>&1; then
  RSCRIPT_BIN="Rscript"
else
  for candidate in "/c/Program Files/R"/R-*/bin/Rscript.exe; do
    [[ -x "$candidate" ]] && RSCRIPT_BIN="$candidate"
  done
  if [[ -z "${RSCRIPT_BIN:-}" ]]; then
    echo "Rscript not found. Set RSCRIPT=/path/to/Rscript and re-run, or add R's bin/ to PATH." >&2
    exit 1
  fi
fi
echo "Using Rscript: $RSCRIPT_BIN"

SKIP_BUILD=0
FROM_RAW=0
for arg in "$@"; do
  case "$arg" in
    --skip-build) SKIP_BUILD=1 ;;
    --from-raw) FROM_RAW=1 ;;
    *) echo "Unknown argument: $arg" >&2; exit 1 ;;
  esac
done

run_r() {
  echo ""
  echo "=== $1 ==="
  "$RSCRIPT_BIN" "$1"
}

# --- 1. Data build (data/raw/ -> data/processed_r/) ---------------------
if [[ "$SKIP_BUILD" -eq 0 ]]; then
  if [[ "$FROM_RAW" -eq 1 || ! -f data/processed_r/pooled_person_quarter.csv ]]; then
    if [[ ! -d data/raw/2022 ]]; then
      echo "data/raw/ is empty -- raw AHIES microdata is not redistributed." >&2
      echo "See README.md's Data section for how to obtain it, then re-run." >&2
      exit 1
    fi
    echo ""
    echo "=== R/pipeline.R (build data/processed_r/*.csv from data/raw/) ==="
    "$RSCRIPT_BIN" -e 'source("R/pipeline.R"); build_and_save_all_years()'
  else
    echo "data/processed_r/ already present -- skipping build (use --from-raw to force)."
  fi
fi

# --- 2. R estimation stage (dependency-ordered) --------------------------
run_r R/run_step0_descriptives.R
run_r R/run_transitions_attrition.R
run_r R/run_core_models.R
run_r R/run_rq3_crisis.R
run_r R/run_rq2_event_study.R
run_r R/run_rq4_heterogeneity.R
run_r R/run_robustness.R
run_r R/run_stratum_sensitivity.R
run_r R/run_node_stability.R
run_r R/run_attrition_full_coefs.R
run_r R/run_subgroup_export.R
run_r R/run_reconciliation.R
run_r R/run_full_coef_tables.R
run_r R/run_descriptives.R

# brms cross-check: recomputes the APE from the already-saved posterior
# draws (output/tables_r/brms_crosscheck_2022_vulnerable_it.rds); does not
# refit. Skipped automatically if that file isn't present.
if [[ -f output/tables_r/brms_crosscheck_2022_vulnerable_it.rds ]]; then
  run_r R/run_brms_ape_check.R
else
  echo ""
  echo "=== R/run_brms_ape_check.R skipped (no saved brms posterior draws on disk) ==="
fi

run_r R/run_manifest.R

# --- 3. Knit the results notebook ----------------------------------------
echo ""
echo "=== Knitting Rmd/02_results.Rmd ==="
"$RSCRIPT_BIN" -e "rmarkdown::render('Rmd/02_results.Rmd', quiet = TRUE)"

echo ""
echo "Done. Final report: Rmd/02_results.html (local only, not committed)"
echo "Exported tables:     output/tables_r/ (local only; see output/tables_r/MANIFEST.md)"
