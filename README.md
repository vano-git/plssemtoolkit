# plssemtools

A reusable, dataset-agnostic **PLS-SEM analysis pipeline** built on top of [`seminr`](https://github.com/sem-in-r/seminr), producing full diagnostic tables and **publication-quality, journal-style figures** (clean black/white/gray `ggplot2`, exported as PNG + PDF).

This package generalizes a one-off analysis script into something you can point at **any survey dataset and any construct/path specification**, without touching the R code — you only ever edit a small YAML config.

## Why this exists

The original script this package replaces hardcoded construct names, item names, and one specific dataset path directly into the analysis logic, and used `seminr`'s default `plot()` (base R graphics) for figures. That works once, for one paper. `plssemtools` separates **what your model is** (a YAML file) from **how a PLS-SEM analysis is run** (the package), and replaces the default figures with academic-standard `ggplot2` graphics.

## What it produces

Running the pipeline once writes:

**Tables (CSV)** — construct & item descriptives, path coefficients, f², outer loadings, reliability/validity (Cronbach's α, CR, AVE), Fornell-Larcker criterion, HTMT ratio, VIF, cross-loadings, Q² (SSO/SSE), bootstrapped path coefficients and loadings.

**Figures (PNG + PDF, 300 dpi)** — in clean black/white/gray academic style:

| File | Contents |
|---|---|
| `01_path_diagram` | Structural model with path coefficients, R², significance stars |
| `02_outer_loadings` | Item loadings by construct, faceted, with 0.70 threshold line |
| `03_htmt_heatmap` | HTMT discriminant validity matrix |
| `04_r_squared` | R² bar chart for endogenous constructs |
| `05_f_squared` | f² effect size heatmap with magnitude labels |
| `06_bootstrap_paths` | Forest plot of bootstrapped path coefficients with CIs |
| `07_q_squared` | Predictive relevance (Q²) by item |
| `08_construct_descriptives` | Construct means ± SD |

## Installation

### Prerequisites

- **R** version 4.1 or later. Check with `R.version.string` in an R console.
- No RStudio required — everything below works from a plain R console or terminal.

### Step 1: Install required packages

`plssemtools` depends on `seminr` (does the actual PLS-SEM estimation) plus a
handful of tidyverse-style packages for the tables and figures. Install them
once:

```r
install.packages(c(
  "devtools", "seminr", "ggplot2", "dplyr", "tidyr", "stringr",
  "tibble", "scales", "ggrepel", "yaml", "jsonlite", "purrr",
  "rlang", "testthat", "knitr", "rmarkdown", "svglite"
))
```

This can take a few minutes the first time.

### Step 2: Install `plssemtools` itself

Pick **one** of the following, depending on where your copy of the package lives.

**Option A — from GitHub (recommended, once you've pushed it there):**

```r
devtools::install_github("YOUR_USERNAME/plssemtools")
```

**Option B — from a local folder** (e.g. you just unzipped it):

```r
devtools::install("path/to/pls-sem-toolkit")
```

**Option C — load without installing** (fastest for active development —
changes to the R files take effect immediately without reinstalling):

```r
setwd("path/to/pls-sem-toolkit")
devtools::document()   # builds documentation + NAMESPACE from source comments
devtools::load_all()   # loads every function into your session, like library()
```

### Step 3: Verify it worked

```r
library(plssemtools)   # only needed after Option A or B
?run_pls_pipeline       # opens the help page if docs built correctly
```

If that help page opens (or, after `load_all()`, if `run_pls_pipeline` shows
up when you type `run_pls_pipeline` and hit Tab), you're set up correctly.

### Troubleshooting

- **`there is no package called 'seminr'`** — re-run the `install.packages()` line above; one of the dependencies didn't install.
- **`devtools::document()` errors about roxygen2** — run `install.packages("roxygen2")` first.
- **macOS asks for Command Line Tools / Rtools on Windows** — some dependencies compile from source the first time; accept the prompt to install Xcode Command Line Tools (Mac) or install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (Windows), then retry.

## Quick start

**1. Describe your model in a YAML file** (see `inst/extdata/example_model_spec.yml`):

```yaml
constructs:
  Psychological Factors:
    type: reflective
    items: [PS_1, PS_2, PS_3]
  Social Factors:
    type: reflective
    items: [SO_1, SO_2, SO_3, SO_4]
  Environmental Factors:
    type: reflective
    items: [EN_1, EN_2, EN_3]
  Good Shopping Vibes:
    type: reflective
    items: [GV_1, GV_2, GV_3, GV_4, GV_5]

paths:
  - from: [Psychological Factors, Social Factors, Environmental Factors]
    to: Good Shopping Vibes
```

**2. Run the pipeline:**

```r
library(plssemtools)

result <- run_pls_pipeline(
  data       = "your_survey_data.csv",
  spec       = "your_model_spec.yml",
  output_dir = "pls_output",
  nboot      = 5000,           # 5000-10000 recommended for final submission
  seed       = 1234,           # reproducibility
  theme      = plssem_theme(base_family = "serif")
)
```

That's it. All tables land in `pls_output/tables/`, all figures in `pls_output/figures/`.

**3. Reuse on a completely different dataset:** copy the YAML, rename constructs/items to match your new questionnaire's columns, point `data` at the new CSV. No R code changes required.

## Using a different dataset with a different model

This is the core reuse case. Say your new study has constructs `Trust` and `Satisfaction` predicting `Loyalty`:

```yaml
constructs:
  Trust:
    type: reflective
    items: [TR_1, TR_2, TR_3]
  Satisfaction:
    type: reflective
    items: [SAT_1, SAT_2]
  Loyalty:
    type: reflective
    items: [LOY_1, LOY_2, LOY_3]

paths:
  - from: [Trust, Satisfaction]
    to: Loyalty
```

```r
run_pls_pipeline(data = "study2_data.csv", spec = "study2_spec.yml", output_dir = "study2_output")
```

Composite (formative, Mode B) constructs are supported too — set `type: composite` on any construct.

## Customizing figures

Every figure is also available as a standalone function if you want to tweak size, theme, or drop it into a specific manuscript figure slot:

```r
th <- plssem_theme(base_family = "serif", base_size = 12)

p <- plot_path_diagram(result$model_summary, result$spec, boot_summary = result$boot$summary, theme = th)
ggplot2::ggsave("figure1.pdf", p, width = 8, height = 5, device = grDevices::cairo_pdf)
```

`plssem_theme()` gives you a strictly grayscale, minimal-gridline theme by default (journal-safe for print). Pass `base_family = "serif"` for a Times-like look, or leave default for your device's sans font.

## Package structure

```
R/
  model_spec.R       Load & validate YAML/JSON/list model specifications
  data_io.R           Load & validate survey CSVs against a model spec
  descriptives.R       Construct- and item-level descriptive statistics
  pipeline.R            estimate_model(), run_bootstrap(), compute_q_squared(),
                          run_pls_pipeline() (the one-call entry point)
  theme.R                plssem_theme(), save_figure()
  plot_path_diagram.R     Structural model diagram
  plot_loadings.R          Outer loadings plot
  plot_validity.R           HTMT heatmap, R², f² plots
  plot_bootstrap.R           Bootstrap forest plot, Q² plot
  plot_descriptives.R         Construct descriptives plot, generate_all_figures()
inst/
  extdata/example_model_spec.yml   Example config (this study's model)
  examples/run_analysis.R           Example end-to-end usage script
tests/testthat/                       Unit + integration tests on synthetic data
```

## Development

```r
devtools::load_all()
devtools::test()
devtools::document()   # regenerate NAMESPACE/man/ after changing roxygen comments
devtools::check()
```

## Publishing this package to GitHub

If you don't already have a GitHub account, create one at
[github.com](https://github.com) first. Then, from a terminal (not the R
console) inside the `pls-sem-toolkit` folder:

### Step 1: Create the repository on GitHub

Go to [github.com/new](https://github.com/new), name it `plssemtools` (or
whatever you like), leave it **empty** (don't initialize with a README —
you already have one), and click **Create repository**. Copy the URL it
gives you, e.g. `https://github.com/YOUR_USERNAME/plssemtools.git`.

### Step 2: Push your local folder to it

```bash
cd path/to/pls-sem-toolkit
git init
git add .
git commit -m "Initial commit: plssemtools package"
git branch -M main
git remote add origin https://github.com/YOUR_USERNAME/plssemtools.git
git push -u origin main
```

If this is your first time using git from this machine, it may ask you to
configure your identity first:

```bash
git config --global user.name "Your Name"
git config --global user.email "you@example.com"
```

And it will prompt you to authenticate — GitHub no longer accepts a plain
password over HTTPS, so either:
- Use a **Personal Access Token** in place of your password ([create one here](https://github.com/settings/tokens), scope `repo`), or
- Set up SSH keys and use `git@github.com:YOUR_USERNAME/plssemtools.git` as the remote URL instead.

### Step 3: Update the placeholder username

Search-and-replace `YOUR_USERNAME` with your actual GitHub username in:
- `README.md` (badge URL and install command)
- `DESCRIPTION` (`URL:` and `BugReports:` fields)

Commit and push that change:

```bash
git add README.md DESCRIPTION
git commit -m "Update GitHub username placeholders"
git push
```

### Step 4: Confirm it installs from GitHub

From a fresh R session, anyone (including you, on another machine) can now run:

```r
devtools::install_github("YOUR_USERNAME/plssemtools")
```

### Step 5 (optional but recommended): Let CI check it automatically

This repo already includes `.github/workflows/R-CMD-check.yaml`. The moment
you push to GitHub, it will automatically run `R CMD check` on every push and
pull request, and show a pass/fail badge (already wired up at the top of this
README once you swap in your username). Check the **Actions** tab on your
repo page to see it run.

## Citing seminr

This package is a workflow wrapper around `seminr`. If you publish results produced with it, cite:

> Ringle, C. M., Wende, S., & Becker, J.-M. (2024). *SmartPLS 4*. Ringle, Wende & Becker.
> Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2022). *A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM)* (3rd ed.). SAGE.
> Ray, S., Danks, N. P., & Calero Valdez, A. (2024). *seminr: Building and Estimating Structural Equation Models*. R package.

## License

MIT — see `LICENSE.md`.
