# Water Stress Tolerance in Barley

Statistical analysis and publication-quality figures for an experiment assessing
the effect of water stress on barley genotypes, at two developmental stages.

## Overview

The study evaluates genotypic variability in drought tolerance across two experiments:

- **Experiment 1 — Germination stage.** Five genotypes (Rihane03, Sougueur, Tihert,
  Saida183, Nailia) under PEG-6000-induced water stress at four levels
  (control T, Dh10, Dh15, Dh20). Measured traits: germination precocity, final
  germination rate, fresh weight, dry weight, seedling length.
- **Experiment 2 — Adult stage.** Two genotypes (Nailia, Saida183) under field
  water deficit (control T, DH1 = 30% field capacity, DH2 = 60% field capacity).
  Measured traits: weight-based moisture (HP), water content (WC), relative water
  content (RWC), water deficit (WD), grain number (GN).

## Repository contents

| File | Description |
|------|-------------|
| `code.R` | Main R script: data import, assumption checks, statistical tests, and publication-quality figures (grayscale + hatching, significance letters). |
| `Data.xlsx` | Input data and analysis results (raw data, descriptive statistics, test outputs, post-hoc comparisons). |
| Output figures (PNG 600 dpi + PDF). |

## Statistical approach

The analysis adapts the test to each variable based on whether ANOVA assumptions hold:

1. **Assumption checks** — normality of residuals (Shapiro-Wilk) and homogeneity of
   variances (Levene).
2. **If assumptions are met** — two-way ANOVA (genotype × treatment) followed by
   Tukey's HSD, with homogeneous groups expressed as letters.
3. **If assumptions are not met** — Kruskal-Wallis test followed by Dunn's test with
   Holm correction.

Significance threshold: *p* < 0.05. Results are expressed as mean ± standard error.
On the figures, bars sharing a letter do not differ significantly.

## Data cleaning

- Two water-deficit values of 100% were removed as likely data-entry errors.
- Grain number for Nailia was zero across all treatments, including controls;
  this reflects absence of fruiting under the experimental conditions rather than
  a drought response, and is not interpreted as a tolerance trait.

## How to run

Open `code.R` in R or RStudio, make sure `data.xlsx` is in the
working directory, then run the script. Required packages install automatically on
first run.

```r
pkgs <- c("readxl","car","agricolae","FSA","rcompanion","multcompView",
          "ggplot2","ggpattern","dplyr")

invisible(lapply(pkgs, library, character.only = TRUE))
}
```

Figures are written in PNG (600 dpi) and PDF format.

## Requirements

- R (>= 4.0)
- Packages: readxl, car, agricolae, FSA, rcompanion, multcompView, ggplot2,
  ggpattern, dplyr

## Author

Meriem MEKEDEM (PhD)

