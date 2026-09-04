# ARCHITECTURE

## Assumption Checking and Reporting for ANOVA Designs

---

## 1. Purpose and Scope

This subsystem provides a **single, design-aware workflow for checking
assumptions, fitting, and reporting ANOVA models**, covering:

- One-way between-subjects designs
- One-way within-subjects (repeated measures) designs
- Factorial between-subjects designs
- Mixed (split-plot) designs

The goal is that a complete workup, from assumptions through to an APA
sentence, takes a few lines rather than a page, and that **every statistical
recommendation the system makes is traceable to a citation**.

Primary reference throughout:

> Maxwell, S. E., Delaney, H. D., & Kelly, K. (2017). *Designing experiments
> and analyzing data: A model comparison perspective* (3rd ed.). New York:
> Taylor & Francis.

Referred to as **MDK** below.

---

## 2. Core Philosophy

### 2.1 Findings are separate from advice

The central invariant:

> **A test produces a finding. A finding does not know what to do about
> itself.**

- **Tests** compute a statistic and return a `finding`: what was tested, the
  result, a severity, and a reference.
- **Engines** know which findings a given design requires.
- **The recommender** maps a set of findings to a recommended next step.

Mixing these produces code where changing "what to do when variances are
unequal" means editing Levene's test. Keeping them apart means the
recommendation rules live in one auditable table, and the same findings can
drive different advice in different contexts.

### 2.2 Citations are data, not prose

Every reference lives in one registry (`anova_refs.R`), keyed by concept.
Tests, recommendations, print methods, and any teaching material all pull from
that registry. A citation is written once and is correct everywhere, or wrong
once and fixable in one place.

Each entry carries a `verified` flag. **An unverified equation number is worse
than no equation number**, so entries default to `NA` and
`anova_refs_todo()` lists what still needs filling in from the book.

### 2.3 The design determines the assumptions

Assumptions are properties of the design, not of the function you happened to
call. Between-subjects designs need homogeneity of variance. Within-subjects
designs need sphericity. Mixed designs need both, plus homogeneity of
covariance matrices. The user should state the design, or let it be detected,
and get the right checks automatically.

### 2.4 Base R inside, no new dependencies

The package already imports a large stack. This subsystem adds **nothing** to
it. `tapply()`, `aggregate()`, `data.frame()`, `aov()`, `lm()`, `anova()`.
ggplot2 for plots only, loaded via `requireNamespace()`.

Levene's test is implemented in base R rather than importing `car`, and is
verified against `car::leveneTest()` for both `mean` and `median` centers.

---

## 3. Six-Layer Architecture

### Layer 1 - Orchestration (what you call)

- `anova_check()` - assumptions for any supported design
- `anova_workup()` - assumptions, test, means, figure, APA sentence

These accept either a fitted model or `formula + data`. Within-subjects
designs are awkward to express as a fitted `aov()`, so the formula interface
is not optional.

### Layer 2 - Design engines (internal)

One per design, each knowing which findings apply:

| Engine | Assumptions checked |
|---|---|
| `.engine_oneway_bs()` | normality of residuals, homogeneity of variance, independence (flagged, not tested) |
| `.engine_oneway_ws()` | normality of residuals, sphericity |
| | *Input is LONG format: `dv ~ within`, plus `id`. Reshapes to wide internally for `mauchly.test()` and `anova(fit, X = ~1, test = "Spherical")`, both base R, which also return the Greenhouse-Geisser and Huynh-Feldt epsilons. No new dependency, and the caller never reshapes anything. Incomplete cases are dropped, with a count reported.* |
| `.engine_factorial_bs()` | normality of residuals, homogeneity across all cells |
| `.engine_mixed()` | normality, sphericity, homogeneity of covariance matrices |

Engines never print and never recommend. They return findings.

### Layer 3 - Tests

Each returns a `finding`. Standard shape, so the printer and the recommender
never special-case one.

```
finding(
  concept   = "homogeneity",     # key into the reference registry
  label     = "Homogeneity of variance",
  statistic = 0.14, df = c(2, 42), p = 0.865,
  severity  = "ok" | "watch" | "violated" | "untestable",
  detail    = "largest / smallest group variance = 1.36"
)
```

`severity` is what the recommender consumes. `untestable` exists for
independence, which is a design question rather than a test.

### Layer 4 - Knowledge base

- **Reference registry** (`anova_refs.R`): concept, MDK chapter, MDK equation,
  the equation itself, a plain-English gloss, supplementary citations, and a
  `verified` flag.
- **Recommendation rules**: a table mapping a pattern of findings to a
  recommended action plus the concept key that justifies it. Data, not `if`
  statements scattered through the engines.

### Layer 5 - Presentation

- `print.anova_check()` - verdict block, `verbosity = "short" | "long"`
  following the `print.mlm_report()` idiom already in the package
- `anova_check_plots()` - diagnostics, opt-in
- `anova_plot()` - the reporting figure
- `apa_anova()` - the sentence

### Layer 6 - Multiverse (`anova_multiverse.R`)

`anova_multi()` crosses the small analysis decisions you could defensibly have
made, refits under each, and reports whether the conclusion survives.

This exists because Field and Wilcox (2017) argue sensitivity analysis should
be required rather than optional: fit the conventional model and a robust
counterpart, report either where they agree, report the robust one where they
do not. Their stronger claim is that this comparison is the only known way to
judge whether a conventional method gave a reasonable answer.

**Default grid** (2 x 2 x 3, with redundant cells collapsed):

| Decision | Levels |
|---|---|
| `transform` | none, log, sqrt |
| `variance` | pooled, welch |
| `estimator` | ols, trim20 |

A trimmed-means test is already heteroscedastic-robust, so `trim20` cells are
collapsed to a single `variance = "robust"` row rather than duplicated across
pooled and Welch. Outlier trimming is available but deliberately **not** a
default, because Field and Wilcox are critical of SD-based trims followed by
OLS; when requested it is labelled in the output as the flawed-but-common
approach.

**What the forest plot shows.** The omnibus F has no single signed effect, so
the plot is anchored on a focal contrast between two groups, **standardized**
so that transformed and untransformed specifications share an axis. The
omnibus test lives in the results table. Robust rows use the trimmed mean
difference over the pooled winsorized SD, rescaled by 0.642 so it is on the
same footing as Cohen's d under normality (Algina, Keselman & Penfield, 2005).

**Precision cost.** Each row reports its CI width relative to the baseline
specification, which is the concrete price of moving to a robust method. No
simulation required.

**Effect sizes are not interchangeable across estimators.** OLS rows report
omega squared; trimmed rows report xi, the explanatory measure. The print
method says so. Do not compare down that column across estimators.

`WRS2` is in **Suggests**, not Imports. Robust specifications run when it is
installed and are skipped with a stated reason when it is not.

---

## 4. Conventions

- Prefix `anova_*` for exported functions, matching the existing `mlm_*`,
  `efa_*`, `cor_*` families.
- Internal helpers get a leading dot and are not exported.
- Reuse `format_p()` and `sig_stars()` from `report_helpers.R`. Do not write
  new p-value formatters.
- Report objects are S3 with a print method, following `mlm_report`.
- roxygen2 markdown, `@export`, `@importFrom` for anything outside base.

---

## 5. Extension checklist

When adding support for a new design:

1. Add the required concepts to the reference registry, with citations. Mark
   `verified = FALSE` until confirmed against the book.
2. Write the tests as Layer 3 functions returning findings.
3. Write the Layer 2 engine listing which findings the design needs.
4. Add recommendation rules for the new findings.
5. Register the design in `anova_check()`'s dispatch.
6. Verify against a known result before considering it done.

Layers 1 and 5 should not need to change.
