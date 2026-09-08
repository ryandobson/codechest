---
name: codechest-function
description: Write, edit, or refactor an R function inside the codechest package (Ryan's personal R package for data analysis, modeling, and APA-style reporting). Covers file placement, the dot-prefixed-internal-helper convention, the single-function + run_X()/save_X() wrapper pattern, roxygen2 documentation, testthat coverage, and syncing NAMESPACE/man pages. Use this whenever the user asks to add, write, create, port, refactor, fix, clean up, or edit a function that lives in or is headed for codechest — including porting code in from collab_chest/helper_functions.R or collab_chest/pmi_helper_functions.R — even if they just say "add a function for X" or "fix this function" without naming the skill.
---

# Writing and editing codechest functions

codechest is a one-person R package, not a scratch script. Every function that
lands in `R/` is expected to be documented, exported deliberately (or not),
and — for anything public — backed by a test. The payoff is that six months
from now the author can trust `?function_name` and the test suite instead of
re-reading the source. That's the bar for anything this skill touches.

The package's most mature code — `R/anova_assumptions.R`, `R/anova_describe.R`,
`R/anova_multiverse.R`, `R/anova_refs.R`, `R/var_labels.R`, and their tests in
`tests/testthat/test-anova.R` — is the model to imitate. Older files
(`R/mlm_centering.R`, `R/reliability_helpers.R`, etc.) are functional but
thinner on docs and have no tests; don't copy their gaps, copy the anova
family's habits.

## Step 1 — Work out where the function belongs

Before writing anything, check whether it already exists in another form:

1. **Search first.** Grep `R/` for the concept (e.g. a keyword from the
   function's purpose) and check `references/shared-helpers.md` in this skill
   for a list of small helpers that get reinvented often (p-value formatting,
   significance stars, safe list-plucking, APA table styling). If one exists,
   call it — don't write a second copy.
2. **Pick the file.** codechest's files are organized by *concern*, one topic
   per file (`mlm_centering.R`, `reliability_helpers.R`, `dynamic_histogram.R`).
   - If the function extends an existing concern (e.g. another APA table
     builder, another EFA diagnostic), add it to that file.
   - If it's a genuinely new concern, create a new file named for the concern
     in `snake_case` — no dates, versions, or "V1"/"V2" in the filename
     (`kbl_desc_20241126.R` is a cautionary example already flagged for
     rename). Git history is where versioning belongs, not the filename.
3. **Check the naming pattern.** codechest has a deliberate convention for
   multi-step or batch operations: a core function that does one unit of
   work, plus a `run_*()` wrapper that applies it across a list/loop, and
   sometimes a `save_*()` wrapper that writes output to disk (see
   `apa_lmer_model()` / `run_apa_lmer_model()` / `save_apa_lmer_tables()`).
   If the new function is a batch/reporting version of something that could
   stand alone, follow that three-tier shape rather than inventing a new
   naming style.

## Step 2 — Write the function

**Internal vs. exported.** Prefix with `.` (e.g. `.norm_vec`) anything that's
an implementation detail other functions in the file rely on but a user of
the package should never call directly. A dot-prefixed function is **never**
`@export`ed — the package currently has a handful of these exported by
mistake (`.model_info`, `.wrapcat`, and others in `report_helpers.R`); don't
add to that list. If you're editing one of those existing mistakes as part of
your change, flag it to the user rather than silently "fixing" their export
status, since removing an export is a breaking change for anyone already
depending on it.

**Argument validation.** Fail loudly and early, the way `set_var_labels()` and
`anova_check()` do:
- `stop("message", call. = FALSE)` for invalid input — the message should say
  what was wrong and, where useful, show the right call shape.
- `match.arg()` for a fixed set of string options (see `mlm_groupmean()`'s
  `affix_type`), so a typo errors instead of silently doing the wrong thing.
- `warning("message", call. = FALSE)` for recoverable problems (e.g. a
  requested column that doesn't exist — see `set_var_labels()`'s
  `warn_missing`).

**Don't reinvent what's already in the package.** Before writing local
formatting/plucking/styling logic, check
`references/shared-helpers.md`. If you're building or editing an APA-style
table function specifically, read that file's "APA tables" section first —
this is an area with known duplication that's actively being cleaned up, and
new table code should compose the shared pieces rather than adding a fourth
copy of the same header/footer/border boilerplate.

**Porting from collab_chest.** If the source material is
`../collab_chest/helper_functions.R` or `../collab_chest/pmi_helper_functions.R`,
treat the port as a rewrite, not a copy-paste:
- Drop any commented-out dead code, abandoned attempts, or dated inline notes
  ("this needs fixing 12/24") — those belong in git history, not in a
  released file.
- Check the function doesn't call something that only exists informally in
  the scratch file's environment (e.g. a helper defined earlier in the same
  script but never given a home) — it needs to resolve inside the package.
- If it's one of several near-duplicate variants scattered across the scratch
  file, consolidate to one canonical version as part of the port rather than
  bringing all variants in.

**Scope the port before writing any of it.** Trace the function's real
dependency chain first — the scratch files are dense, and a single "port this
one function" request routinely turns out to need twenty or thirty helpers
underneath it. Grep for what it calls, then what those call, until the set
stops growing. If the result spans more than roughly one file's worth of
functions, say so and agree on scope before writing code. A half-finished
port is genuinely worse than an unstarted one: it leaves `R/` holding a
fraction of a subsystem that `devtools::load_all()` can't resolve, which
breaks the package for everything else until it's either finished or removed.
Staging it deliberately — engine first, verified loading, then the layer
above — keeps the package working the whole way through.

**When a port surfaces a bug in the source.** It will; this code was written
under research deadlines and the scratch files have never been under test.
The distinction that matters is whether you can *prove* the fix:
- If it's provably broken and you can write a test that fails before your fix
  and passes after (a variable referenced before assignment, an argument that
  never reaches the function it's forwarded to), fix it and add that test.
- If the behavior is suspect but you can't validate a change — a regex that
  looks too greedy, a statistical formula you'd be guessing at — leave it
  exactly as-is, document the limitation in `@details`, and tell the user.
  Preserve an author's live warning comment rather than deleting it, since it
  is an honest signal about real behavior.
- Never silently "fix" something you have no way to test. An unvalidated fix
  in a package the author will trust for real analyses is worse than a known,
  documented flaw.

## Step 3 — Document it

Every function — exported or internal — gets a roxygen block immediately
above it, in this shape (adapt sections that don't apply):

```r
#' Title in sentence case, no period
#'
#' One or two sentences on what it does and why it exists, written for
#' someone who has never seen the function before.
#'
#' @param x What this argument is, its expected type, and its default's
#'   behavior if not obvious.
#' @param group Optional; note what happens when it's NULL vs. supplied.
#'
#' @details
#' Only include this section for genuine nuance — an edge case, a formula,
#' a "why it's built this way" note. Skip it if the param docs already say
#' everything.
#'
#' @return What comes back, concretely (a data frame with which columns, a
#'   list with which named elements, an object of which S3 class).
#'
#' @examples
#' # Prefer a runnable example using a built-in dataset (PlantGrowth, mtcars)
#' # so it can execute during R CMD check. Use \dontrun{} only when the
#' # function needs data/side effects that can't run in that context.
#'
#' @references
#' Include this whenever the function implements a published statistical
#' index or procedure, with a full citation and DOI. Someone reading the
#' result needs to know which formula it implements — there are usually
#' several competing ones.
#'
#' @seealso [related_function()] for the complementary operation.
#' @family <shared family tag, if this joins a group like the anova_* family>
#' @export
```

For internal (`.`-prefixed) functions, leave off `@export` and add `@noRd`,
which tells roxygen not to generate a help page at all — that's what
`report_helpers.R` does for `%||%`. Use `@keywords internal` instead when the
function should still get an `.Rd` page (documented, but kept out of the
package index) — that's the right choice for something a curious user might
reasonably look up. Use `@family` when
the function is one of a set that belongs together (see how the whole
`anova_*` family cross-references itself) — it's what makes `?anova_check`
show "See Also" links to the rest of the family.

## Step 4 — Test it

Every new **exported** function needs a testthat test before the work is
considered done — this was the explicit ground rule agreed with the package
author, treated the same way `test-anova.R` treats the anova family: not
optional, not "add later." Internal (dot-prefixed) helpers get tests when
they carry real logic worth pinning down (see `.levene`, `.y_limits`,
`.point_style` all being tested directly in `test-anova.R`), but skip trivial
one-liners.

Conventions, taken directly from `tests/testthat/test-anova.R`:
- Group tests by *topic/family* in `tests/testthat/test-<topic>.R`, not one
  file per function — if you're adding to an existing family, add to its
  existing test file rather than starting a new one.
- Prefer base-R built-in datasets (`PlantGrowth`, `mtcars`, `warpbreaks`) or a
  small `set.seed()`-seeded simulation built inline, so tests run anywhere
  without shipped data files.
- `skip_if_not_installed("pkg")` around anything that touches an optional
  dependency (the package's own `car`, `WRS2`, `ggplot2`, `haven` are all in
  `Suggests`, not `Imports` — check `DESCRIPTION` before assuming a package
  is always available).
- Use `expect_identical()` when an exact value/type match matters,
  `expect_equal(..., tolerance = ...)` for floating-point results, and check
  both the happy path and that bad input actually errors
  (`expect_error(fn(...), "some message fragment")`).
- Test structure, not just values, where relevant: `expect_s3_class()`,
  `expect_named()`, `expect_type()`.

## Step 5 — Sync and verify

`Rscript` is not on PATH in this environment, so a bare `Rscript -e "..."`
will fail with "command not found" even though R is installed. Locate it
first (PowerShell):

```powershell
$rscript = (Get-ChildItem "C:\Program Files\R" -Directory |
            Sort-Object Name -Descending | Select-Object -First 1).FullName + "\bin\Rscript.exe"
& $rscript -e "devtools::document()"
```

This picks the newest installed R version rather than hardcoding one that
will go stale. Use the resolved `$rscript` path for every command below.

After writing or editing, always:

1. Run `devtools::document()` from the package root — this regenerates
   `NAMESPACE` and the `man/` pages from your roxygen comments. **Never**
   hand-edit `NAMESPACE`; it says "do not edit by hand" at the top for a
   reason, and a manual edit will just be overwritten (or silently diverge)
   the next time someone runs `document()`.
2. Run `devtools::load_all()` to make the change available in the current R
   session.
3. Run the relevant test file — `devtools::test_file("tests/testthat/test-<topic>.R")`
   or `devtools::test()` for the full suite if the change could ripple beyond
   one file (e.g. you touched a shared helper).
4. Report the actual test output to the user, not just "should work" — if
   something fails, fix it before calling the task done, the same way you'd
   treat a failing test on any other project.
5. Run `git status` afterwards. Test runs leave things behind — `psych::omega()`
   draws a plot, which drops an `Rplots.pdf` into `tests/testthat/` — and a
   side effect that quietly becomes an untracked repo file is a side effect
   nobody chose. Clean up what the run created, and gitignore it if it will
   recur.

## Editing an existing function

Extra care beyond the steps above:
- **Check callers before changing a signature.** Grep `R/` for the function
  name — codechest's `run_*`/`save_*` wrappers often call the base function
  internally, so a signature change can break a sibling function silently.
- **Update the roxygen block to match reality.** A stale `@param` or
  `@return` is worse than none, because it actively misleads. If you fix a
  bug (e.g. an argument that was silently ignored, like `scale_reliabilities()`
  not forwarding `omega_h`), update or add a test that would have caught it —
  don't just patch the line and move on.
- **Update the function's test file**, not just add a new one, when you
  change behavior an existing test already covers — a passing-but-stale test
  is a false signal of correctness.
- **Watch for dead branches while you're in there.** If you notice a branch
  that can never execute (an `if` guarding a condition already ruled out by
  the branch above it), that's worth flagging or removing, but don't expand
  scope into an unrelated cleanup pass unless the user asked for one.

## Reference

`references/shared-helpers.md` — helpers that already exist somewhere in the
package (or in `collab_chest/helper_functions.R`, waiting to be ported) and
should be reused rather than reimplemented. Check it before writing anything
that smells like formatting, styling, or plucking-from-a-list logic.
