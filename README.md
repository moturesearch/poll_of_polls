# poll_of_polls
Motu/NZHerald Poll-of-polls model for the 2026 general election

A Bayesian poll aggregator for NZ party vote share, plus a two-stage electorate model that turns national vote draws into electorate-level party and candidate vote predictions (and electorate win probabilities). The workflow is a [`targets`](https://docs.ropensci.org/targets/) pipeline; packages are pinned with [`renv`](https://rstudio.github.io/renv/).

## Setup

**Requirements**
- R 4.6.0 (the version in `renv.lock`; other recent versions will likely work)
- A C++ toolchain (Rtools on Windows, Xcode command line tools on macOS, `build-essential` on Linux)
- [CmdStan](https://mc-stan.org/cmdstanr/), installed through `cmdstanr` (the models use the `cmdstanr` backend)
- Internet access (polls are scraped from Wikipedia's opinion-polling page for the election year)

**Steps**
1. Clone the repo and open R in the project root (the `.Rprofile` activates `renv` automatically).
2. Restore the package library:
   ```r
   renv::restore()
   ```
3. Install CmdStan if you don't already have it:
   ```r
   cmdstanr::check_cmdstan_toolchain(fix = TRUE)
   cmdstanr::install_cmdstan()
   ```
4. Download the pre-fitted electorate models (~400 MB). They are too large for git, so they are attached to the `models-v1` [GitHub release](https://github.com/moturesearch/poll_of_polls/releases/tag/models-v1). Run this from the project root:
   ```r
   base <- "https://github.com/moturesearch/poll_of_polls/releases/download/models-v1/"
   files <- c(
     electorate_model_stage1.rds = "8ace2fd8fdfcae543c5a28ad64b88f3729a034ae2f2d2f0a5b6c365d88d0fae0",
     electorate_model_stage2.rds = "2b797561023f2ea597c0876770d286c28a115121c2f3b667de3104c6a438dec8"
   )
   options(timeout = 1800)
   for (f in names(files)) {
     dest <- file.path("electorate_model", f)
     if (!file.exists(dest)) download.file(paste0(base, f), dest, mode = "wb")
     stopifnot(unname(tools::sha256sum(dest)) == files[[f]])  # verify the download (R >= 4.5)
   }
   ```
5. Run the pipeline:
   ```r
   targets::tar_make()
   ```

The national model is refitted on every run and can take a while. Results are written to `outputs/` (created automatically, git-ignored).

## What's in the repo

| Path | Contents |
|---|---|
| `_targets.R` | The pipeline definition |
| `code/R/` | Functions sourced by the pipeline (scraping/cleaning, national models, electorate prediction) |
| `.Rprofile` | Shared constants (pollster lookup, column aliases, number of draws, etc.) |
| `data/polls_historic.rds` | Curated historic polls that new scrapes are appended to |
| `electorate_model/` | Pre-fitted electorate models (stage 1 and 2) and the 2026 electorate/candidate prediction grid. These are static inputs and are not re-estimated by the pipeline. The two model files (`*.rds`, ~400 MB) are not stored in git; download them from the GitHub release (see Setup, step 4). `electorate_prediction_data.rds` is in the repo |
| `outputs/` | Generated: dated data snapshots, fitted model, and electorate summaries |

## What the pipeline does

1. **Polls** (`polls_combined`): scrapes the Wikipedia polling table for `year` (2026), combines it with `data/polls_historic.rds`, de-duplicates, standardises pollster names, converts shares to proportions, and adds the time axis (`days_diff`, with 0 = the run date `today`) and poll precision.
2. **TOP imputation** (`op_model`, `polls_predicted`): not all polls report TOP separately. A Dirichlet model of how the "TOP + other" share splits is fitted and used to impute TOP where it is missing.
3. **National model** (`national_model`): a multi-party Dirichlet `brms` model of vote share with a spline over time and pollster effects. The input data and fitted model are saved with the date in `outputs/data/` and `outputs/models/`.
4. **National predictions** (`ppreds_long`): posterior-predictive draws of the national vote split on election day.
5. **Electorate predictions** (`electorate_votes_party`, `electorate_votes_candidate`): for each national draw, stage 1 simulates party votes in every electorate and stage 2 simulates candidate votes and flags the winner. The combined draws are saved to `outputs/summaries/`.
6. **No-TOP scenario** (`*_no_top` targets): repeats step 5 using only the national draws where TOP falls below 5%, i.e. a scenario in which TOP doesn't clear the threshold.

Useful commands:
```r
targets::tar_visnetwork()                 # view the pipeline graph
targets::tar_read(electorate_votes_candidate)   # load a result
targets::tar_invalidate(today)            # force date-dependent targets to re-run on a later day
```

## Licence
See `LICENSE`.
