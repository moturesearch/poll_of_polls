# _targets.R
# Targets pipeline for the Motu Poll-of-polls
#
# Run with:   targets::tar_make()
# Visualise:  targets::tar_visnetwork()
# Load result: targets::tar_read(model_obj_path)

library(targets)
library(tarchetypes)

# Clear _targets/workspaces
file.remove(list.files("_targets/workspaces", full.names = TRUE))

# ----------------------------------------------------------------
# External constants set in .Rprofile
# ----------------------------------------------------------------

# Global options
tar_option_set(
  packages = c(
    "dplyr", "tidyr", "stringr", "purrr", "glue",
    "readxl", "here", "rvest", "janitor",
    "brms", "cmdstanr", "lubridate",
    "posterior", "tidybayes", "mgcv",
    "ggplot2", "jpeg", "grid", "gridExtra"
  ),
  format = "qs"
)

# Source all function files in R/
tar_source("code/R/")

tar_plan(  
  
  # Election year whose Wikipedia opinion-polling page is scraped and appended to the historic polls.
  tar_target(year, 2026),
  
  # Run date. Anchors the model's time axis (days_diff = 0 is today) and names the output files.
  # Invalidate it (tar_invalidate(today)) to re-run the date-dependent targets on a later day.
  tar_target(today, Sys.Date()),
  
  # ----------------------------------------------------------------
  # Path constants
  # ----------------------------------------------------------------
  # Path to the curated historic polls (data/polls_historic.rds).
  tar_target(historic_path, here("data", "polls_historic.rds")),
  
  # ----------------------------------------------------------------
  # 01 - Scrape new polls, combine with historic, save
  # Rewrite to include TOP in data collection for all years
  # Join for sample size
  # ----------------------------------------------------------------
  
  # Modelling dataset: historic polls + newly scraped polls, de-duplicated and cleaned
  # (shares as proportions, days_diff, precision, election cycle). Returns a data frame, not a file path.
  tar_target(
    name = polls_combined,
    command = {
      make_combined_polls(year, historic_path, today)
    }
  ),
  
  # ----------------------------------------------------------------
  # 02 - Run TOP model
  # ----------------------------------------------------------------
  
  # Dirichlet model of how the "TOP + other" share splits between the two; used to impute TOP
  # for polls that did not report it. Fitted on polls with a non-zero TOP and other share.
  tar_target(
    name = op_model,
    command = {
      
      model_data_op <- polls_combined |> 
      select(
        median_date, 
        days_diff,
        pollster, 
        sample_size,
        precision, 
        top, 
        other
      ) |> 
      na.omit() |> 
      mutate(
        oth_share = other/(other + top),
        top_share = 1 - oth_share
      ) |> filter(
        oth_share > 0,
        top_share > 0
      )
      
      # Run model
      op_model <- run_op_model(model_data_op)
    }
  ),
  
  # ----------------------------------------------------------------
  # 03 - Predict on TOP model
  # ----------------------------------------------------------------
  
  # polls_combined with TOP imputed (posterior mean) where missing; flags imputed rows in `inferred`.
  tar_target(
    name = polls_predicted,
    command = {
      predict_op(op_model, polls_combined)
    }
  ),

  # ----------------------------------------------------------------
  # 04 - Run main model
  # Saves timestamped data and model objects to outputs/
  # ----------------------------------------------------------------
  # National multi-party Dirichlet polling model (spline over time + pollster effects), refitted on every run.
  # NB: the commented-out readRDS() lines load a saved model and discard it; run_model() always refits.
  tar_target(
    name = national_model,
    command = {
      run_model(polls_predicted)
    }
  ),

  # Saves a dated snapshot of the model input to outputs/data/ and tracks the file.
  tar_target(
    name = national_data_path,
    command = {
      # Save timestamped data snapshot
      data_path <- here::here("outputs/data", paste0("data_", today, ".rds"))
      saveRDS(polls_predicted, data_path)

      data_path
    }, 
    format = "file"
  ),

  # Saves the fitted national model to outputs/models/ (dated) and tracks the file.
  tar_target(
    name = national_model_path,
    command = {
      model_path <- here::here("outputs/models", paste0("model_", today, ".rds"))
      saveRDS(national_model, model_path)
    
      message(glue("Model saved to {model_path}"))
      
      model_path
    }, 
    format = "file"
  ),
  
  # ----------------------------------------------------------------
  # 05 - Run electorate predictions
  # ----------------------------------------------------------------

  # Static (not re-estimated) electorate model, stage 1: party votes per electorate.
  tar_target(
    name = electorate_model_a,
    command = {
      readRDS("electorate_model/electorate_model_stage1.rds") # model_15a beta_binomial
    }
  ),

  # Static (not re-estimated) electorate model, stage 2: candidate votes.
  tar_target(
    name = electorate_model_b,
    command = {
      readRDS("electorate_model/electorate_model_stage2.rds") # model_17b beta_binomial
    }
  ),
  
  # Electorate x party/candidate grid for 2026 (supplied externally); adds the year factor and candidate_party
  # key the electorate models expect.
  tar_target(
    name = prediction_grid_2026,
    command = {
      # Beta-binomial models need the number of trials: total party votes and total candidate
      # votes in the electorate. Unknown for 2026, so assume each electorate's 2023 totals; electorates not in
      # the training data (e.g. new boundaries) fall back to the median 2023 total. Only columns the loaded
      # models were trained with are added.
      sum_2023 <- function(model, var_name) {
        if (!var_name %in% names(model$data)) return(NULL)
        out <- model$data[model$data$year == "2023", c("electorate_name", var_name)]
        out <- out[!duplicated(out$electorate_name), ]
        out
      }

      grid <- readRDS("electorate_model/electorate_prediction_data.rds") # Fixed, complete
      
      add_total <- function(grid, model, var_name) {
        totals <- sum_2023(model, var_name)
        if (is.null(totals)) return(grid)
        grid <- dplyr::left_join(grid, totals, by = "electorate_name")
        grid[[var_name]][is.na(grid[[var_name]])] <- median(totals[[var_name]])
        grid
      }
      
      grid <- grid |>
      add_total(electorate_model_a, "Votes_party_sum") |>
      add_total(electorate_model_b, "Votes_candidate_sum")
      
      grid
    }
  ),

  # N_NATIONAL_DRAWS evenly spaced draw indices across the full posterior of the stage 1 electorate model
  # (4 chains x 1000 each here) -- draw_ids = 1:100 would silently only sample chain 1.
  tar_target(
    name = party_draw_ids,
    command = {
      round(seq(1, brms::ndraws(electorate_model_a), length.out = N_NATIONAL_DRAWS))
    }
  ),

  # One-row newdata for national prediction: election day (days_diff = 0), the "General election"
  # pollster, and the smallest observed precision.
  tar_target(
    name = data_new_d,
    command = {
      data.frame(
        days_diff = 0,
        date = today,
        month = lubridate::month(today),
        pollster = "General election",
        precision = min(polls_predicted$precision)
      )
    }
  ),

  # 100 posterior-predictive draws of the national vote split on election day, in long format.
  tar_target(
    name = ppreds_long,
    command = {
      ppreds <- brms::posterior_predict(
        national_model,
        newdata = data_new_d,
        allow_new_levels = TRUE,
        ndraws = N_NATIONAL_DRAWS
      )
      
      ppreds_to_long(ppreds, data_new_d$date)
    }
  ),

  # Stage 1: for each national draw, simulate party votes in every electorate (predict_votes_party()).
  tar_target(
    name = electorate_votes_party,
    command = {
      purrr::map_dfr(
        seq_len(max(ppreds_long$iter)), 
        predict_votes_party,
        ppreds_long = ppreds_long,
        prediction_grid_2026 = prediction_grid_2026,
        electorate_model = electorate_model_a,
        party_draw_ids = party_draw_ids
      )
    }
  ),

  # Stage 2: for each stage 1 draw, simulate candidate votes and flag the winner (predict_votes_candidate()).
  tar_target(
    name = electorate_votes_candidate,
    command = {
      purrr::map_dfr(
        seq_len(max(electorate_votes_party$iter)),
        predict_votes_candidate,
        n_candidate_draws = N_CANDIDATE_DRAWS,
        electorate_votes_party = electorate_votes_party,
        electorate_model = electorate_model_b
      )
    }
  ),
  
  # Writes electorate_votes_candidate to outputs/summaries/ as a dated, ASCII only.
  tar_target(
    name = electorate_votes_candidate_path,
    command = {
      summary_path <- here::here("outputs/summaries", paste0("electorate_votes_candidate_", today, ".rds"))
      saveRDS(to_ascii(electorate_votes_candidate), summary_path)  
      
      message(glue("Summary saved to {summary_path}"))
      
      summary_path
    },
    format = "file"
  ),

  # ----------------------------------------------------------------
  # 05b - Electorate predictions conditional on TOP not clearing 5%
  # Parallel to the chain above; the same functions are reused on a filtered set of national draws.
  # ----------------------------------------------------------------

  # National posterior-predictive draws of election-day vote share where TOP is under 5%. Rejection
  # sampling: predict from every posterior draw (4000), keep those with top < 0.05, cap at 100, and renumber
  # iter to 1..K as predict_votes_party() indexes party_draw_ids by iter. TOP clears 5% in the vast majority
  # of draws, so K is small -- hence all draws are used rather than the 100 in ppreds_long.
  tar_target(
    name = ppreds_long_no_top,
    command = {
      ppreds <- brms::posterior_predict(
        national_model,
        newdata = data_new_d,
        allow_new_levels = TRUE,
        ndraws = brms::ndraws(national_model)
      )

      keep <- which(ppreds[, 1, "top"] < 0.05)
      if (length(keep) > 100) keep <- sort(sample(keep, 100))

      message(glue("{length(keep)} of {nrow(ppreds)} national draws have TOP under 5%"))
      if (length(keep) < 20) {
        stop(glue("Only {length(keep)} national draws have TOP under 5%; too few for a usable no-TOP variant."))
      }

      ppreds_to_long(ppreds[keep, , , drop = FALSE], data_new_d$date)
    }
  ),

  # Evenly spaced stage 1 posterior draw indices, one per filtered national draw (see party_draw_ids).
  # Length matches ppreds_long_no_top rather than a fixed 100, so the full posterior is still spanned.
  tar_target(
    name = party_draw_ids_no_top,
    command = {
      round(seq(1, brms::ndraws(electorate_model_a), length.out = max(ppreds_long_no_top$iter)))
    }
  ),

  # Stage 1 for the no-TOP national draws.
  tar_target(
    name = electorate_votes_party_no_top,
    command = {
      purrr::map_dfr(
        seq_len(max(ppreds_long_no_top$iter)),
        predict_votes_party,
        ppreds_long = ppreds_long_no_top,
        prediction_grid_2026 = prediction_grid_2026,
        electorate_model = electorate_model_a,
        party_draw_ids = party_draw_ids_no_top
      )
    }
  ),

  # Stage 2 for the no-TOP national draws.
  tar_target(
    name = electorate_votes_candidate_no_top,
    command = {
      purrr::map_dfr(
        seq_len(max(electorate_votes_party_no_top$iter)),
        predict_votes_candidate,
        electorate_votes_party = electorate_votes_party_no_top,
        electorate_model = electorate_model_b
      )
    }
  ),

  # Writes electorate_votes_candidate_no_top to outputs/summaries/ as a dated, ASCII only.
  tar_target(
    name = electorate_votes_candidate_no_top_path,
    command = {
      summary_path <- here::here("outputs/summaries", paste0("electorate_votes_candidate_no_top_", today, ".rds"))
      saveRDS(to_ascii(electorate_votes_candidate_no_top), summary_path)

      message(glue("Summary saved to {summary_path}"))

      summary_path
    },
    format = "file"
  )
)
