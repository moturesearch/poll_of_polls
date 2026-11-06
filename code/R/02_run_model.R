

#' Fit the national multi-party polling model
#'
#' Fits a Bayesian Dirichlet regression with [brms::brm()] (Stan via
#' `cmdstanr`) to the poll-level party shares. It is the core "poll
#' aggregator": it estimates the underlying national vote share of each party
#' as a smooth function of time, adjusting for house effects.
#'
#' **Response.** `cbind(other, act, grn, lab, nat, nzf, tpm, top)`, i.e. the
#' eight vote-share proportions in each poll (which sum to 1), modelled with a
#' Dirichlet distribution.
#'
#' **Mean (location) model.** `1 + s(days_diff, k = K_SET)`: a party-specific
#' intercept plus a penalised spline over time, so support can drift smoothly.
#' `(1 | gr(pollster, dist = "student"))` adds a pollster random intercept
#' (house effect) with a Student-t distribution, which is more tolerant of an
#' occasional outlying pollster than a normal. The "General election" results
#' are treated as one of the pollsters, anchoring the model to actual results.
#'
#' **Precision (`phi`) model.** `1 + precision + gp(days_diff, k = K_SET,
#' scale = TRUE) + (1 | pollster)`: polls with larger samples (`precision =
#' log(sample_size)`) are allowed to be more precise, precision may vary
#' smoothly over time (approximate Gaussian process), and each pollster gets
#' its own precision offset.
#'
#' **Sampler settings** (fixed in the function): 4 chains x 2000 iterations
#' (half warm-up), 16 cores with 4 threads per chain, `adapt_delta = 0.80`,
#' `max_treedepth = 12`, seed 2317, `init = 0`. All parameters are saved
#' (`save_pars(all = TRUE, latent = TRUE)`) so that `posterior_predict()` and
#' `posterior_epred()` work with new pollster levels. Adjust `cores` and
#' `threads` to your machine. Fitting takes a long time, and the elapsed time is
#' reported via `message()`.
#'
#' @param model_data Data frame from the `polls_predicted` target (the output
#'   of [predict_op()]). Must contain the columns `other`, `act`, `grn`, `lab`,
#'   `nat`, `nzf`, `tpm`, `top`, `days_diff`, `precision` and `pollster`.
#'
#' @return A fitted `brmsfit` object.
#'
#' @section Requires:
#' The global `K_SET` (number of basis functions for the spline and GP),
#' defined in `.Rprofile`. This is the main smoothness control: larger values
#' allow a wigglier trend.
#'
#' @seealso [run_op_model()] for the same structure applied to the TOP/other
#'   split; [make_combined_polls()] for how the input is prepared.
#' @noRd
run_model <- function(model_data) {
  
  form <- brms::bf(cbind(other, act, grn, lab, nat, nzf, tpm, top) ~ 1 + s(days_diff, k = K_SET) + (1 | gr(pollster, dist = "student")),
  phi ~ 1 + precision + gp(days_diff, k = K_SET, scale = TRUE) + (1 | pollster)) # Updated model
  
  
  start_time <- Sys.time()
  model_d <- brms::brm(formula = form, data = model_data, family = dirichlet(), 
  iter = 2000, cores = 16, chains = 4, threads = threading(4), init = 0.0, thin = 1,
  seed = 2317, control = list(max_treedepth = 12, adapt_delta = 0.80),
  save_pars = save_pars(all = TRUE, latent = TRUE), 
  backend = "cmdstanr")
  end_time <- Sys.time()
  message(glue("Model run succesfully - Time elapsed: {end_time - start_time}"))
  
  model_d
}

#' Fit the Opportunities Party (TOP) imputation model
#'
#' Many polls did not report TOP separately, but TOP is one of the modelled
#' parties, so its share must be inferred for those polls. This function fits
#' a two-category Dirichlet model of how the combined "TOP + other" share
#' divides between the two.
#'
#' **Response.** `cbind(oth_share, top_share)`, where `oth_share = other /
#' (other + top)` and `top_share = 1 - oth_share`, using only polls that
#' reported both.
#'
#' **Structure.** Identical to [run_model()]: a spline over time and a
#' Student-t pollster effect in the mean, and `precision`, a time GP and a
#' pollster effect in `phi`. The sampler settings are also identical (4 chains,
#' 2000 iterations, seed 2317, `cmdstanr` backend).
#'
#' @param model_data Data frame with `oth_share`, `top_share`, `days_diff`,
#'   `pollster` and `precision`. The `op_model` target builds it from
#'   `polls_combined`: it keeps complete rows and drops rows where either share
#'   is not strictly positive.
#'
#' @return A fitted `brmsfit` object, passed to [predict_op()].
#'
#' @section Requires:
#' The global `K_SET` from `.Rprofile`.
#' @noRd
run_op_model <- function(model_data) {
  
  form <- brms::bf(cbind(oth_share, top_share) ~ 1 + s(days_diff, k = K_SET) + (1 | gr(pollster, dist = "student")),
  phi ~ 1 + precision + gp(days_diff, k = K_SET, scale = TRUE) + (1 | pollster))
  
  
  start_time <- Sys.time()
  model_op <- brms::brm(formula = form, data = model_data, family = dirichlet(), 
  iter = 2000, cores = 16, chains = 4, threads = threading(4), init = 0.0, thin = 1,
  seed = 2317, control = list(max_treedepth = 12, adapt_delta = 0.80),
  save_pars = save_pars(all = TRUE, latent = TRUE), 
  backend = "cmdstanr")
  end_time <- Sys.time()
  message(glue("Model run succesfully - Time elapsed: {end_time - start_time}"))
  
  model_op
}

#' Impute TOP support for polls that did not report it
#'
#' Splits the polls into those that report TOP (`top` present and above zero)
#' and those that do not (`top` is `NA` or 0). For the latter, the posterior
#' mean TOP share of the "TOP + other" total is predicted from `model_op` and
#' used to divide the reported `other` figure:
#' `top = other * top_share_hat` and `other = other * (1 - top_share_hat)`.
#' Polls that already report TOP are left untouched.
#'
#' The imputed value is a single posterior mean per poll (the mean of
#' [brms::posterior_epred()] across draws), so uncertainty about the imputation
#' is **not** carried into the national model. New pollster levels are
#' allowed.
#'
#' @param model_op A fitted `brmsfit` from [run_op_model()]. The response order
#'   must be `cbind(oth_share, top_share)` because the second response
#'   dimension is taken as the TOP share.
#' @param polls_combined Data frame from [make_combined_polls()].
#'
#' @return A data frame of all polls, sorted by `median_date` and `pollster`.
#'   If no polls needed imputing, only the known rows are returned (without the
#'   `inferred` column). Otherwise two columns are added: `top_original`
#'   (the TOP value before imputation) and `inferred` (`TRUE` for polls
#'   whose `top` was imputed). The join used to recover `top_original` is on
#'   `start_date` and `pollster`, so a duplicate pair would duplicate rows.
#'
#' @seealso [run_op_model()]
#' @noRd
predict_op <- function(model_op, polls_combined) {
  
  # Rows where TOP already has a value — leave untouched
  known     <- polls_combined |> filter(!is.na(top), top > 0)
  
  # Rows where TOP existed but value is missing — predict
  to_pred   <- polls_combined |>
  filter(is.na(top) | top == 0)
  
  if (nrow(to_pred) == 0) {
    return(bind_rows(known) |> arrange(median_date, pollster))
  }
  
  # posterior_epred returns [draws x obs x response]
  # response order matches cbind(oth_share, top_share)
  epreds <- brms::posterior_epred(
    model_op, 
    newdata           = to_pred, 
    allow_new_levels  = TRUE
  )
  
  # Mean predicted top_share across draws (dimension 3, index 2)
  top_share_pred <- apply(epreds[, , 2], 2, mean)
  
  # Scale back to party support:
  # top   = other * top_share
  # other = other * oth_share (= other - top)
  predicted <- to_pred |>
  mutate(
    top   = other * top_share_pred,
    other = other * (1 - top_share_pred)
  )
  
  bind_rows(known, predicted) |>
  arrange(median_date, pollster) |> 
  left_join(
    polls_combined |> 
      rename(top_original = top) |> 
      select(start_date, pollster, top_original), 
    by = c("start_date","pollster")
  ) |> 
  mutate(inferred = is.na(top_original))
}