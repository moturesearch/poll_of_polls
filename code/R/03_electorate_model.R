
# 04_electorate_model.R
# Functions that turn a national vote-share forecast into electorate-level
# predictions. The electorate models themselves (a party-vote model and a
# candidate-vote model) are fitted offline and loaded as static, pre-estimated
# objects; only the prediction steps defined here are re-run when the national
# model is updated.

#' Reshape national posterior predictions into long format
#'
#' Converts the draws-by-observations-by-parties array returned by
#' [brms::posterior_predict()] on the national model into a long table with
#' one row per draw and party. This is the hand-off between the national model
#' and the electorate predictions: each `iter` is one plausible national vote
#' split, which [predict_votes_party()] then pushes down to electorates.
#'
#' The function is written for a single prediction date (one row of `newdata`).
#' The `date` index is wrapped with `%% 1`, so every row is labelled with
#' `dates[1]`. If the model is ever predicted at more than one date, the
#' indexing must be changed.
#'
#' @param ppreds Numeric array with dimensions `[draws, observations, parties]`
#'   and the party names in the third dimension's `dimnames`.
#' @param dates A vector of dates, one per prediction row (only the first is
#'   used; see above).
#'
#' @return A [data.table::data.table()] with columns `iter` (draw number,
#'   `1..n_draws`), `date`, `party` (e.g. `"lab"`, `"nat"`, `"other"`) and
#'   `draw` (the predicted vote share as a proportion).
#' @noRd
ppreds_to_long <- function(ppreds, dates) {

  dn <- dimnames(ppreds)
  n_draws <- dim(ppreds)[1]
  n_parties <- dim(ppreds)[3]

  l <- seq_len(n_draws * n_parties)
  data.table::data.table(
    iter = ((l - 1L) %% n_draws) + 1L,
    date = dates[(((l - 1L) %/% n_draws) %% 1) + 1L],
    party = dn[[3]][((l - 1L) %/% (n_draws)) + 1L],
    draw = as.vector(ppreds)
  )
}

#' Simulate party votes in every electorate for one national draw (stage 1)
#'
#' For a single draw of the national vote split, predicts the number of party
#' votes each party receives in each electorate (`Votes_party`) using the
#' party-vote electorate model. The national share enters as the covariate
#' `election_vote`.
#'
#' Steps:
#' 1. Take the rows of `ppreds_long` for `iter_id` and join them to the
#'    prediction grid by `party`, renaming the draw to `election_vote`.
#' 2. Split the grid into rows whose `party` was seen when the model was fitted
#'    and rows with an unseen party (the "other" placeholder rows). The model
#'    contains a `(0 + candidate_exists | gr(candidate_party, by = party))`
#'    term, which needs a donor level from a trained party. Unseen parties have
#'    none, so brms would error. Because `candidate_exists` is 0 for those rows
#'    the term contributes nothing anyway, so they are predicted with that term
#'    removed from the formula (`re_formula`).
#' 3. Call [brms::posterior_predict()] for the `Votesparty` response, one
#'    posterior draw each (`ndraws = 1`), selecting the draw
#'    `party_draw_ids[iter_id]`. Pairing national draw *i* with electorate-model
#'    draw *i* spreads the electorate-model uncertainty across the full
#'    posterior, so each national draw sees a different set of electorate
#'    parameters.
#'
#' @param iter_id Integer index of the national draw (a value of `iter` in
#'   `ppreds_long`).
#' @param ppreds_long Long-format national draws from [ppreds_to_long()].
#' @param prediction_grid_2026 One row per electorate x party/candidate, with the
#'   covariates the electorate model needs (e.g. `candidate_party`,
#'   `candidate_exists`, `no_candidate`, `new_candidate`, `is_incumbent`,
#'   `year`). Built in the `prediction_grid_2026` target.
#' @param electorate_model Fitted multivariate `brmsfit` containing the
#'   `Votesparty` response (the `electorate_model_a` target).
#' @param party_draw_ids Integer vector of posterior draw indices of
#'   `electorate_model`, one per national draw (the `party_draw_ids` target).
#'
#' @return A data frame with one row per grid row, with columns `iter`,
#'   `electorate_name`, `party`, `candidate_name`, `candidate_id`,
#'   `candidate_party`, `new_candidate`, `no_candidate`, `candidate_exists`,
#'   `year`, `is_incumbent`, `election_vote` and `Votes_party` (the simulated
#'   party votes). Intended to be row-bound over all `iter_id`s with
#'   [purrr::map_dfr()].
#'
#' @seealso [predict_votes_candidate()] for stage 2.
#' @noRd
predict_votes_party <- function(iter_id, ppreds_long, prediction_grid_2026, electorate_model, party_draw_ids) {

  election_vote_i <- ppreds_long[ppreds_long$iter == iter_id, c("party", "draw")]

  newdata_i <- prediction_grid_2026 |>
    dplyr::inner_join(election_vote_i, by = "party") |>
    dplyr::rename(election_vote = draw) |> 
    dplyr::mutate(
      logit_election = qlogis(election_vote),
      logit_poll_p = logit_election - logit_poll_p
    )

  # The (0 + candidate_exists | gr(candidate_party, by = party)) term can only
  # borrow a donor candidate_party level from a party seen at training time.
  # The "other" placeholder rows (no_candidate = 1, candidate_exists always 0)
  # aren't in electorate_model's training data, so brms has no donor to sample
  # and errors. Since candidate_exists == 0 zeroes this term out for those rows
  # regardless, predict them with the term dropped instead of making brms
  # sample a donor level that doesn't exist.
  trained_by_levels <- unique(as.character(electorate_model$data$party))
  has_donor <- newdata_i$party %in% trained_by_levels

  no_donor_re <- update(
    electorate_model$formula$formula,
    . ~ . - (0 + candidate_exists | gr(candidate_party, by = party))
  )

  pp <- vector("numeric", nrow(newdata_i))

  if (any(has_donor)) {
    pp[has_donor] <- as.vector(brms::posterior_predict(
      electorate_model,
      newdata = newdata_i[has_donor, ],
      resp = "Votesparty",
      allow_new_levels = TRUE,
      ndraws = 1,
      draw_ids = party_draw_ids[iter_id]
    ))
  }

  if (any(!has_donor)) {
    pp[!has_donor] <- as.vector(brms::posterior_predict(
      electorate_model,
      newdata = newdata_i[!has_donor, ],
      resp = "Votesparty",
      re_formula = no_donor_re,
      allow_new_levels = TRUE,
      ndraws = 1,
      draw_ids = party_draw_ids[iter_id]
    ))
  }

  newdata_i |>
    dplyr::mutate(
      iter = iter_id, 
      Votes_party = pp,
      Votes_party_per = Votes_party / Votes_party_sum,
      logit_party = qlogis(Votes_party_per),
      logit_poll_c =  logit_party  - logit_poll_c
    )
}


#' Simulate candidate votes and electorate winners for one national draw (stage 2)
#'
#' Takes the stage 1 output for one national draw ([predict_votes_party()]) and
#' predicts the votes each candidate receives (`Votes_candidate`) with the
#' candidate-vote electorate model, using the simulated `Votes_party` as an
#' input. It then flags which candidate wins each electorate in each simulation.
#'
#' Steps:
#' 1. Subset `electorate_votes_party` to `iter_id`.
#' 2. Randomly choose `n_candidate_draws` posterior draws of `electorate_model`
#'    (without replacement, so this uses [sample()] and is affected by the RNG
#'    seed state) to represent model uncertainty at this stage.
#' 3. Drop placeholder rows (`no_candidate == 1`): no candidate stands for that
#'    party in that electorate, and the model is trained on real candidates only.
#' 4. Call [brms::posterior_predict()] for the `Votescandidate` response,
#'    which gives an `n_candidate_draws x n_rows` matrix, and flatten it to long
#'    form.
#' 5. Set `wins_draw` to `TRUE` for the candidate with the most votes within each
#'    `electorate_name` x `party_iter` x `candidate_draw` group. Averaging
#'    `wins_draw` over all simulations gives each candidate's win probability.
#'
#' The total number of simulations per candidate is the number of national
#' draws times `n_candidate_draws` (by default 100 x 100).
#'
#' @param iter_id Integer index of the national draw to process.
#' @param n_candidate_draws Number of electorate-model posterior draws to
#'   simulate per national draw. Default 100.
#' @param electorate_votes_party Stage 1 output covering all draws, from
#'   [predict_votes_party()] (the `electorate_votes_party` target).
#' @param electorate_model Fitted multivariate `brmsfit` containing the
#'   `Votescandidate` response (the `electorate_model_b` target).
#'
#' @return A [data.table::data.table()] with one row per candidate per
#'   candidate draw: `party_iter`, `candidate_draw`, `electorate_name`, `party`,
#'   `candidate_name`, `is_incumbent`, `year`, `Votes_party` (carried over from
#'   stage 1), `Votes_candidate` and `wins_draw` (logical).
#'
#' @seealso [predict_votes_party()]
#' @noRd
predict_votes_candidate <- function(iter_id, n_candidate_draws = 100, electorate_votes_party, electorate_model) {

  stage1_i <- electorate_votes_party[electorate_votes_party$iter == iter_id, ]

  # Placeholder rows (no_candidate == 1) are parties with no candidate standing in
  # the electorate. The candidate model is trained on real candidates only, so
  # they can't be predicted (and can't win) -- drop them.
  stage1_i <- stage1_i[stage1_i$no_candidate == 0, ]

  # Stage 2 predicts from the simulated party vote share rather than the raw count.
  if ("Votes_party_sum" %in% names(stage1_i)) {
    stage1_i$Votes_party_per <- stage1_i$Votes_party / stage1_i$Votes_party_sum
  }

  candidate_draw_ids <- sample(seq_len(brms::ndraws(electorate_model)), n_candidate_draws)

  pp <- brms::posterior_predict(
    electorate_model,
    newdata = stage1_i,
    resp = "Votescandidate",
    allow_new_levels = TRUE,
    draw_ids = candidate_draw_ids
  )

  n_obs <- ncol(pp)

  data.table::data.table(
    party_iter      = iter_id,
    candidate_draw  = rep(seq_len(n_candidate_draws), times = n_obs),
    electorate_name = rep(stage1_i$electorate_name, each = n_candidate_draws),
    party           = rep(stage1_i$party,           each = n_candidate_draws),
    candidate_name  = rep(stage1_i$candidate_name,  each = n_candidate_draws),
    is_incumbent    = rep(stage1_i$is_incumbent,    each = n_candidate_draws),
    year            = rep(stage1_i$year,            each = n_candidate_draws),
    Votes_party     = rep(stage1_i$Votes_party,     each = n_candidate_draws),
    Votes_candidate = as.vector(pp)
  ) |> 
  # Summarise by the proportion of the time that the frontrunner wins
  dplyr::mutate(
    wins_draw = row_number(dplyr::desc(Votes_candidate)) == 1,
    .by = c(electorate_name, party_iter, candidate_draw)
  )
}

#' Transliterate character columns to plain ASCII
#'
#' Replaces accented and macronised characters in every character column with
#' their closest Latin-ASCII equivalent (e.g. "Māori" becomes "Maori") using
#' [stringi::stri_trans_general()]. This is applied to the CSV summaries before
#' writing so that they open cleanly in tools that mishandle UTF-8, such as
#' Excel on Windows. Non-character columns are unchanged.
#'
#' @param df A data frame.
#'
#' @return `df` with all character columns transliterated.
#' @noRd
to_ascii <- function(df) {
  df |> mutate(across(where(is.character), ~ stringi::stri_trans_general(.x, "Latin-ASCII")))
}


