# get_polls.R
# Scrapes Wikipedia opinion polling tables for NZ general elections.
# Outputs a data frame matching the DATA EXT schema used by the Dirichlet model.

# --------------------------------------------------
# Date parsers (from read-polls.R, with edge-case handling)
# --------------------------------------------------

#' Resolve the start date of a date range split into "from" and "to" parts
#'
#' Wikipedia poll tables write fieldwork dates as ranges that abbreviate the
#' start date, e.g. `"2-7 Mar 2022"` or `"28 Feb-3 Mar 2022"`. Once such a
#' string has been split on the hyphen, the "from" half (`"2"`, `"28 Feb"`) is
#' incomplete on its own. This function borrows the missing month and/or year
#' from the "to" half, which is always complete.
#'
#' How it works: it extracts the day (and month, if present) from the "from"
#' half, then substitutes that into the "to" half in place of the end day (or
#' end day and month, if the "from" half had its own month), and parses the
#' result with [lubridate::dmy()].
#'
#' @param dates_text Character vector of length 2: `c(from, to)`, e.g.
#'   `c("2", "7 Mar 2022")` or `c("28 Feb", "3 Mar 2022")`. Whitespace should
#'   already be trimmed. Only the first two elements are used.
#'
#' @return A [Date] of length 1, or `NA` if the result cannot be parsed
#'   (parsing is quiet, no warning is raised).
#'
#' @section Limitations:
#' The year is always taken from the "to" half, so a range that spans New
#' Year (e.g. `"28 Dec-2 Jan 2023"`) will give a start date in the wrong year.
#'
#' @seealso [wikipedia_date_parser_from()], which calls this for range strings.
#' @noRd
extract_date_from <- function(dates_text) {
  # month.name and month.abb are base objects
  .months <- stringr::str_flatten(c(month.name, month.abb), collapse = "|")
  
  from_raw  <- dates_text[1]
  to_raw    <- dates_text[2]
  from_part <- stringr::str_extract(from_raw, glue::glue("\\d+[ {.months}]*"))
  to_raw |>
  stringr::str_replace(
    pattern = if_else(grepl(.months, from_part),
    glue::glue("\\d+ [{.months}]+"), "\\d+"),
    replacement = glue::glue("{from_part}")
  ) |>
  lubridate::dmy(quiet = TRUE)
}

#' Parse the fieldwork start date from Wikipedia date strings
#'
#' Vectorised parser for the "Date" column of a Wikipedia opinion-polling
#' table. Returns the date fieldwork *began*. Handles three shapes of input:
#'
#' * a range (`"2-7 Mar 2022"`, `"28 Feb-3 Mar 2022"`), delegated to
#'   [extract_date_from()];
#' * a single day (`"14 Mar 2022"`), parsed with [lubridate::dmy()];
#' * a month only (`"Mar 2022"`), parsed with [lubridate::my()] as the 1st of
#'   that month.
#'
#' Before parsing, footnote markers (`[1]`) are removed and en/em dashes are
#' normalised to a plain hyphen.
#'
#' Two known-bad strings on the Wikipedia pages are hard-coded:
#' `"2-7, 14-15 Mar 2022"` (non-contiguous fieldwork; the earlier start,
#' 15 Feb 2022, is used) and `"31 Sep - 11 Oct 2015"` (impossible date; 30 Sep
#' is assumed). New malformed strings should be added to the `case_when()`
#' here and in [wikipedia_date_parser_to()].
#'
#' @param date_text Character vector of raw date strings.
#'
#' @return A [Date] vector the same length as `date_text`; `NA` where a string
#'   cannot be parsed.
#'
#' @seealso [wikipedia_date_parser_to()] for the matching end date.
#' @noRd
wikipedia_date_parser_from <- function(date_text) {
  date_text |>
  stringr::str_remove_all("\\[.*\\]") |>
  stringr::str_replace_all("\u2013|\u2014", "-") |>          # normalise en/em dash to hyphen first
  purrr::map(~ dplyr::case_when(
    . == "2-7, 14-15 Mar 2022"  ~ lubridate::ymd("2022-02-15"),  # non-contiguous range; take earlier start
    . == "31 Sep - 11 Oct 2015" ~ lubridate::ymd("2015-09-30"),  # impossible date; assume 30 Sep
    grepl("-", .)               ~ str_split_1(., "-") |>
    str_trim() |>
    extract_date_from(),
    TRUE ~ dplyr::coalesce(dmy(., quiet = TRUE), my(., quiet = TRUE))
  )) |>
  purrr::reduce(base::c)
}

#' Parse the fieldwork end date from Wikipedia date strings
#'
#' Companion to [wikipedia_date_parser_from()]; returns the date fieldwork
#' *finished*. For a range this is the text after the hyphen (always a
#' complete date on Wikipedia). For a single day it is that day. For a
#' month-only string (`"Mar 2022"`) it is the **last** day of the month, so the
#' median date of a month-only poll falls mid-month.
#'
#' Footnotes and dash variants are cleaned exactly as in the "from" parser, and
#' the same two malformed strings are special-cased (the non-contiguous
#' `"2-7, 14-15 Mar 2022"` ends 15 Mar 2022; `"31 Sep - 11 Oct 2015"` ends
#' 11 Oct 2015).
#'
#' @param date_text Character vector of raw date strings.
#'
#' @return A [Date] vector the same length as `date_text`; `NA` where a string
#'   cannot be parsed.
#'
#' @seealso [wikipedia_date_parser_from()]
#' @noRd
wikipedia_date_parser_to <- function(date_text) {
  date_text |>
  stringr::str_remove_all("\\[.*\\]") |>
  stringr::str_replace_all("\u2013|\u2014", "-") |>
  purrr::map(~ case_when(
    . == "2-7, 14-15 Mar 2022"  ~ lubridate::ymd("2022-03-15"),  # take later end date
    . == "31 Sep - 11 Oct 2015" ~ lubridate::ymd("2015-10-11"),
    grepl("-", .)               ~ str_split_1(., "-") |>
    str_trim() |>
    last() |>
    dmy(quiet = TRUE),
    !is.na(my(., quiet = TRUE)) ~ my(., quiet = TRUE) + months(1) - days(1),
    TRUE ~ dmy(., quiet = TRUE)
  )) |>
  purrr::reduce(base::c)
}

#' Parse a numeric cell from a Wikipedia poll table
#'
#' Converts the text of a table cell such as `"31.2%"`, `"1,003"` or
#' `"28[4]"` to a number. Whitespace is squished and any residual footnote
#' markers are removed. Placeholder values that Wikipedia uses for "no data"
#' (en dash, em dash, bare hyphen, `""`, `"N/A"`, `"n/a"`) become `NA` rather
#' than raising a parsing warning. The bare hyphen case matters for the
#' sample-size column of election-result rows.
#'
#' @param x Character vector of cell contents.
#'
#' @return Numeric vector the same length as `x`, with `NA_real_` for
#'   placeholders. Percent signs and thousands separators are dropped by
#'   [readr::parse_number()], so `"31.2%"` returns `31.2` (on the 0-100 scale,
#'   not a proportion).
#' @noRd
parse_wiki_number <- function(x) {
  x |>
  stringr::str_squish() |>
  stringr::str_remove_all("\\[[^\\]]*\\]") |>       # strip any residual footnotes
  dplyr::na_if("\u2013") |>                        # en dash
  dplyr::na_if("\u2014") |>                        # em dash  
  dplyr::na_if("-") |>                             # bare hyphen (e.g. sample size for election results)
  dplyr::na_if("") |>
  dplyr::na_if("N/A") |>
  dplyr::na_if("n/a") |>
  readr::parse_number()   # handles "31.2%", "1,003", etc.
}

#' Normalise a polling-organisation string
#'
#' Prepares the "Polling organisation" cell so it can be matched against the
#' keys of `POLLSTER_LOOKUP` (defined in `.Rprofile`). It squishes whitespace,
#' strips footnote markers, removes trailing Wayback Machine text ("Archived
#' ..."), converts en/em dashes to a hyphen, and removes spaces around hyphens
#' (so `"1 News – Verian"` becomes `"1 News-Verian"`).
#'
#' @param x Character vector of raw polling-organisation strings.
#'
#' @return Character vector of cleaned strings, same length as `x`.
#' @seealso `POLLSTER_LOOKUP` in `.Rprofile`; used by [get_polls()].
#' @noRd
clean_org <- function(x) {
  x |>
  stringr::str_squish() |>
  stringr::str_remove_all("\\[[^\\]]*\\]") |>
  stringr::str_remove(regex("\\s*Archived.*$", ignore_case = TRUE)) |>  # strip Wayback Machine references
  stringr::str_replace_all("\u2013|\u2014", "-") |>
  stringr::str_replace_all("\\s*-\\s*", "-")
}

#' Build the Wikipedia opinion-polling URL for an election year
#'
#' @param year Election year, numeric or character (e.g. `2026`).
#'
#' @return A [glue::glue()] string, e.g.
#'   `"https://en.wikipedia.org/wiki/Opinion_polling_for_the_2026_New_Zealand_general_election"`.
#' @noRd
wikipedia_poll_url <- function(year) {
  glue::glue(
    "https://en.wikipedia.org/wiki/Opinion_polling_for_the_{year}_New_Zealand_general_election"
  )
}

#' Scrape and tidy the Wikipedia opinion-polling table for one election
#'
#' Downloads the Wikipedia page "Opinion polling for the `year` New Zealand
#' general election", takes the first table whose headers mention "poll", and
#' converts it to the tidy wide schema used by the rest of the pipeline.
#'
#' Steps:
#' 1. Clean column names (strip footnotes, [janitor::make_clean_names()]) and
#'    apply `COLUMN_ALIASES` so party columns have canonical codes
#'    (`nat`, `lab`, `grn`, `act`, `nzf`, `tpm`, `top`). The Opportunities
#'    column (`opp`) is renamed `top`.
#' 2. Keep only data rows (those whose `nat` cell is purely numeric), which
#'    drops sub-headers and notes rows.
#' 3. Parse fieldwork dates with [wikipedia_date_parser_from()] and
#'    [wikipedia_date_parser_to()], and set `median_date` to the midpoint.
#' 4. Parse all other columns to numbers with [parse_wiki_number()].
#' 5. Drop parties not in `NAMED_PARTIES` and replace them with a single
#'    residual `other = 100 - sum(named parties)`, floored at 0 to absorb
#'    rounding.
#' 6. Split the polling organisation into `client` (who commissioned it) and
#'    `pollster` (who ran it) via `POLLSTER_LOOKUP`; unmatched organisations
#'    fall back to the cleaned name for both.
#' 7. Compute `gov_bloc = nat + act + nzf` and
#'    `opp_bloc = lab + grn + tpm + top`.
#'
#' Relies on the constants `COLUMN_ALIASES`, `NON_NUMERIC_COLS`,
#' `NAMED_PARTIES` and `POLLSTER_LOOKUP`, all defined in `.Rprofile`. Needs an
#' internet connection.
#'
#' @param year Election year to scrape, e.g. `2026`.
#'
#' @return A tibble with one row per poll and columns `start_date`,
#'   `end_date`, `median_date`, `client`, `pollster`, `sample_size`, `lab`,
#'   `nat`, `grn`, `act`, `tpm`, `nzf`, `other`, `top`, `gov_bloc`,
#'   `opp_bloc`. Party columns are percentages (0-100). Rows with no parseable
#'   `end_date` are dropped. Election-result rows appear with `pollster ==
#'   "General election"`.
#'
#' @section Known issue:
#' The body of this function currently begins with a leftover test line,
#' `year <- 2026`, which overwrites the `year` argument. Remove it before
#' scraping any other year.
#'
#' @seealso [make_combined_polls()], which calls this and merges the result
#'   with the historic dataset.
#' @noRd
get_polls <- function(year) {

  # --------------------------------------------------
  # Fetch raw data from Wikipedia
  # --------------------------------------------------
  raw <- rvest::read_html(wikipedia_poll_url(year)) |>
  rvest::html_table() |>
  purrr::keep(~ any(grepl("Poll|poll", names(.)))) |>
  dplyr::first() |>
  dplyr::rename_with(~ stringr::str_remove_all(.x, "\\[[^\\]]*\\]")) |>
  dplyr::rename_with(janitor::make_clean_names) |>
  dplyr::rename_with(~ dplyr::coalesce(COLUMN_ALIASES[.x], .x)) |> 
  dplyr::rename(top = opp)
  
  
  # --------------------------------------------------
  # Filter to data rows only (nat column starts with a digit)
  # --------------------------------------------------
  raw <- raw |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_remove_all(.x, "\\[[^\\]]*\\]"))) |>
  dplyr::filter(stringr::str_detect(nat, "^[0-9.]+$")) |> 
  dplyr::select(-dplyr::any_of("lead"))
  
  # --------------------------------------------------
  # Parse dates
  # --------------------------------------------------
  polls <- raw |>
  dplyr::mutate(
    start_date  = wikipedia_date_parser_from(date),
    end_date    = wikipedia_date_parser_to(date),
    median_date = start_date + (end_date - start_date) / 2
  )
  
  # Update non-umeric columns
  NON_NUMERIC_COLS <- c(NON_NUMERIC_COLS, "median_date", "start_date", "end_date")
  
  # --------------------------------------------------
  # Parse numeric columns (every column except the known text ones)
  # --------------------------------------------------
  
  numeric_cols <- setdiff(names(polls), NON_NUMERIC_COLS)
  polls <- polls |> dplyr::mutate(across(all_of(numeric_cols), parse_wiki_number))
  
  # --------------------------------------------------
  # other: residual imputation (absorbs all non-NAMED_PARTIES)
  # --------------------------------------------------
  infra_cols    <- c(NON_NUMERIC_COLS, "sample_size", "lead")
  minor_parties <- setdiff(numeric_cols, c(NAMED_PARTIES, infra_cols))
  
  polls <- polls |>
  dplyr::select(-any_of(minor_parties)) |>  # drop NCP, others, etc. — subsumed by residual
  dplyr::mutate(
    other = 100 - rowSums(across(all_of(NAMED_PARTIES)), na.rm = TRUE),
    other = pmax(other, 0)                          # guard against small negative values from rounding
  )
  
  # Parse and split polling organisation -> client + pollster
  polls <- polls |>
  dplyr::mutate(org_clean = clean_org(polling_organisation)) |>
  dplyr::left_join(POLLSTER_LOOKUP, by = c("org_clean" = "org")) |>
  dplyr::mutate(
    client   = dplyr::coalesce(client, org_clean),
    pollster = dplyr::coalesce(pollster, org_clean)
  )
  
  # --------------------------------------------------
  # Bloc totals
  # --------------------------------------------------

  # Final schema matching DATA EXT
  polls |>
  dplyr::mutate(
    opp_bloc = rowSums(across(all_of(c("lab", "grn", "tpm", "top"))), na.rm = TRUE),
    gov_bloc = nat + act + nzf
  ) |> 
  dplyr::select(
    start_date, end_date, median_date,
    client, pollster,
    sample_size,
    lab, nat, grn, act, tpm, nzf, other, 
    top,
    gov_bloc, opp_bloc
  ) |>
  dplyr::filter(!is.na(end_date))
  
}

#' Combine historic and newly scraped polls into the modelling dataset
#'
#' Builds the dataset consumed by the national model. It merges the manually
#' curated historic polls with the latest Wikipedia scrape and prepares the
#' result for modelling.
#'
#' Steps:
#' 1. Read `historic_path` (an `.rds` of curated polls) and coerce the date
#'    columns to [Date].
#' 2. Call [get_polls()] for `year`. If the scrape fails, a warning is issued
#'    and the historic data is returned as-is (see *Fallback* below).
#' 3. Split the scrape into rows already in the historic data and genuinely new
#'    rows, matching on `DEDUP_COLS` (`end_date` + `pollster`), and report the
#'    counts via `message()`.
#' 4. Bind historic rows first and de-duplicate on `DEDUP_COLS`, so curated
#'    historic rows win any conflict. Sort by `median_date`, `pollster`.
#' 5. Assign each poll to an `election` cycle (2014, 2017, 2020, 2023, 2026)
#'    by `median_date`. The cut-offs are the general-election dates, hard-coded
#'    in the function. They will need updating after the 2026 election.
#' 6. Clean and transform:
#'    * drop polls where any named party other than `top` is missing or zero
#'      (these are usually polls that did not report minor parties);
#'    * `days_diff` = `median_date - today` in days (zero today, negative in
#'      the past), which is the time axis of the model;
#'    * `sample_size` is made numeric and missing values are filled with the
#'      smallest observed sample size;
#'    * `precision = log(sample_size)`, used to model the Dirichlet precision;
#'    * `client`, `pollster` and `election` become factors;
#'    * `other` is floored at 0.0001 and each row of party shares is rescaled to
#'      sum to 1, which also converts percentages to proportions.
#'
#' @param year Election year to scrape, passed to [get_polls()].
#' @param historic_path Path to the curated historic polls `.rds`
#'   (`data/polls_historic.rds`).
#' @param today A [Date], used to compute `days_diff`. Because the pipeline
#'   passes `Sys.Date()`, `days_diff` is re-anchored on every run.
#'
#' @return A tibble with one row per poll: the columns of [get_polls()] plus
#'   `election`, `days_diff`, `precision`, with the party columns (`act`,
#'   `grn`, `lab`, `nat`, `nzf`, `tpm`, `top`, `other`) as proportions summing
#'   to 1. Note that this is the data itself, not a file path.
#'
#' @section Fallback:
#' If the Wikipedia scrape fails, the function returns the historic data
#' *before* steps 5-6, so the result will lack `election`, `days_diff` and
#' `precision`, and downstream targets will fail. Fix the scrape or restore
#' connectivity rather than relying on this path.
#'
#' @section Requires:
#' `DEDUP_COLS` and `NAMED_PARTIES` from `.Rprofile`.
#' @noRd
make_combined_polls <- function(year, historic_path, today) {
  
  ## Test
  # year <- tar_read(year)
  # historic_path <- tar_read(historic_path)
  # today <- tar_read(today)
  ##

  # --------------------------------------------------
  # 1. Load historic data
  # --------------------------------------------------
  polls_historic <- readRDS(historic_path) |>
  dplyr::mutate(across(c(start_date, end_date, median_date), as.Date))
  
  # --------------------------------------------------
  # 2. Fetch latest Wikipedia data
  # --------------------------------------------------
  polls_new <- tryCatch(
    get_polls(year),
    error = function(e) {
      warning(glue::glue("get_polls() failed: {e$message}. Using historic data only."))
      NULL
    }
  ) 
  
  if (is.null(polls_new)) {
    return(polls_historic)
    stop("Pipeline halted: could not fetch new polls.")
  }
  
  # --------------------------------------------------
  # 3. Identify genuinely new rows before binding
  # --------------------------------------------------
  already_seen <- polls_new |>
  dplyr::semi_join(polls_historic, by = DEDUP_COLS)
  
  truly_new <- polls_new |>
  dplyr::anti_join(polls_historic, by = DEDUP_COLS)
  
  message(glue(
    "Wikipedia: {nrow(polls_new)} rows fetched | ",
    "{nrow(truly_new)} new | ",
    "{nrow(already_seen)} already in historic cache"
  ))
  
  if (nrow(truly_new) == 0) {
    message("No new polls detected. Combined dataset unchanged.")
  }
  
  # --------------------------------------------------
  # 4. Bind — historic first so distinct() keeps curated rows on conflict
  # --------------------------------------------------
  polls_combined <- bind_rows(polls_historic, truly_new) |>
  dplyr::arrange(median_date, pollster) |>
  dplyr::distinct(across(all_of(DEDUP_COLS)), .keep_all = TRUE)
  
  # --------------------------------------------------
  # 5. Assign election cycle
  # --------------------------------------------------
  
  polls_combined <- polls_combined |>
  dplyr::mutate(election = case_when(
    median_date <= "2014-09-20" ~ "2014",
    median_date <= "2017-09-23" ~ "2017",
    median_date <= "2020-10-17" ~ "2020",
    median_date <= "2023-10-14" ~ "2023",
    .default = "2026"
  ))
  
  # --------------------------------------------------
  # Polls combined cleaning and manipulation
  # --------------------------------------------------
  
  polls_combined <- polls_combined |> 
  filter(
    # Drop the strange poll with no minor parties
    if_all(all_of(setdiff(NAMED_PARTIES, "top")), ~ !is.na(.x)),
    # Drop minor parties with 0
    if_all(all_of(setdiff(NAMED_PARTIES, "top")), ~ .x > 0)
  ) |>
  # Infra columns: compute before pivoting while still in wide format
  dplyr::mutate(
    days_diff   = as.numeric(median_date - today),
    sample_size = as.numeric(sample_size),
    sample_size = coalesce(sample_size, min(sample_size, na.rm = TRUE)),
    # precision = 1 / sqrt(sample_size),
    precision = log(sample_size),
    across(c(client, pollster, election), as.factor)
  ) |>
  # Add epsilon to other categories that are 0 and rescale all other party proportions
  # NB: this also takes care of the percentage to proportion conversion
  dplyr::mutate(
    other = pmax(other, 0.0001),
    scale = rowSums(across(all_of(c(NAMED_PARTIES, "other"))), na.rm = TRUE),
    across(all_of(c(NAMED_PARTIES, "other")), ~ .x / scale)
  ) |>
  # Pivot to long — date/infra columns are preserved automatically as id cols
  tidyr::pivot_longer(
    cols      = all_of(c(NAMED_PARTIES, "other")),
    names_to  = "party",
    values_to = "support"
  ) |>
  # Support-derived columns
  dplyr::mutate(
    party      = as.factor(party)
  ) |> 
  tidyr::pivot_wider(names_from = party, values_from = support) |>
  dplyr::select(-scale)
  
  # --------------------------------------------------
  # 6. Save
  # --------------------------------------------------
  polls_combined
  
}
