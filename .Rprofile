options(tidyverse.quiet = TRUE)
source("renv/activate.R")
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/data", showWarnings = FALSE)
dir.create("outputs/models", showWarnings = FALSE)
dir.create("outputs/summaries", showWarnings = FALSE)


DEDUP_COLS <- c("end_date", "pollster") # Deduplication key <e2><80><94> identifies a unique poll observation from pollster and end_date
NON_NUMERIC_COLS <- c("date", "polling_organisation") # Only these columns are genuinely non-numeric in a Wikipedia polling table
NAMED_PARTIES <- c("nat", "lab", "grn", "act", "nzf", "tpm", "top") # The seven named parties we model explicitly; everything else is OTH

# Rename cols in historic (wikipedia) polling tables to consistent values
HISTORIC_RENAMES <- c(
  sample_size  = "sample",
  lab          = "labour",
  nat          = "national",
  grn          = "greens",
  tpm          = "te_pati_maori",
  nzf          = "nz_first",
  lab_bloc     = "labour_bloc",
  nat_bloc     = "national_bloc"
)

# Pollster lookup: Wikipedia "polling_organisation" -> client + pollster
POLLSTER_LOOKUP <- tibble::tribble(
  ~org,                          ~client,              ~pollster,
  "1 News-Verian",               "1 News",             "Verian",
  "1 News-Kantar Public",        "1 News",             "Verian",
  "One News-Colmar Brunton",     "1 News",             "Verian",
  "1 News-Colmar Brunton",       "1 News",             "Verian",
  "Stuff-YouGov",                "Stuff",              "YouGov",
  "Business NZ-Reid Research",   "Business NZ",        "Reid_Research",
  "Taxpayers' Union-Curia",      "Taxpayers' Union",   "Curia",
  "Talbot Mills",                "Talbot Mills",       "Anacta",
  "Labour-Talbot Mills",         "Labour",             "Anacta",
  "Anacta",                      "Anacta",             "Anacta",
  "Newshub-Reid Research",       "Newshub",            "Reid Research",
  "Newshub Reid Research",       "Newshub",            "Reid Research",
  "3 News-Reid Research",        "3 News",             "Reid Research",
  "Listener: Bauer Media Insights", "Listener",        "Bauer Media Insights",
  "Newsroom-SSI",                "Newsroom",           "SSI",
  "Roy Morgan",                  "Roy Morgan",         "Roy Morgan",
  "Roy Morgan Research",         "Roy Morgan",         "Roy Morgan",
  "Herald-DigiPoll",             "Herald",             "DigiPoll",
  "Essential",                   "Essential",          "Essential",
  "Horizon Research",            "Horizon Research",   "Horizon Research",
  "The Post-Freshwater Strategy","The Post",           "Freshwater Strategy",
  "The Post/Freshwater Strategy","The Post",           "Freshwater Strategy",
  "Radio NZ - Reid",             "Radio NZ",           "Reid Research",
  "RNZ-Reid Research",           "Radio NZ",           "Reid Research",
  "2014 election result",         "General election",   "General election",
  "2017 election result",         "General election",   "General election",
  "2020 election result",         "General election",   "General election",
  "2023 election result",         "General election",   "General election"
)

# --------------------------------------------------
# Column aliases
# Canonical form is the value, all known aliases are keys.
# Anything not listed passes through unchanged.
# --------------------------------------------------
COLUMN_ALIASES <- c(
  # Polling organisation
  "poll"                = "polling_organisation",    
  # Named parties
  "national"            = "nat",
  "labour"              = "lab",
  "green"               = "grn",
  "nz_first"            = "nzf",
  "mri"                 = "tpm",
  "maori"               = "tpm"
)

# BRMS Modelling constants
K_SET <- 25 # Squiggliness parameter for GAM

# Other settings
TOP_EXISTS_FROM <- as.Date("2016-11-04")

N_NATIONAL_DRAWS <- 400
N_CANDIDATE_DRAWS <- 400