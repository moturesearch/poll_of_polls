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