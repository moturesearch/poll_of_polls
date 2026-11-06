base <- "https://github.com/moturesearch/poll_of_polls/releases/download/models-v1/"
files <- c("electorate_model_stage1.rds", "electorate_model_stage2.rds")
options(timeout = 1800)
for (f in files) {
  dest <- file.path("electorate_model", f)
  if (!file.exists(dest)) download.file(paste0(base, f), dest, mode = "wb")
}
