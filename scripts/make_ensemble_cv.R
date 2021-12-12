outcomes <- c("pkrate", "pkweek", "cumhosp")

make_batch_job <- function(outcome,
                           holdout_template = 1:15,
                           base_file = "04_run_superlearner.R",
                           cv_file = "10_ensemble-cv.R",
                           cores = 32) {

  paste0(
    "sed -e ", "'s/", base_file, "/", cv_file,
    "/' RUN_TARGET-", outcome,
    "_LAMBDA-lambda-min.sh | sbatch --export=ALL,HOLDOUT_TEMPLATE=", holdout_template
  )
}

batchlines <- unlist(lapply(outcomes, make_batch_job))

fn = "scripts/RUN_ENSEMBLE_CV.sh"
file.remove(fn)

sapply(
  batchlines,
  function(.x) {
    sink(file = fn, append = TRUE)
    cat(.x, "\n")
    sink()
  }
)
