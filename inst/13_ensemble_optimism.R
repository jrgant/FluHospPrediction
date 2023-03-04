################################################################################
## SETUP ##
################################################################################

# NOTE Output created within this script is not presented in the paper.

pacman::p_load(data.table, ggplot2, ggthemes, magrittr)


################################################################################
## IN-SAMPLE VERSUS CROSS-VALIDATED ENSEMBLE RISKS ##
################################################################################

ef <- list.files(here::here("results"), pattern = "^Ensemble", full.names = T)

ens <- lapply(ef, fread)

ensw <- rbindlist(
  lapply(ens, function(x) {
    tmp <- dcast(
      x[, .(analysis, target, Week, learner, mean_risk)],
      analysis + target + Week ~ learner,
      value.var = "mean_risk"
    )
    tmp[analysis == "LambdaMin", analysis := "Main Analysis"]
    tmp[analysis == "LambdaSE", analysis := "Lambda 1SE"]
    tmp[analysis == "ElastNetRF", analysis := "Elastic Net, Random Forest"]
    tmp[analysis == "SqErrLoss", analysis := "Squared Error Loss"]
    tmp
  })
)

anlabs <- ensw[, unique(analysis)]

plots <- lapply(setNames(anlabs, anlabs), function(x) {
  tmp <- ensw[analysis == x]
  tmp[, target := factor(
          target,
          levels = c("PeakRate", "PeakWeek", "CumHosp")
        )][] %>%
    ggplot(aes(x = SuperLearner, y = SuperLearnerCV)) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_text(aes(label = Week), size = 12) +
    facet_wrap(~ target, scales = "free") +
    labs(
      x = "\nIn-sample Risk",
      y = "Cross-validated Risk\n",
      caption = "Risks expressed on the scale of each prediction target."
    ) +
    ggtitle(paste("Ensemble Prediction Risk,", x)) +
    theme_base(base_size = 30, base_family = "sans") +
    theme(
      plot.caption = element_text(hjust = 0),
      strip.text = element_text(size = 30),
      plot.title = element_text(
        hjust = 0.5,
        margin = margin(b = 1, unit = "cm")
      )
    )
})

plots[[1]]
plots[[2]]
plots[[3]]



lapply(anlabs, function(x) {
  if (x %like% "Main") slug <- "main"
  if (x %like% "1SE") slug <- "1se"
  if (x %like% "Elastic") slug <- "elastnetrf"
  if (x %like% "Squared") slug <- "sqerrloss"
  ggsave(
    paste0("ensrisk-cv-insample_", slug, ".png"),
    plots[[x]]
  )
  invisible
})
