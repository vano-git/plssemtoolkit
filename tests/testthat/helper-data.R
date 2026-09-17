# Generates small synthetic survey-like data for tests, so the test suite
# never depends on any real/private dataset.
make_synthetic_data <- function(n = 150, seed = 42) {
  set.seed(seed)
  psych <- stats::rnorm(n, 4, 1)
  social <- stats::rnorm(n, 4, 1)
  env <- stats::rnorm(n, 4, 1)
  outcome_latent <- 0.3 * psych + 0.3 * social + 0.2 * env + stats::rnorm(n, 0, 1)

  jitter_item <- function(latent, sd = 0.6) {
    x <- latent + stats::rnorm(length(latent), 0, sd)
    pmax(1, pmin(7, round(x)))
  }

  data.frame(
    PS_1 = jitter_item(psych), PS_2 = jitter_item(psych), PS_3 = jitter_item(psych),
    SO_1 = jitter_item(social), SO_2 = jitter_item(social),
    SO_3 = jitter_item(social), SO_4 = jitter_item(social),
    EN_1 = jitter_item(env), EN_2 = jitter_item(env), EN_3 = jitter_item(env),
    GV_1 = jitter_item(outcome_latent), GV_2 = jitter_item(outcome_latent),
    GV_3 = jitter_item(outcome_latent), GV_4 = jitter_item(outcome_latent),
    GV_5 = jitter_item(outcome_latent)
  )
}

make_test_spec <- function() {
  load_model_spec(list(
    constructs = list(
      "Psychological Factors" = list(type = "reflective", items = c("PS_1", "PS_2", "PS_3")),
      "Social Factors" = list(type = "reflective", items = c("SO_1", "SO_2", "SO_3", "SO_4")),
      "Environmental Factors" = list(type = "reflective", items = c("EN_1", "EN_2", "EN_3")),
      "Good Shopping Vibes" = list(type = "reflective", items = c("GV_1", "GV_2", "GV_3", "GV_4", "GV_5"))
    ),
    paths = list(
      list(from = list("Psychological Factors", "Social Factors", "Environmental Factors"),
           to = list("Good Shopping Vibes"))
    )
  ))
}
