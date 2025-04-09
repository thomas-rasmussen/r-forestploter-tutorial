# Make data for examples and exercises

library(dplyr)

path_data <- "S:/Stat day May 2025/forest_plots/data/"

#### Parameter estimates from regression models ####

set.seed(2)

dat <- tribble(
  ~analysis, ~pt_n_total,
  "Overall", 10000,
  "Sensitivity analysis", 9000,
  "Subgroup analysis", NA,
  "\u00A0\u00A0\u00A0\u00A0Subgroup 1", 5000,
  "\u00A0\u00A0\u00A0\u00A0Subgroup 2", 5000
)

n_rows <- nrow(dat)

dat <- dat %>%
  mutate(
    pt_n_0 = round(pt_n_total * runif(n_rows, 0.3, 0.7)),
    pt_n_1 = pt_n_total - pt_n_0,
    deaths_n_0 = round(runif(n_rows, 0.05, 0.15) * pt_n_0),
    deaths_n_1 = round(runif(n_rows, 0.15, 0.25) * pt_n_1),
    deaths_pct_0 = 100 * deaths_n_0 / pt_n_0,
    deaths_pct_1 = 100 * deaths_n_1 / pt_n_1,
    deaths_n_pct_0 = paste0(
      formatC(deaths_n_0, digits = 0, format = "d", big.mark = ","),
      " (",
      formatC(deaths_pct_0, digits = 1, format = "f"),
      "%)"
    ),
    deaths_n_pct_1 = paste0(
      formatC(deaths_n_1, digits = 0, format = "d", big.mark = ","),
      " (",
      formatC(deaths_pct_1, digits = 1, format = "f"),
      "%)"
    ),
    hr_est = deaths_n_1 / deaths_n_0 * runif(n_rows, 1.2, 1.5),
    tmp = runif(n_rows, 0.3, 0.6),
    hr_lcl = exp(log(hr_est) - tmp),
    hr_ucl = exp(log(hr_est) + tmp),
    hr_ci = paste0(
      formatC(hr_est, digits = 2, format = "f"),
      " (",
      formatC(hr_lcl, digits = 2, format = "f"),
      "-",
      formatC(hr_ucl, digits = 2, format = "f"),
      ")"
    ),
    deaths_n_pct_0 = ifelse(analysis == "Subgroup analysis", "", deaths_n_pct_0),
    deaths_n_pct_1 = ifelse(analysis == "Subgroup analysis", "", deaths_n_pct_1),
    hr_ci = ifelse(analysis == "Subgroup analysis", "", hr_ci)
  ) %>%
  select(
    analysis, deaths_n_pct_0, deaths_n_pct_1,
    hr_ci, hr_est, hr_lcl, hr_ucl
  )

saveRDS(dat, paste0(path_data, "forest_plot_regression.rds"))



#### Multiple CI's ####

set.seed(3)

dat <- tribble(
  ~population,    ~or_est_1, ~or_est_2,
  "Population 1", 1.50,      2.50,
  "Population 2", 1.30,      2.00,
  "Population 3", 1.00,      3.50,
  "Population 4", 2.00,      2.00
)

n_obs <- 4L
dat <- dat %>%
  mutate(
    tmp1 = runif(n_obs),
    tmp2 = runif(n_obs),
    or_lcl_1 = round(exp(log(or_est_1) - tmp1), digits = 2),
    or_ucl_1 = round(exp(log(or_est_1) + tmp1), digits = 2),
    or_lcl_2 = round(exp(log(or_est_2) - tmp2), digits = 2),
    or_ucl_2 = round(exp(log(or_est_2) + tmp2), digits = 2),
    or_ci_1 = paste0(
      formatC(or_est_1, digits = 2, format = "f"),
      " (",
      formatC(or_lcl_1, digits = 2, format = "f"),
      "-",
      formatC(or_ucl_1, digits = 2, format = "f"),
      ")"
    ),
    or_ci_2 = paste0(
      formatC(or_est_2, digits = 2, format = "f"),
      " (",
      formatC(or_lcl_2, digits = 2, format = "f"),
      "-",
      formatC(or_ucl_2, digits = 2, format = "f"),
      ")"
    ),
  ) %>%
  select(-tmp1, -tmp2) %>%
  relocate(
    population, or_ci_1, or_ci_2,
    or_est_1, or_lcl_1, or_ucl_1, or_est_2, or_lcl_2, or_ucl_2
  )

saveRDS(dat, paste0(path_data, "forest_plot_multiple_ci.rds"))


#### Meta-analysis ####

set.seed(2)
n_study <- 10L
dat <- tibble(
    study = paste0("Study ", 1:n_study),
    n_events_treatment = round(runif(n_study, 100, 10000)),
    n_events_placebo = round(n_events_treatment*runif(n_study, 0.2, 0.8)),
    rr_estimate = n_events_treatment / n_events_placebo + runif(n_study, -0.5, 0.5),
    rr_se = 1000/(n_events_treatment + n_events_placebo),
    # below correct, but doesn't matter, we just need something that is
    # symmetric around the point estimate on a log scale.
    rr_lcl = exp(log(rr_estimate) - 1.96*rr_se),
    rr_ucl = exp(log(rr_estimate) + 1.96*rr_se),
    weight = sqrt(1/rr_se**2)/max(sqrt(1/rr_se**2))
  )

combined_estimate <- tibble(
  study = "Random effects model    ",
  rr_estimate = mean(dat$rr_estimate),
  rr_se = mean(dat$rr_se) / 2,
  rr_lcl = rr_estimate - 1.96*rr_se,
  rr_ucl = rr_estimate + 1.96*rr_se,
  weight = 1
  )

dat <- bind_rows(dat, combined_estimate)


dat <- dat %>%
  mutate(
    n_events_treatment = formatC(n_events_treatment, digits = 0, format = "d", big.mark = ","),
    n_events_placebo = formatC(n_events_placebo, digits = 0, format = "d", big.mark = ","),
    rr_ci = paste0(
      formatC(rr_estimate, digits = 2, format = "f"),
      " (",
      formatC(rr_lcl, digits = 2, format = "f"),
      "-",
      formatC(rr_ucl, digits = 2, format = "f"),
      ")"
    ),
    n_events_treatment = ifelse(study == "Random effects model    ", "", n_events_treatment),
    n_events_placebo = ifelse(study == "Random effects model    ", "", n_events_placebo),
  ) %>%
  relocate(study, n_events_treatment, n_events_placebo, rr_ci)


saveRDS(dat, file = paste0(path_data, "forest_plot_meta_analysis.rds"))


