org::initialize_project(
  env = .GlobalEnv,
  home = c(
    "~/papadopoulos-lab/2025-white-random-intercepts/"
  ),
  results = c(
    "~/papadopoulos-lab/2025-white-random-intercepts/results/"
  )
)

# load every single function (commands) into what is currently available in these named libraries
library(data.table)
library(ggplot2)
library(magrittr)

set.seed(123)

# Parameters
n_clusters <- 10
cluster_size <- 20
n <- n_clusters * cluster_size

# Cluster IDs
cluster_id <- rep(1:n_clusters, each = cluster_size)

raw <- vector("list", length = 20)
for(i in seq_along(raw)){
  cat(i, "\n")
  # Strong cluster-level confounder
  U_cluster <- rnorm(n_clusters, mean = 0, sd = 1)
  U <- U_cluster[cluster_id]

  # Simulate exposure with strong effect from U
  # Weak individual-level noise
  X <- 0.8 * U + rnorm(n, mean = 0, sd = 0.5)

  # Simulate outcome with strong U effect and weak X effect
  Y <- 1.5 * U + 0.2 * X + rnorm(n, mean = 0, sd = 1)

  # Put in a data frame
  dat <- data.frame(cluster_id, U, X, Y)

  # Check associations
  summary(lm(Y ~ X))                  # Confounded effect

  fit_lm <- coef(lm(Y ~ X + factor(cluster_id)))[2]  # Adjusted for clusters
  fit_lmer <- coef(lme4::lmer(Y ~ X + (1 | cluster_id), data = dat))$cluster_id[1,2]  # Random intercepts
  fit <- rstanarm::stan_lm(Y ~ X + as.factor(cluster_id), data = dat, prior = rstanarm::R2(0.75), refresh = 0)  # Random intercepts
  fit_stan_lm <- coef(fit)[2]
  fit <- rstanarm::stan_glmer(Y ~ X + (1 | cluster_id), data = dat, family = gaussian(link = "identity"), refresh = 0)  # Random intercepts
  fit_stan_lmer <- coef(fit)$cluster_id[1,2]

  raw[[i]] <- data.frame(
    real_value = 0.2,
    lm = fit_lm,
    lmer = fit_lmer,
    stan_lm = fit_stan_lm,
    stan_lmer = fit_stan_lmer
  )
}

d <- rbindlist(raw)
d
d[, id := 1:.N]
d <- melt.data.table(d, id.vars = c("id", "real_value"))
d[, deviance := value - real_value]
d[,.(mean(deviance)), by = .(variable)]



