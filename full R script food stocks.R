# ================================================================
# Title: A novel search-attention index for Iran-Hormuz energy risk and its connectedness with global food markets
# ================================================================
# (Weekly): Baseline Connectedness Analysis
# IHERI / GPR / OVX / CF -> Food & Agri Market Spillovers
# ================================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

setwd("D:/Food stocks/Study 1")

required_packages <- c("haven", "dplyr", "zoo", "tseries", "FinTS",
                       "moments", "ConnectednessApproach", "writexl")

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

df <- read_dta("D:/Food stocks/Study 1/weekly.dta")

# Confirm daten imported as proper R Date
str(df$daten)
class(df$daten)   # should read "Date"

# Check for duplicate or missing dates before proceeding - critical,
# since a duplicated/missing date will silently corrupt the zoo index
sum(duplicated(df$daten))
sum(is.na(df$daten))

# ------------------------------------------------------------
# 2. Build baseline 9-variable panel
#    Transmitters: OVX, GPRD, iheri_aggregate, CF
#    Receivers: DBA, MOO, VEGI, PBJ, FTXG
# ------------------------------------------------------------

df <- df %>%
  arrange(daten) %>%
  mutate(
    lr_DBA  = c(NA, diff(log(DBA))),
    lr_MOO  = c(NA, diff(log(MOO))),
    lr_VEGI = c(NA, diff(log(VEGI))),
    lr_PBJ  = c(NA, diff(log(PBJ))),
    lr_FTXG = c(NA, diff(log(FTXG))),
    lr_OVX  = c(NA, diff(log(OVX))),
    lr_CF   = c(NA, diff(log(CF)))
  )

# GPRD and iheri_aggregate stay in LEVELS - already confirmed

baseline_vars <- c("lr_OVX", "GPRD", "iheri_aggregate", "lr_CF",
                   "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")

baseline_df <- df %>%
  select(daten, all_of(baseline_vars)) %>%
  filter(complete.cases(.))   # drops the first NA row from differencing

cat("Baseline panel: ", nrow(baseline_df), "observations,",
    min(baseline_df$daten), "to", max(baseline_df$daten), "\n")

# ------------------------------------------------------------
# 3. TABLE 1 - Descriptive statistics & stationarity
# ------------------------------------------------------------

desc_stats <- data.frame(
  Variable = baseline_vars,
  Mean = sapply(baseline_df[baseline_vars], mean),
  SD   = sapply(baseline_df[baseline_vars], sd),
  Min  = sapply(baseline_df[baseline_vars], min),
  Max  = sapply(baseline_df[baseline_vars], max),
  Skewness = sapply(baseline_df[baseline_vars], skewness),
  Kurtosis = sapply(baseline_df[baseline_vars], kurtosis),
  JB_stat  = sapply(baseline_df[baseline_vars], function(x) jarque.bera.test(x)$statistic),
  JB_pval  = sapply(baseline_df[baseline_vars], function(x) jarque.bera.test(x)$p.value),
  ADF_stat = sapply(baseline_df[baseline_vars], function(x) adf.test(x, k = 2)$statistic),
  ADF_pval = sapply(baseline_df[baseline_vars], function(x) adf.test(x, k = 2)$p.value),
  ARCHLM_stat = sapply(baseline_df[baseline_vars], function(x) ArchTest(x, lags = 2)$statistic),
  ARCHLM_pval = sapply(baseline_df[baseline_vars], function(x) ArchTest(x, lags = 2)$p.value)
)

print(desc_stats)
write_xlsx(desc_stats, "Table1_Descriptive_Statistics.xlsx")

# ------------------------------------------------------------
# 4. Prepare zoo object for ConnectednessApproach
#    (requires zoo class specifically, per package documentation)
# ------------------------------------------------------------

connectedness_data <- zoo(baseline_df[baseline_vars], order.by = baseline_df$daten)

# ------------------------------------------------------------
# 5. TABLE 2 - Static (full-sample) connectedness, baseline model
# ------------------------------------------------------------

static_dca <- ConnectednessApproach(
  connectedness_data,
  nlag = 1,
  nfore = 10,
  model = "VAR",
  connectedness = "Time",
  Connectedness_config = list(
    TimeConnectedness = list(generalized = TRUE)
  )
)

static_table_df <- as.data.frame(static_dca$TABLE)
static_table_df <- cbind(Variable = rownames(static_dca$TABLE), static_table_df)
rownames(static_table_df) <- NULL

write_xlsx(static_table_df, "Table2_Static_Connectedness_Baseline.xlsx")
print(static_table_df)

saveRDS(static_dca, "static_dca_baseline.rds")

# ------------------------------------------------------------
# 6. TABLE 3 - Quantile connectedness at tau = 0.05, 0.50, 0.95
# ------------------------------------------------------------

quantiles <- c(0.05, 0.50, 0.95)
qvar_results <- list()

for (q in quantiles) {
  cat("Estimating QVAR at tau =", q, "...\n")
  
  qvar_results[[paste0("tau_", q)]] <- ConnectednessApproach(
    connectedness_data,
    nlag = 1,
    nfore = 10,
    model = "QVAR",
    connectedness = "Time",
    VAR_config = list(QVAR = list(tau = q, method = "fn"))
  )
}

# Extract and combine the three quantile TABLEs into one comparison table
qvar_table_combined <- lapply(names(qvar_results), function(nm) {
  tbl <- as.data.frame(qvar_results[[nm]]$TABLE)
  tbl$Quantile <- nm
  tbl
})
qvar_table_combined <- do.call(rbind, qvar_table_combined)

write_xlsx(qvar_table_combined, "Table3_Quantile_Connectedness_Baseline.xlsx")

saveRDS(qvar_results, "qvar_results_baseline.rds")

cat("\n=== Study 1 baseline analysis complete. Outputs saved to D:/Food stocks/Study 1 ===\n")

# ------------------------------------------------------------
# TABLE 4 - Robustness: add tanker composite (10-variable model)
# ------------------------------------------------------------

df <- df %>%
  arrange(daten) %>%
  mutate(
    lr_FRO  = c(NA, diff(log(FRO))),
    lr_DHT  = c(NA, diff(log(DHT))),
    lr_TNK  = c(NA, diff(log(TNK))),
    lr_NAT  = c(NA, diff(log(NAT))),
    lr_STNG = c(NA, diff(log(STNG))),
    lr_INSW = c(NA, diff(log(INSW))),
    tanker_composite = rowMeans(cbind(lr_FRO, lr_DHT, lr_TNK, lr_NAT, lr_STNG, lr_INSW), na.rm = TRUE)
  )

tanker_vars <- c("lr_OVX", "GPRD", "iheri_aggregate", "lr_CF", "tanker_composite",
                 "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")

tanker_df <- df %>%
  select(daten, all_of(tanker_vars)) %>%
  filter(complete.cases(.))

tanker_data <- zoo(tanker_df[tanker_vars], order.by = tanker_df$daten)

tanker_dca <- ConnectednessApproach(
  tanker_data, nlag = 1, nfore = 10, model = "VAR", connectedness = "Time",
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE))
)

tanker_table_df <- as.data.frame(tanker_dca$TABLE)
tanker_table_df <- cbind(Variable = rownames(tanker_dca$TABLE), tanker_table_df)
rownames(tanker_table_df) <- NULL

write_xlsx(tanker_table_df, "Table4_Robustness_TankerComposite.xlsx")
saveRDS(tanker_dca, "tanker_dca.rds")

# ------------------------------------------------------------
# TABLE 5 - Robustness: exclude IHERI_aggregate (8-variable model)
# ------------------------------------------------------------

noiheri_vars <- c("lr_OVX", "GPRD", "lr_CF", "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")

noiheri_df <- df %>%
  select(daten, all_of(noiheri_vars)) %>%
  filter(complete.cases(.))

noiheri_data <- zoo(noiheri_df[noiheri_vars], order.by = noiheri_df$daten)

noiheri_dca <- ConnectednessApproach(
  noiheri_data, nlag = 1, nfore = 10, model = "VAR", connectedness = "Time",
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE))
)

noiheri_table_df <- as.data.frame(noiheri_dca$TABLE)
noiheri_table_df <- cbind(Variable = rownames(noiheri_dca$TABLE), noiheri_table_df)
rownames(noiheri_table_df) <- NULL

write_xlsx(noiheri_table_df, "Table5_Robustness_ExcludeIHERI.xlsx")
saveRDS(noiheri_dca, "noiheri_dca.rds")

# ------------------------------------------------------------
# TABLE 6 - Robustness: GPR decomposed (Acts vs Threats)
# ------------------------------------------------------------

gprdecomp_vars <- c("lr_OVX", "GPRD_ACT", "GPRD_THREAT", "iheri_aggregate", "lr_CF",
                    "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")

gprdecomp_df <- df %>%
  select(daten, all_of(gprdecomp_vars)) %>%
  filter(complete.cases(.))

gprdecomp_data <- zoo(gprdecomp_df[gprdecomp_vars], order.by = gprdecomp_df$daten)

gprdecomp_dca <- ConnectednessApproach(
  gprdecomp_data, nlag = 1, nfore = 10, model = "VAR", connectedness = "Time",
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE))
)

gprdecomp_table_df <- as.data.frame(gprdecomp_dca$TABLE)
gprdecomp_table_df <- cbind(Variable = rownames(gprdecomp_dca$TABLE), gprdecomp_table_df)
rownames(gprdecomp_table_df) <- NULL

write_xlsx(gprdecomp_table_df, "Table6_Robustness_GPR_Decomposed.xlsx")
saveRDS(gprdecomp_dca, "gprdecomp_dca.rds")

# ------------------------------------------------------------
# TABLE 7 - Robustness: IHERI decomposed (Chokepoint/Market/Transport)
# ------------------------------------------------------------

iheridecomp_vars <- c("lr_OVX", "GPRD", "iheri_c", "iheri_m", "iheri_t", "lr_CF",
                      "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")

iheridecomp_df <- df %>%
  select(daten, all_of(iheridecomp_vars)) %>%
  filter(complete.cases(.))

iheridecomp_data <- zoo(iheridecomp_df[iheridecomp_vars], order.by = iheridecomp_df$daten)

iheridecomp_dca <- ConnectednessApproach(
  iheridecomp_data, nlag = 1, nfore = 10, model = "VAR", connectedness = "Time",
  Connectedness_config = list(TimeConnectedness = list(generalized = TRUE))
)

iheridecomp_table_df <- as.data.frame(iheridecomp_dca$TABLE)
iheridecomp_table_df <- cbind(Variable = rownames(iheridecomp_dca$TABLE), iheridecomp_table_df)
rownames(iheridecomp_table_df) <- NULL

write_xlsx(iheridecomp_table_df, "Table7_Robustness_IHERI_Decomposed.xlsx")
saveRDS(iheridecomp_dca, "iheridecomp_dca.rds")

# ------------------------------------------------------------
# TABLE 8 - Dynamic Total Connectedness Index (TCI), baseline model
# (feeds Figure 2 - saved here as both data and a quick-look plot)
# ------------------------------------------------------------

dynamic_dca <- ConnectednessApproach(
  connectedness_data, nlag = 1, nfore = 10, model = "TVP-VAR", connectedness = "Time",
  VAR_config = list(TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99, prior = "BayesPrior", gamma = 0.01))
)

tci_series <- data.frame(
  date = index(dynamic_dca$TCI),
  TCI = coredata(dynamic_dca$TCI)
)

write_xlsx(tci_series, "Table8_Dynamic_TCI_Baseline.xlsx")
saveRDS(dynamic_dca, "dynamic_dca_baseline.rds")

png("Figure2_Dynamic_TCI.png", width = 2000, height = 1200, res = 200)
plot(tci_series$date, tci_series$TCI, type = "l", col = "navy", lwd = 2,
     xlab = "", ylab = "TCI (%)", main = "Dynamic Total Connectedness Index (Study 1, Weekly)")
dev.off()

# ================================================================
# TABLE 9 - CAViaR Extension
# No actively-maintained CRAN package reliably implements
# Engle-Manganelli CAViaR with exogenous regressors, so this is a
# from-scratch implementation of the Symmetric Absolute Value (SAV)
# specification, extended with lagged IHERI and GPR as exogenous
# risk drivers - this is standard practice in this literature given
# the lack of a canonical package.
# ================================================================

# ------------------------------------------------------------
# CAViaR-SAV with exogenous regressors:
# VaR_t = b0 + b1*VaR_{t-1} + b2*|y_{t-1}| + b3*X1_{t-1} + b4*X2_{t-1}
# ------------------------------------------------------------

caviar_loss <- function(params, y, x1, x2, tau) {
  n <- length(y)
  VaR <- numeric(n)
  VaR[1] <- quantile(y, tau)   # initialize at unconditional quantile
  
  b0 <- params[1]; b1 <- params[2]; b2 <- params[3]; b3 <- params[4]; b4 <- params[5]
  
  for (t in 2:n) {
    VaR[t] <- b0 + b1 * VaR[t-1] + b2 * abs(y[t-1]) + b3 * x1[t-1] + b4 * x2[t-1]
  }
  
  u <- y - VaR
  loss <- sum(u * (tau - as.numeric(u < 0)))
  return(loss)
}

fit_caviar <- function(y, x1, x2, tau, n_starts = 10, seed = 123) {
  set.seed(seed)
  best_loss <- Inf
  best_params <- NULL
  
  for (i in 1:n_starts) {
    start_vals <- c(
      runif(1, -0.5, 0.5),   # b0
      runif(1, 0.1, 0.95),   # b1 - autoregressive persistence
      runif(1, -0.5, 0.5),   # b2
      runif(1, -0.1, 0.1),   # b3 - exogenous var 1 loading
      runif(1, -0.1, 0.1)    # b4 - exogenous var 2 loading
    )
    
    fit <- tryCatch(
      optim(start_vals, caviar_loss, y = y, x1 = x1, x2 = x2, tau = tau,
            method = "Nelder-Mead", control = list(maxit = 5000)),
      error = function(e) NULL
    )
    
    if (!is.null(fit) && fit$value < best_loss) {
      best_loss <- fit$value
      best_params <- fit$par
    }
  }
  
  names(best_params) <- c("Intercept", "AR_VaR_lag1", "abs_return_lag1", "IHERI_lag1", "GPR_lag1")
  list(params = best_params, loss = best_loss)
}

# ------------------------------------------------------------
# Block bootstrap standard errors (200 replications, block length
# ~8 weeks to preserve short-run dependence structure)
# ------------------------------------------------------------

block_bootstrap_se <- function(y, x1, x2, tau, point_est, n_boot = 200, block_len = 8) {
  n <- length(y)
  boot_estimates <- matrix(NA, nrow = n_boot, ncol = 5)
  
  for (b in 1:n_boot) {
    n_blocks <- ceiling(n / block_len)
    starts <- sample(1:(n - block_len), n_blocks, replace = TRUE)
    idx <- unlist(lapply(starts, function(s) s:(s + block_len - 1)))
    idx <- idx[1:n]
    idx[idx > n] <- n
    
    y_boot <- y[idx]; x1_boot <- x1[idx]; x2_boot <- x2[idx]
    
    fit_b <- tryCatch(
      optim(point_est, caviar_loss, y = y_boot, x1 = x1_boot, x2 = x2_boot, tau = tau,
            method = "Nelder-Mead", control = list(maxit = 2000)),
      error = function(e) NULL
    )
    if (!is.null(fit_b)) boot_estimates[b, ] <- fit_b$par
  }
  
  apply(boot_estimates, 2, sd, na.rm = TRUE)
}

# ------------------------------------------------------------
# Kupiec (1995) unconditional coverage backtest
# ------------------------------------------------------------

kupiec_test <- function(y, VaR, tau) {
  hits <- as.numeric(y < VaR)
  n <- length(hits)
  x <- sum(hits)
  pi_hat <- x / n
  
  LR_uc <- -2 * (log(((1 - tau)^(n - x)) * (tau^x)) -
                   log(((1 - pi_hat)^(n - x)) * (pi_hat^x)))
  
  p_val <- 1 - pchisq(LR_uc, df = 1)
  list(hit_rate = pi_hat, expected_rate = tau, LR_stat = LR_uc, p_value = p_val)
}

# ------------------------------------------------------------
# Run CAViaR for each of the 5 food-market receivers, both tails
# (tau = 0.05 for downside VaR, tau = 0.95 for upside)
# Exogenous drivers: lagged IHERI_aggregate, lagged GPRD
# ------------------------------------------------------------

receivers <- c("lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")
caviar_results <- list()

caviar_input_df <- df %>%
  select(daten, all_of(receivers), iheri_aggregate, GPRD) %>%
  filter(complete.cases(.))

for (r in receivers) {
  for (tau_val in c(0.05, 0.95)) {
    
    cat("Fitting CAViaR:", r, "at tau =", tau_val, "...\n")
    
    y  <- caviar_input_df[[r]]
    x1 <- caviar_input_df$iheri_aggregate
    x2 <- caviar_input_df$GPRD
    
    fit <- fit_caviar(y, x1, x2, tau_val)
    se  <- block_bootstrap_se(y, x1, x2, tau_val, fit$params)
    
    z_stat <- fit$params / se
    p_val  <- 2 * (1 - pnorm(abs(z_stat)))
    
    # Reconstruct fitted VaR series for backtesting
    n <- length(y)
    VaR_fitted <- numeric(n)
    VaR_fitted[1] <- quantile(y, tau_val)
    for (t in 2:n) {
      VaR_fitted[t] <- fit$params[1] + fit$params[2]*VaR_fitted[t-1] +
        fit$params[3]*abs(y[t-1]) + fit$params[4]*x1[t-1] + fit$params[5]*x2[t-1]
    }
    
    backtest <- kupiec_test(y, VaR_fitted, tau_val)
    
    caviar_results[[paste0(r, "_tau", tau_val)]] <- data.frame(
      Receiver = r, Tau = tau_val,
      Parameter = names(fit$params), Estimate = fit$params, SE = se,
      Z = z_stat, P_value = p_val,
      Hit_Rate = backtest$hit_rate, Expected_Rate = backtest$expected_rate,
      Kupiec_LR = backtest$LR_stat, Kupiec_p = backtest$p_value
    )
  }
}

caviar_table <- do.call(rbind, caviar_results)
rownames(caviar_table) <- NULL

write_xlsx(caviar_table, "Table9_CAViaR_Results.xlsx")
print(caviar_table)

cat("\n=== Study 1 complete. All tables saved to D:/Food stocks/Study 1 ===\n")

# ================================================================
# STUDY 1 (Weekly): Figures
# Builds on objects already in session from the baseline + robustness
# scripts: static_dca, dynamic_dca, tanker_dca, noiheri_dca,
# gprdecomp_dca, iheridecomp_dca, qvar_results, caviar_table,
# caviar_input_df, connectedness_data
# ================================================================

setwd("D:/Food stocks/Study 1")

required_packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("figures", showWarnings = FALSE)

# ------------------------------------------------------------
# FIGURE 1 - Network plot, baseline static model
# Uses package's built-in PlotNetwork (writes file via 'path')
# ------------------------------------------------------------


png("figures/Figure1_Network_Baseline.png", width = 10, height = 10, units = "in", res = 300)
PlotNetwork(static_dca, method = "NPDC", threshold = 0)
dev.off()

# ------------------------------------------------------------
# FIGURE 3 - Net total directional connectedness (NET), all variables
# Small-multiples plot, one panel per variable, over time
# ------------------------------------------------------------

png("figures/Figure3_Net_Directional_Connectedness.png", width = 12, height = 8, units = "in", res = 300)
PlotNET(dynamic_dca)
dev.off()


# ------------------------------------------------------------
# FIGURE 4 - Quantile TCI comparison
# Static TCI at tau = 0.05 / 0.50 / 0.95, bar chart
# (Built manually since this is a cross-quantile comparison,
# not a single dca object the package plots natively)
# ------------------------------------------------------------

qvar_tci <- data.frame(
  Quantile = c("Lower tail (τ=0.05)", "Median (τ=0.50)", "Upper tail (τ=0.95)"),
  TCI = c(79.65, 47.68, 84.07)
)

qvar_tci$Quantile <- factor(qvar_tci$Quantile, levels = qvar_tci$Quantile)

fig4 <- ggplot(qvar_tci, aes(x = Quantile, y = TCI, fill = Quantile)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(TCI, "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("firebrick", "steelblue", "darkorange")) +
  labs(title = "Total Connectedness Index Across Quantiles (Study 1, Weekly)",
       y = "TCI (%)", x = "") +
  ylim(0, 95) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("figures/Figure4_Quantile_TCI_Comparison.png", fig4, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------
# FIGURE 5 - Net pairwise directional connectedness:
# IHERI_aggregate -> each food market receiver
# ------------------------------------------------------------

library(ggplot2)
library(tidyr)
library(dplyr)

var_names <- dimnames(dynamic_dca$NPDC)[[1]]
iheri_idx <- which(var_names == "iheri_aggregate")

time_index <- index(connectedness_data)

npdc_iheri <- data.frame(date = time_index)

for (j in seq_along(var_names)) {
  if (j != iheri_idx) {
    npdc_iheri[[var_names[j]]] <- dynamic_dca$NPDC[iheri_idx, j, ]
  }
}

npdc_long <- npdc_iheri %>%
  pivot_longer(-date, names_to = "Variable", values_to = "NPDC")

fig5 <- ggplot(npdc_long, aes(x = date, y = NPDC)) +
  geom_line(color = "navy", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Net Pairwise Directional Connectedness: IHERI vs. Other Variables",
       subtitle = "Positive = IHERI net transmitter to that variable",
       x = "", y = "NPDC") +
  theme_minimal(base_size = 11)

ggsave("figures/Figure5_NPDC_IHERI.png", fig5, width = 12, height = 8, dpi = 300)



# ------------------------------------------------------------
# FIGURE 6 - CAViaR backtest plots: actual returns vs fitted VaR,
# with breaches marked. Built for MOO and VEGI (where IHERI was
# significant) plus DBA as a contrast case where it wasn't.
# ------------------------------------------------------------

plot_caviar_backtest <- function(receiver_name, tau_val, y, x1, x2, params, title_suffix) {
  
  n <- length(y)
  VaR_fitted <- numeric(n)
  VaR_fitted[1] <- quantile(y, tau_val)
  for (t in 2:n) {
    VaR_fitted[t] <- params[1] + params[2]*VaR_fitted[t-1] +
      params[3]*abs(y[t-1]) + params[4]*x1[t-1] + params[5]*x2[t-1]
  }
  
  breach <- if (tau_val < 0.5) y < VaR_fitted else y > VaR_fitted
  
  plot_df <- data.frame(
    date = caviar_input_df$daten,
    Return = y, VaR = VaR_fitted, Breach = breach
  )
  
  ggplot(plot_df, aes(x = date)) +
    geom_line(aes(y = Return), color = "grey40", linewidth = 0.4) +
    geom_line(aes(y = VaR), color = "firebrick", linewidth = 0.7) +
    geom_point(data = subset(plot_df, Breach), aes(y = Return), color = "red", size = 1.5) +
    labs(title = paste0(receiver_name, " - CAViaR ", title_suffix, " (τ=", tau_val, ")"),
         y = "Weekly log return", x = "") +
    theme_minimal(base_size = 12)
}

# Re-fit params for the three cases we want to plot (re-using fit_caviar
# from the previous script - must be in session, or re-run that section first)

caviar_plot_specs <- list(
  list(receiver = "lr_MOO",  tau = 0.05, label = "Downside VaR"),
  list(receiver = "lr_VEGI", tau = 0.05, label = "Downside VaR"),
  list(receiver = "lr_VEGI", tau = 0.95, label = "Upside VaR"),
  list(receiver = "lr_DBA",  tau = 0.05, label = "Downside VaR")
)

caviar_plots <- list()

for (spec in caviar_plot_specs) {
  y  <- caviar_input_df[[spec$receiver]]
  x1 <- caviar_input_df$iheri_aggregate
  x2 <- caviar_input_df$GPRD
  
  fit <- fit_caviar(y, x1, x2, spec$tau)
  
  p <- plot_caviar_backtest(spec$receiver, spec$tau, y, x1, x2, fit$params, spec$label)
  caviar_plots[[paste0(spec$receiver, "_", spec$tau)]] <- p
}

combined_caviar <- gridExtra::grid.arrange(grobs = caviar_plots, ncol = 2)
ggsave("figures/Figure6_CAViaR_Backtests.png", combined_caviar, width = 14, height = 10, dpi = 300)

# ------------------------------------------------------------
# FIGURE 9 - Robustness comparison: IHERI's NET position across
# every specification (baseline, +tanker, GPR-decomposed, IHERI-decomposed)
# ------------------------------------------------------------

robustness_net <- data.frame(
  Specification = c("Baseline", "+ Tanker composite", "GPR decomposed",
                    "IHERI decomposed (aggregate row for ref.)"),
  NET = c(8.06, 6.76, 13.36, NA)   # pull directly from your Tables 2/4/6 NET values
)
robustness_net <- robustness_net[!is.na(robustness_net$NET), ]
robustness_net$Specification <- factor(robustness_net$Specification, levels = robustness_net$Specification)

fig9 <- ggplot(robustness_net, aes(x = Specification, y = NET, fill = Specification)) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = round(NET, 2)), vjust = -0.5, size = 5) +
  labs(title = "IHERI_aggregate: Net Transmitter Status Across Model Specifications",
       y = "NET spillover (%)", x = "") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("figures/Figure9_Robustness_NET_Comparison.png", fig9, width = 9, height = 6, dpi = 300)

cat("\n=== Study 1 figures complete. Saved to D:/Food stocks/Study 1/figures ===\n")







# ================================================================
# (Daily): Tanker-Composite Framework
# Transmitters: OVX, GPRD, CF, tanker_composite  |  Receivers: DBA, MOO, VEGI, PBJ, FTXG
# ================================================================

setwd("D:/Food stocks/Study 2")
dir.create("figures", showWarnings = FALSE)

required_packages <- c("haven", "dplyr", "zoo", "tseries", "FinTS", "moments",
                       "ConnectednessApproach", "writexl", "ggplot2", "tidyr", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------
# 1. Load & prepare data
# ------------------------------------------------------------

df <- read_dta("D:/Food stocks/Study 2/daily.dta") %>% arrange(daten)

# Tanker composite: log-return average across 6 tankers (NOT na.rm=TRUE,
# so the ~15 days INSW is missing drop cleanly via complete.cases below,
# keeping the composite's denominator consistent every day it's used)
df <- df %>%
  mutate(
    lr_FRO  = c(NA, diff(log(FRO))),  lr_DHT  = c(NA, diff(log(DHT))),
    lr_TNK  = c(NA, diff(log(TNK))),  lr_NAT  = c(NA, diff(log(NAT))),
    lr_STNG = c(NA, diff(log(STNG))), lr_INSW = c(NA, diff(log(INSW))),
    tanker_composite = rowMeans(cbind(lr_FRO, lr_DHT, lr_TNK, lr_NAT, lr_STNG, lr_INSW)),
    lr_DBA = c(NA, diff(log(DBA))), lr_MOO = c(NA, diff(log(MOO))),
    lr_VEGI = c(NA, diff(log(VEGI))), lr_PBJ = c(NA, diff(log(PBJ))),
    lr_FTXG = c(NA, diff(log(FTXG))), lr_OVX = c(NA, diff(log(OVX))),
    lr_CF = c(NA, diff(log(CF)))
  )

# Quick stationarity check on GPRD in levels at DAILY frequency -
cat("ADF on GPRD (daily, levels):\n"); print(adf.test(na.omit(df$GPRD)))
# If p > 0.05, difference GPRD here before proceeding:
# df$GPRD <- c(NA, diff(df$GPRD))

baseline_vars <- c("lr_OVX", "GPRD", "lr_CF", "tanker_composite",
                   "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")

baseline_df <- df %>% select(daten, all_of(baseline_vars)) %>% filter(complete.cases(.))
cat("Study 2 baseline panel:", nrow(baseline_df), "obs,",
    as.character(min(baseline_df$daten)), "to", as.character(max(baseline_df$daten)), "\n")

connectedness_data <- zoo(baseline_df[baseline_vars], order.by = baseline_df$daten)

# ------------------------------------------------------------
# 2. Table D1 - Descriptive statistics
# ------------------------------------------------------------

desc_stats <- data.frame(
  Variable = baseline_vars,
  Mean = sapply(baseline_df[baseline_vars], mean), SD = sapply(baseline_df[baseline_vars], sd),
  Skewness = sapply(baseline_df[baseline_vars], skewness),
  Kurtosis = sapply(baseline_df[baseline_vars], kurtosis),
  JB_pval  = sapply(baseline_df[baseline_vars], function(x) jarque.bera.test(x)$p.value),
  ADF_pval = sapply(baseline_df[baseline_vars], function(x) adf.test(x, k = 2)$p.value),
  ARCHLM_pval = sapply(baseline_df[baseline_vars], function(x) ArchTest(x, lags = 2)$p.value)
)
write_xlsx(desc_stats, "Table_D1_Descriptive_Statistics.xlsx")
print(desc_stats)

# ------------------------------------------------------------
# 3. Table D2 - Static connectedness, baseline
# ------------------------------------------------------------

static_dca <- ConnectednessApproach(connectedness_data, nlag = 1, nfore = 10,
                                    model = "VAR", connectedness = "Time",
                                    Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

static_table_df <- cbind(Variable = rownames(static_dca$TABLE), as.data.frame(static_dca$TABLE))
write_xlsx(static_table_df, "Table_D2_Static_Connectedness_Baseline.xlsx")
print(static_table_df)
saveRDS(static_dca, "static_dca_baseline.rds")

# ------------------------------------------------------------
# 4. Table D3 - Quantile connectedness (tau = 0.05/0.50/0.95)
# ------------------------------------------------------------

qvar_results <- list()
for (q in c(0.05, 0.50, 0.95)) {
  cat("Estimating QVAR at tau =", q, "...\n")
  fit <- ConnectednessApproach(connectedness_data, nlag = 1, nfore = 10,
                               model = "QVAR", connectedness = "Time",
                               VAR_config = list(QVAR = list(tau = q, method = "fn")))
  qvar_results[[paste0("tau_", q)]] <- fit
  
  tbl <- cbind(Variable = rownames(fit$TABLE), as.data.frame(fit$TABLE))
  write_xlsx(tbl, paste0("Table_D3_QVAR_tau", q, ".xlsx"))
  cat("--- tau =", q, "NET row ---\n"); print(tbl[tbl$Variable == "NET", ])
}
saveRDS(qvar_results, "qvar_results_baseline.rds")

# ------------------------------------------------------------
# 5. Table D4 - Robustness: exclude tanker_composite
#    (tests whether tanker index is THE dominant transmitter,
#    mirroring Study 1's exclude-IHERI logic)
# ------------------------------------------------------------

notanker_vars <- setdiff(baseline_vars, "tanker_composite")
notanker_df <- df %>% select(daten, all_of(notanker_vars)) %>% filter(complete.cases(.))
notanker_data <- zoo(notanker_df[notanker_vars], order.by = notanker_df$daten)

notanker_dca <- ConnectednessApproach(notanker_data, nlag = 1, nfore = 10,
                                      model = "VAR", connectedness = "Time",
                                      Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

notanker_table_df <- cbind(Variable = rownames(notanker_dca$TABLE), as.data.frame(notanker_dca$TABLE))
write_xlsx(notanker_table_df, "Table_D4_Robustness_ExcludeTanker.xlsx")
saveRDS(notanker_dca, "notanker_dca.rds")

# ------------------------------------------------------------
# 6. Table D5 - Robustness: GPR decomposed (Acts vs Threats)
# ------------------------------------------------------------

gprdecomp_vars <- c("lr_OVX", "GPRD_ACT", "GPRD_THREAT", "lr_CF", "tanker_composite",
                    "lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")
gprdecomp_df <- df %>% select(daten, all_of(gprdecomp_vars)) %>% filter(complete.cases(.))
gprdecomp_data <- zoo(gprdecomp_df[gprdecomp_vars], order.by = gprdecomp_df$daten)

gprdecomp_dca <- ConnectednessApproach(gprdecomp_data, nlag = 1, nfore = 10,
                                       model = "VAR", connectedness = "Time",
                                       Connectedness_config = list(TimeConnectedness = list(generalized = TRUE)))

gprdecomp_table_df <- cbind(Variable = rownames(gprdecomp_dca$TABLE), as.data.frame(gprdecomp_dca$TABLE))
write_xlsx(gprdecomp_table_df, "Table_D5_Robustness_GPR_Decomposed.xlsx")
saveRDS(gprdecomp_dca, "gprdecomp_dca.rds")

# ------------------------------------------------------------
# 7. Dynamic TCI (TVP-VAR) - feeds Figures D2-D4
# ------------------------------------------------------------

dynamic_dca <- ConnectednessApproach(connectedness_data, nlag = 1, nfore = 10, model = "TVP-VAR",
                                     connectedness = "Time",
                                     VAR_config = list(TVPVAR = list(kappa1 = 0.99, kappa2 = 0.99, prior = "BayesPrior", gamma = 0.01)))
saveRDS(dynamic_dca, "dynamic_dca_baseline.rds")

# ================================================================
# 8. Table D6 - CAViaR (identical SAV engine as Study 1, exogenous
#    drivers now tanker_composite + GPRD in place of IHERI + GPRD)
# ================================================================

caviar_loss <- function(params, y, x1, x2, tau) {
  n <- length(y); VaR <- numeric(n); VaR[1] <- quantile(y, tau)
  b0<-params[1]; b1<-params[2]; b2<-params[3]; b3<-params[4]; b4<-params[5]
  for (t in 2:n) VaR[t] <- b0 + b1*VaR[t-1] + b2*abs(y[t-1]) + b3*x1[t-1] + b4*x2[t-1]
  u <- y - VaR; sum(u * (tau - as.numeric(u < 0)))
}

fit_caviar <- function(y, x1, x2, tau, n_starts = 10, seed = 123) {
  set.seed(seed); best_loss <- Inf; best_params <- NULL
  for (i in 1:n_starts) {
    sv <- c(runif(1,-0.5,0.5), runif(1,0.1,0.95), runif(1,-0.5,0.5), runif(1,-0.1,0.1), runif(1,-0.1,0.1))
    fit <- tryCatch(optim(sv, caviar_loss, y=y, x1=x1, x2=x2, tau=tau,
                          method="Nelder-Mead", control=list(maxit=5000)), error=function(e) NULL)
    if (!is.null(fit) && fit$value < best_loss) { best_loss <- fit$value; best_params <- fit$par }
  }
  names(best_params) <- c("Intercept","AR_VaR_lag1","abs_return_lag1","Tanker_lag1","GPR_lag1")
  list(params = best_params, loss = best_loss)
}

block_bootstrap_se <- function(y, x1, x2, tau, point_est, n_boot=200, block_len=8) {
  n <- length(y); boot <- matrix(NA, n_boot, 5)
  for (b in 1:n_boot) {
    starts <- sample(1:(n-block_len), ceiling(n/block_len), replace=TRUE)
    idx <- unlist(lapply(starts, function(s) s:(s+block_len-1))); idx <- idx[1:n]; idx[idx>n] <- n
    fit_b <- tryCatch(optim(point_est, caviar_loss, y=y[idx], x1=x1[idx], x2=x2[idx], tau=tau,
                            method="Nelder-Mead", control=list(maxit=2000)), error=function(e) NULL)
    if (!is.null(fit_b)) boot[b,] <- fit_b$par
  }
  apply(boot, 2, sd, na.rm=TRUE)
}

kupiec_test <- function(y, VaR, tau) {
  hits <- as.numeric(y < VaR); n <- length(hits); x <- sum(hits); pi_hat <- x/n
  LR <- -2*(log(((1-tau)^(n-x))*(tau^x)) - log(((1-pi_hat)^(n-x))*(pi_hat^x)))
  list(hit_rate = pi_hat, LR_stat = LR, p_value = 1 - pchisq(LR, df=1))
}

receivers <- c("lr_DBA", "lr_MOO", "lr_VEGI", "lr_PBJ", "lr_FTXG")
caviar_input_df <- df %>% select(daten, all_of(receivers), tanker_composite, GPRD) %>% filter(complete.cases(.))
caviar_results <- list()

for (r in receivers) for (tau_val in c(0.05, 0.95)) {
  cat("CAViaR:", r, "tau =", tau_val, "\n")
  y <- caviar_input_df[[r]]; x1 <- caviar_input_df$tanker_composite; x2 <- caviar_input_df$GPRD
  fit <- fit_caviar(y, x1, x2, tau_val); se <- block_bootstrap_se(y, x1, x2, tau_val, fit$params)
  z <- fit$params/se; p <- 2*(1-pnorm(abs(z)))
  
  n <- length(y); VaR <- numeric(n); VaR[1] <- quantile(y, tau_val)
  for (t in 2:n) VaR[t] <- fit$params[1]+fit$params[2]*VaR[t-1]+fit$params[3]*abs(y[t-1])+fit$params[4]*x1[t-1]+fit$params[5]*x2[t-1]
  bt <- kupiec_test(y, VaR, tau_val)
  
  caviar_results[[paste0(r,"_tau",tau_val)]] <- data.frame(
    Receiver=r, Tau=tau_val, Parameter=names(fit$params), Estimate=fit$params, SE=se, Z=z, P_value=p,
    Hit_Rate=bt$hit_rate, Kupiec_p=bt$p_value)
}

caviar_table <- do.call(rbind, caviar_results); rownames(caviar_table) <- NULL
write_xlsx(caviar_table, "Table_D6_CAViaR_Results.xlsx")
print(caviar_table)

# ================================================================
# FIGURES
# ================================================================

# Figure D1 - Network plot 
png("figures/Figure_D1_Network_Baseline.png", width=10, height=10, units="in", res=300)
PlotNetwork(static_dca, method="NPDC", threshold=0)
dev.off()

# Figure D2 - Net total directional connectedness, small multiples
png("figures/Figure_D2_Net_Directional_Connectedness.png", width=12, height=8, units="in", res=300)
PlotNET(dynamic_dca)
dev.off()

# Figure D3 - pull TCI directly from your Table D3 exports
# (NET row, FROM column, format "X/Y" — Y is TCI)
qvar_tci <- data.frame(
  Quantile = factor(c("Lower tail (τ=0.05)","Median (τ=0.50)","Upper tail (τ=0.95)"),
                    levels = c("Lower tail (τ=0.05)","Median (τ=0.50)","Upper tail (τ=0.95)")),
  TCI = c(81.62, 44.60, 82.12)   # from your Table_D3 exports
)
fig_d3 <- ggplot(qvar_tci, aes(Quantile, TCI, fill=Quantile)) + geom_col(width=0.6) +
  geom_text(aes(label=paste0(TCI,"%")), vjust=-0.5, size=5) +
  scale_fill_manual(values=c("firebrick","steelblue","darkorange")) +
  labs(title="Total Connectedness Index Across Quantiles (Study 2, Daily)", y="TCI (%)", x="") +
  ylim(0, 95) + theme_minimal(base_size=14) + theme(legend.position="none")
ggsave("figures/Figure_D3_Quantile_TCI_Comparison.png", fig_d3, width=8, height=6, dpi=300)

# Figure D4 - NPDC: tanker_composite vs all other variables
var_names <- dimnames(dynamic_dca$NPDC)[[1]]
tanker_idx <- which(var_names == "tanker_composite")
time_index <- index(connectedness_data)

npdc_tanker <- data.frame(date = time_index)
for (j in seq_along(var_names)) if (j != tanker_idx) {
  npdc_tanker[[var_names[j]]] <- dynamic_dca$NPDC[tanker_idx, j, ]
}
npdc_long <- npdc_tanker %>% pivot_longer(-date, names_to="Variable", values_to="NPDC")

fig_d4 <- ggplot(npdc_long, aes(date, NPDC)) + geom_line(color="darkgreen", linewidth=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  facet_wrap(~Variable, scales="free_y") +
  labs(title="Net Pairwise Directional Connectedness: Tanker Composite vs. Other Variables",
       subtitle="Positive = Tanker composite net transmitter to that variable", x="", y="NPDC") +
  theme_minimal(base_size=11)
ggsave("figures/Figure_D4_NPDC_TankerComposite.png", fig_d4, width=12, height=8, dpi=300)

# Figure D5 - CAViaR backtests (MOO, VEGI, PBJ - adjust based on which
plot_caviar_backtest <- function(receiver_name, tau_val, y, x1, x2, params, title_suffix) {
  n <- length(y); VaR <- numeric(n); VaR[1] <- quantile(y, tau_val)
  for (t in 2:n) VaR[t] <- params[1]+params[2]*VaR[t-1]+params[3]*abs(y[t-1])+params[4]*x1[t-1]+params[5]*x2[t-1]
  breach <- if (tau_val < 0.5) y < VaR else y > VaR
  pdf <- data.frame(date=caviar_input_df$daten, Return=y, VaR=VaR, Breach=breach)
  ggplot(pdf, aes(date)) + geom_line(aes(y=Return), color="grey40", linewidth=0.3) +
    geom_line(aes(y=VaR), color="firebrick", linewidth=0.6) +
    geom_point(data=subset(pdf, Breach), aes(y=Return), color="red", size=1) +
    labs(title=paste0(receiver_name," - CAViaR ",title_suffix," (τ=",tau_val,")"), y="Daily log return", x="") +
    theme_minimal(base_size=12)
}

specs <- list(list(r="lr_MOO", tau=0.05), list(r="lr_VEGI", tau=0.05),
              list(r="lr_VEGI", tau=0.95), list(r="lr_PBJ", tau=0.05))
plots <- lapply(specs, function(s) {
  y<-caviar_input_df[[s$r]]; x1<-caviar_input_df$tanker_composite; x2<-caviar_input_df$GPRD
  fit <- fit_caviar(y, x1, x2, s$tau)
  plot_caviar_backtest(s$r, s$tau, y, x1, x2, fit$params, if(s$tau<0.5) "Downside VaR" else "Upside VaR")
})
combined <- gridExtra::grid.arrange(grobs=plots, ncol=2)
ggsave("figures/Figure_D5_CAViaR_Backtests.png", combined, width=14, height=10, dpi=300)

# Figure D6 - tanker_composite's NET row from Table D2 and Table D5
robustness_net <- data.frame(
  Specification = factor(c("Baseline","GPR decomposed"), levels=c("Baseline","GPR decomposed")),
  NET = c(-17.40, -17.30)   # from Table D2 and Table D5, tanker_composite column, NET row
)
fig_d6 <- ggplot(robustness_net, aes(Specification, NET, fill=Specification)) +
  geom_col(width=0.5) + geom_hline(yintercept=0, linetype="dashed") +
  geom_text(aes(label=round(NET,2)), vjust=1.5, size=5) +
  labs(title="Tanker Composite: Net Transmitter Status Across Specifications", y="NET spillover (%)", x="") +
  theme_minimal(base_size=13) + theme(legend.position="none")
ggsave("figures/Figure_D6_Robustness_NET_Comparison.png", fig_d6, width=8, height=6, dpi=300)

cat("\n=== Study 2 complete. Tables and figures saved to D:/Food stocks/Study 2 ===\n")

