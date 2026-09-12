# ================================================================
# Iran–Hormuz Energy Risk Index (IHERI)
# Final batched, cached, and reproducible R script
# ================================================================

# ------------------------------------------------------------
# Setup: working directory and cache
# ------------------------------------------------------------

dir.create("D:/IHERI_index", showWarnings = FALSE)
setwd("D:/IHERI_index")

cache_dir <- "cache_batched"
dir.create(cache_dir, showWarnings = FALSE)

set.seed(12345)

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------

library(gtrendsR)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(stringr)

# ------------------------------------------------------------
# Keyword dictionaries
# ------------------------------------------------------------

chokepoint_risk <- c(
  "Hormuz", "Strait of Hormuz", "Persian Gulf", "Gulf of Oman",
  "Iran Israel war", "Iran attack", "Iran missile", "Iran war",
  "Iran strike", "Iran conflict"
)

market_risk <- c(
  "Iran oil", "oil price surge", "Iran sanctions", "Brent crude",
  "crude oil price", "oil market", "Iran oil exports", "OPEC Iran",
  "energy prices", "oil price today"
)

transport_risk <- c(
  "oil tanker", "tanker attack", "shipping disruption", "tanker seized",
  "maritime security", "Red Sea shipping", "Houthi attack",
  "shipping insurance", "tanker traffic", "vessel attacked"
)

anchor_term <- "Iran oil"

# ------------------------------------------------------------
# Batch construction: anchor + up to 4 terms per call
# ------------------------------------------------------------

batch_keywords <- function(keywords, anchor, batch_size = 4) {
  keywords_no_anchor <- setdiff(keywords, anchor)
  chunks <- split(keywords_no_anchor, ceiling(seq_along(keywords_no_anchor) / batch_size))
  lapply(chunks, function(k) unique(c(anchor, k)))
}

chokepoint_batches <- batch_keywords(chokepoint_risk, anchor_term)
market_batches     <- batch_keywords(market_risk, anchor_term)
transport_batches  <- batch_keywords(transport_risk, anchor_term)

# ------------------------------------------------------------
# Time windows
# ------------------------------------------------------------

time_windows <- tibble(
  start = as.Date(c("2004-01-01", "2007-01-01", "2010-01-01",
                    "2013-01-01", "2016-01-01", "2019-01-01",
                    "2022-01-01", "2025-01-01")),
  end   = as.Date(c("2006-12-31", "2009-12-31", "2012-12-31",
                    "2015-12-31", "2018-12-31", "2021-12-31",
                    "2024-12-31", NA))
)

time_windows$end[is.na(time_windows$end)] <- Sys.Date() - 2

# ------------------------------------------------------------
# Cache path (keyed by sorted batch + window)
# ------------------------------------------------------------

cache_path <- function(component, batch, win_start, win_end) {
  batch_id <- str_replace_all(paste(sort(batch), collapse = "_"), "[^A-Za-z0-9]+", "_")
  file.path(
    cache_dir,
    sprintf("%s__%s__%s_%s.rds", component, batch_id, win_start, win_end)
  )
}

# ------------------------------------------------------------
# Fetch one batch (with retry and caching)
# ------------------------------------------------------------

fetch_batch <- function(component, batch, win_start, win_end) {
  cpath <- cache_path(component, batch, win_start, win_end)
  if (file.exists(cpath)) return(readRDS(cpath))
  
  res <- tryCatch(
    gtrends(keyword = batch, geo = "", time = paste(win_start, win_end)),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    Sys.sleep(5)
    res <- tryCatch(
      gtrends(keyword = batch, geo = "", time = paste(win_start, win_end)),
      error = function(e) NULL
    )
  }
  
  out <- if (!is.null(res) && !is.null(res$interest_over_time) &&
            nrow(res$interest_over_time) > 0) {
    res$interest_over_time %>%
      mutate(
        date = as.Date(date),
        hits = case_when(
          hits == "<1" ~ 0.5,
          TRUE ~ suppressWarnings(as.numeric(hits))
        ),
        hits = ifelse(is.na(hits), 0, hits),
        keyword = as.character(keyword)
      ) %>%
      select(date, hits, keyword)
  } else {
    tibble(date = as.Date(character()), hits = numeric(), keyword = character())
  }
  
  saveRDS(out, cpath)
  out
}

# ------------------------------------------------------------
# Run all batches for one component (with delays and cooldowns)
# ------------------------------------------------------------

fetch_component_batched <- function(batches, component_name, batch_counter_env) {
  map_df(seq_along(batches), function(b) {
    map_df(seq_len(nrow(time_windows)), function(i) {
      
      out <- fetch_batch(
        component_name,
        batches[[b]],
        time_windows$start[i],
        time_windows$end[i]
      )
      
      status <- if (nrow(out) > 0) "OK" else "FAILED"
      cat(sprintf(
        "%s | batch %d/%d | %s to %s -> %s (%s)\n",
        component_name, b, length(batches),
        time_windows$start[i], time_windows$end[i], status,
        paste(batches[[b]], collapse = ", ")
      ))
      flush.console()
      
      batch_counter_env$n <- batch_counter_env$n + 1
      Sys.sleep(runif(1, 12, 20))
      
      if (batch_counter_env$n %% 10 == 0) {
        cooldown <- runif(1, 45, 60)
        cat(sprintf("  -- cooldown: %.0f sec --\n", cooldown))
        flush.console()
        Sys.sleep(cooldown)
      }
      
      out
    })
  })
}

counter <- new.env()
counter$n <- 0

chokepoint_raw <- fetch_component_batched(chokepoint_batches, "Chokepoint_Risk", counter)
market_raw     <- fetch_component_batched(market_batches,     "Market_Risk",     counter)
transport_raw  <- fetch_component_batched(transport_batches,  "Transport_Risk",  counter)

# ------------------------------------------------------------
# Cleaning and weekly aggregation
# ------------------------------------------------------------

clean_component <- function(df, own_keywords) {
  df %>%
    filter(!is.na(date), keyword %in% own_keywords) %>%
    mutate(week = floor_date(date, "week")) %>%
    group_by(week, keyword) %>%
    summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop")
}

chokepoint_clean <- clean_component(chokepoint_raw, chokepoint_risk)
market_clean     <- clean_component(market_raw,     market_risk)
transport_clean  <- clean_component(transport_raw,  transport_risk)

# ------------------------------------------------------------
# Index construction (mean across keywords per component)
# ------------------------------------------------------------

make_index <- function(df, name) {
  df %>%
    group_by(week) %>%
    summarise(mean_index = mean(hits, na.rm = TRUE), .groups = "drop") %>%
    rename(!!name := mean_index)
}

chokepoint_index <- make_index(chokepoint_clean, "IHERI_C")
market_index     <- make_index(market_clean,     "IHERI_M")
transport_index  <- make_index(transport_clean,  "IHERI_T")

IHERI <- chokepoint_index %>%
  full_join(market_index,   by = "week") %>%
  full_join(transport_index, by = "week") %>%
  arrange(week) %>%
  mutate(across(c(IHERI_C, IHERI_M, IHERI_T), ~replace_na(.x, 0))) %>%
  mutate(IHERI_aggregate = rowMeans(select(., IHERI_C, IHERI_M, IHERI_T), na.rm = TRUE))

# ------------------------------------------------------------
# Diagnostics: keyword health
# ------------------------------------------------------------

keyword_health <- bind_rows(chokepoint_clean, market_clean, transport_clean) %>%
  group_by(keyword) %>%
  summarise(
    max_hits = max(hits, na.rm = TRUE),
    n_weeks  = n(),
    .groups  = "drop"
  ) %>%
  arrange(max_hits)

print(keyword_health, n = 40)

cat("\n=== INITIAL RUN DONE ===\n")

# ------------------------------------------------------------
# Targeted retry for known failed cache entries
# ------------------------------------------------------------

failed_specs <- list(
  list(
    component = "Transport_Risk",
    batch     = c("Iran oil", "tanker traffic", "vessel attacked"),
    start     = as.Date("2025-01-01"),
    end       = Sys.Date() - 2
  )
)

for (spec in failed_specs) {
  cpath <- cache_path(spec$component, spec$batch, spec$start, spec$end)
  if (file.exists(cpath)) file.remove(cpath)
}

retry_results <- map_df(failed_specs, function(spec) {
  Sys.sleep(runif(1, 20, 30))
  out <- fetch_batch(spec$component, spec$batch, spec$start, spec$end)
  cat(sprintf(
    "Retry %s (%s to %s): %s\n",
    spec$component, spec$start, spec$end,
    if (nrow(out) > 0) "OK" else "STILL FAILED"
  ))
  out
})

transport_raw_updated <- bind_rows(transport_raw, retry_results) %>%
  distinct(date, keyword, .keep_all = TRUE)

transport_clean <- clean_component(transport_raw_updated, transport_risk)
transport_index <- make_index(transport_clean, "IHERI_T")

IHERI <- chokepoint_index %>%
  full_join(market_index,   by = "week") %>%
  full_join(transport_index, by = "week") %>%
  arrange(week) %>%
  mutate(across(c(IHERI_C, IHERI_M, IHERI_T), ~replace_na(.x, 0))) %>%
  mutate(IHERI_aggregate = rowMeans(select(., IHERI_C, IHERI_M, IHERI_T), na.rm = TRUE))

# ------------------------------------------------------------
# Output: CSV files
# ------------------------------------------------------------

write.csv(chokepoint_clean, "IHERI_chokepoint_weekly.csv", row.names = FALSE)
write.csv(market_clean,     "IHERI_market_weekly.csv",     row.names = FALSE)
write.csv(transport_clean,  "IHERI_transport_weekly.csv",  row.names = FALSE)
write.csv(IHERI,            "IHERI_combined.csv",          row.names = FALSE)

cat("\n=== FINAL IHERI BUILD COMPLETE ===\n")
