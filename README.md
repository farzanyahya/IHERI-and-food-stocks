Iran–Hormuz Energy Risk Index (IHERI) – Validation and Connectedness Analysis
This repository contains Stata and R scripts, along with supporting datasets, for validating and analyzing the Iran–Hormuz Energy Risk (IHERI) Index. The IHERI index is constructed from Google Trends data and is used to study geopolitical energy risk, weekly and daily dynamics, and its connectedness with global food stocks.

Repository Contents
1. IHERI validity analysis.do
A Stata script used to validate the IHERI index.

Uses weekly index validation.dta, which contains the weekly Google Trends–based IHERI series.

Performs diagnostic checks, robustness tests, and validity analysis of the index.

2. R script for IHERI.R
An R script showing how the IHERI index is constructed.

Demonstrates the full workflow for generating the index from Google Trends queries.

Useful for replication, transparency, and methodological review.

3. full R script food stocks.R
An R script analyzing the connectedness between the IHERI index and global food stocks.

Implements connectedness or spillover models.

Uses both weekly and daily versions of the index depending on the analysis.

4. weekly.dta
Weekly dataset used for:

Weekly connectedness analysis

Weekly validation checks

Time‑series modeling of weekly IHERI dynamics

5. daily.dta
Daily dataset used for:

Daily connectedness analysis

High‑frequency validation

Event‑study style checks of daily IHERI movements

6. weekly index validation.dta
Contains the weekly Google Trends–derived IHERI index used in the Stata validity script.

Usage Overview
Use IHERI validity analysis.do to run Stata‑based validity checks on the weekly index contained in weekly index validation.dta.

Use R script for IHERI.R to understand or replicate how the IHERI index is constructed from Google Trends data.

Use full R script food stocks.R to analyze the connectedness between the IHERI index and global food stocks.

Use weekly.dta and daily.dta depending on whether weekly or daily connectedness is being evaluated.

Purpose
This repository supports research on geopolitical energy risk in the Iran–Hormuz region, providing:

A transparent index construction workflow

Validation procedures

Connectedness analysis with global food markets

Weekly and daily datasets for replication
