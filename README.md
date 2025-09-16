# NFL Quarterback Analytics Toolkit

## Overview
This repository centralizes my tooling for analyzing NFL quarterback performance. The
`src` pipeline automates downloading play-by-play data, normalizing player names,
and computing season and week level efficiency metrics using the
[`nflverse`](https://nflverse.nflverse.com) family of packages. The `DataViz`
directory builds on those data frames to produce exploratory plots (CPOE vs. EPA,
pressure rate comparisons, player-specific weekly trends, and more) that support
weekly reporting, gambling research, and historical studies. The
`generalPlayground` folder contains one-off notebooks and scratch work where ideas
are prototyped before being promoted into the pipeline.

## Data Sources
The workflow relies on publicly available data that the `nflverse` ecosystem
exposes:

- `nflreadr::load_pbp()` for league-wide play-by-play data (current season and
  historical archives).
- `nflreadr::load_pfr_advstats()` for Pro Football Reference advanced passing
  metrics at the season and weekly levels.

Both helpers download and cache data locally on first use. Expect the initial run
(of historical play-by-play in particular) to take several minutes and require a
reliable internet connection.

## Prerequisites
- **R 4.1+** (earlier versions may work but are not routinely tested).
- The following R packages: `tidyverse`, `nflreadr`, `nflfastR`, `nflplotR`,
  `nflverse`, `nflseedR`, `nfl4th`, `ffopportunity`, `ggrepel`, `stringr`, and
  `zoo`.

Install everything from CRAN in one shot:

```r
install.packages(c(
  "tidyverse", "nflreadr", "nflfastR", "nflplotR", "nflverse",
  "nflseedR", "nfl4th", "ffopportunity", "ggrepel", "stringr", "zoo"
))
```

The scripts assume you are comfortable running R scripts from the command line or
within RStudio. If you prefer to isolate dependencies, initialize an
[`renv`](https://rstudio.github.io/renv/) project before installing packages.

## Project Setup & Usage
1. **Clone the repository**
   ```bash
   git clone https://github.com/<your-user>/nfl.git
   cd nfl
   ```
2. **Configure the season and week** inside `src/scripts/00_setup.R`. The
   `current_week`, `postseason_week`, and `season` variables drive which slices of
data downstream scripts compute.
3. **Run the pipeline**
   ```bash
   Rscript src/main.R
   ```
   `src/main.R` sources each numbered script in order, constructing in-memory data
frames such as `qb_25`, `qb_weekly_25`, `qb_25_current_week`, and
`cw_weekly_plot`. When the message `Pipeline complete ✅` prints, the data objects
are ready for analysis or visualization in the current R session.
4. **Generate visualizations** by sourcing one of the scripts in `DataViz/` from
the same R session that ran the pipeline (or by saving the pipeline outputs to an
`.RData`/`.rds` file and reloading them). Each visualization script expects the
objects created by the pipeline to already exist.

### Working Incrementally
Each script inside `src/scripts/` can also be sourced individually while
iterating:

- `00_setup.R`: loads libraries, sets global season/week switches, and prints the
  run context.
- `01_load_raw.R`: fetches play-by-play data and advanced quarterback stats.
- `02_clean_names.R`: standardizes player naming conventions and fixes common edge
  cases (e.g., suffixes, initials).
- `03_qb_season_aggregates.R`: computes season-level aggregates, merges PFR
  advanced metrics, and derives league averages for reference lines in plots.
- `04_qb_weekly_aggregates.R`: produces weekly, current-week, and postseason QB
  splits while attaching matching PFR weekly stats.
- `05_caleb_analysis.R`: focuses on Caleb Williams, building rolling EPA views,
  opponent summaries, and helper tables for annotating plays.

Running scripts individually is helpful when experimenting in an interactive R
session or when only a subset of data needs to be refreshed.

## Repository Structure
```text
.
├── DataViz/                 # Visualization scripts that depend on the pipeline objects
│   ├── allTime/             # Historical views spanning the full play-by-play archive
│   ├── originalRubbish/     # Early prototypes and exploratory plotting scratch work
│   ├── players/             # Player-specific analyses
│   │   ├── Aaron Rodgers/
│   │   │   ├── gameByGame/  # Game-level breakdowns for the named quarterback
│   │   │   └── weekly/      # Week-over-week trend charts for that quarterback
│   │   ├── Caleb Williams/
│   │   │   ├── gameByGame/
│   │   │   └── weekly/
│   │   └── Jayden Daniels/
│   │       ├── gameByGame/
│   │       └── weekly/
│   ├── badThrowPct_EPA.R    # Scatter plot: bad throw % vs. EPA/play
│   ├── CPOE_EPA.R           # Scatter plot: CPOE vs. EPA/play (season and current week)
│   ├── onTargetPct_EPA.R    # Scatter plot: on-target % vs. EPA/play
│   ├── pocketTime_EPA.R     # Scatter plot: pocket time vs. EPA/play
│   ├── pressurePct_EPA.R    # Scatter plot: pressure rate vs. EPA/play
│   ├── rushEPA_passEPA.R    # Scatter plot: rushing EPA vs. passing EPA
│   └── sacksTaken_EPA.R     # Scatter plot: sacks taken vs. EPA/play
├── generalPlayground/       # One-off cleaning experiments and supporting notebooks
│   ├── 2025_DataCleaning.R  # Initial 2025 season wrangling (precursor to src/scripts)
│   ├── Gambling_DataCleaning.R
│   └── nfelo.r
├── src/                     # Production pipeline entry points
│   ├── main.R               # Orchestrates the end-to-end pipeline
│   └── scripts/
│       ├── 00_setup.R       # Library loads and season/week switches
│       ├── 01_load_raw.R    # Pulls play-by-play & PFR advanced stats
│       ├── 02_clean_names.R # Normalizes player names and handles edge cases
│       ├── 03_qb_season_aggregates.R
│       ├── 04_qb_weekly_aggregates.R
│       └── 05_caleb_analysis.R
└── README.md                # Project documentation (this file)
```

## Tips & Best Practices
- **Cache large downloads**: `nflreadr` caches API responses under
  `~/.cache/nflreadr/` by default. Keeping the cache intact dramatically speeds up
  subsequent runs.
- **Respect API rate limits**: While the nflverse services are robust, avoid
  repeatedly clearing caches and re-downloading in tight loops.
- **Extend with care**: Add new analytical steps by creating a `src/scripts/06_*` file
  and sourcing it from `src/main.R` to keep the pipeline deterministic.
- **Visualization context**: The scatter plots intentionally mute legends by
  default; toggle them back on if you need them for presentations or exports.

## Getting Help
If you encounter issues with the public data sources, start with the
[nflverse documentation](https://nflverse.nflverse.com/articles/nflverse-data.html)
and GitHub issues. Repository-specific questions or improvements are welcome via
pull request or direct contact.
