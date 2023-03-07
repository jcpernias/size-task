library(tidyverse)

# Norway, France, Netherlands, Spain, USA, UK,
# NOR, FRA, NLD, ESP, USA, GBR, DEU


countries <-
  c(DEU = "Alemania", ESP = "España",
    FRA = "Francia", USA = "EE.UU.")

read_oecd_csv <- function(path) {
  read_csv(path, col_types = "cccccid_") |>
    select(LOCATION, TIME, Value) |>
    filter(LOCATION %in% names(countries), TIME >= 1995) |>
    pivot_wider(names_from = LOCATION, values_from = Value) |>
    rename(year = TIME)
}

g_db <- read_oecd_csv("./data/oecd_expend.csv.bz2")
g_db |>
  pivot_longer(-year) |>
  line_plot(labels = countries) +
  theme(legend.position = "bottom")


rev_db <- read_oecd_csv("./data/oecd_rev.csv.bz2")
rev_db |>
  pivot_longer(-year) |>
  line_plot(labels = countries) +
  theme(legend.position = "bottom")


def_db <- read_oecd_csv("./data/oecd_def.csv.bz2")
def_db |>
  pivot_longer(-year) |>
  line_plot(labels = countries) +
  theme(legend.position = "bottom")

debt_db <- read_oecd_csv("./data/oecd_debt.csv.bz2")
debt_db |>
  pivot_longer(-year) |>
  line_plot(labels = countries) +
  theme(legend.position = "bottom")

tax_db <- read_oecd_csv("./data/oecd_tax.csv.bz2")
tax_db |>
  pivot_longer(-year) |>
  line_plot(labels = countries) +
  theme(legend.position = "bottom")

