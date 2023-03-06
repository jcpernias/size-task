library(tidyverse)

# Norway, France, Netherlands, Spain, USA, UK,
# NOR, FRA, NLD, ESP, USA, GBR, DEU

countries <- c("NOR", "FRA", "NLD", "ESP", "USA", "GBR", "DEU")

read_oecd_csv <- function(path) {
  read_csv(path, col_types = "cccccid_") |>
    select(LOCATION, TIME, Value) |>
    pivot_wider(names_from = LOCATION, values_from = Value)
}

g_db <- read_oecd_csv("./data/oecd_expend.csv.bz2")

rev_db <- read_oecd_csv("./data/oecd_rev.csv.bz2")

def_db <- read_oecd_csv("./data/oecd_def.csv.bz2")

def_db <- read_oecd_csv("./data/oecd_debt.csv.bz2")

tax_db <- read_oecd_csv("./data/oecd_tax.csv.bz2")

