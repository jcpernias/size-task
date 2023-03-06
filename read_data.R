library(tidyverse)
library(openxlsx)

# IGAE data: Total government
aapp_vars <-
  tibble(row = c(7, 17, 36, 63, 64, 83),
         vname = c("Gastos", "Intereses", "Ingresos",
                   "SFiscal", "SPrimario", "PIB"))

aapp <- local({
  df_list <- list(
    read.xlsx("./data/A_AAPP.xlsx", "Tabla1a",
              colNames = TRUE,
              rows = c(6, aapp_vars$row[-nrow(aapp_vars)])),
    read.xlsx("./data/A_AAPP.xlsx", "Tabla1b",
              colNames = TRUE,
              rows = c(6, aapp_vars$row[nrow(aapp_vars)])) |>
      mutate(CONCEPTOS  = as.character(CONCEPTOS)))

  bind_rows(df_list)
}) |>
  select(-c("CÓDIGO", "CONCEPTOS")) |>
  mutate(vnames = aapp_vars$vname) |>
  rename("2021" = "2021(P)") |>
  pivot_longer(cols = 1:27, names_to = "year") |>
  mutate(year = as.integer(year)) |>
  pivot_wider(names_from = vnames, values_from = value)


# IGAE data: Taxes
imp_vars <-
  tibble(row = c(7, 74, 86, 95, 96, 97, 98),
         vname = c("TInd", "TRenta", "TCap", "TTotal",
                   "CotSS1", "CotSS2", "CotSS3"))
imp <-
    read.xlsx("./data/A_AAPP_Imp.xlsx", "Tabla1a",
              colNames = TRUE,
              rows = c(6, imp_vars$row)) |>
  select(-c("CÓDIGO", "CONCEPTOS")) |>
  mutate(vnames = imp_vars$vname) |>
  rename("2021" = "2021(P)") |>
  pivot_longer(cols = 1:27, names_to = "year") |>
  mutate(year = as.integer(year)) |>
  pivot_wider(names_from = vnames, values_from = value) |>
  mutate(CotSS = CotSS1 + CotSS2 + CotSS3) |>
  select(-(CotSS1:CotSS3))

# IGAE data: COFOG
cofog_vars <-
  tibble(row = c(7, 15, 16, 22, 29, 39, 46, 53, 60, 67, 76),
         vname = c("G01", "G01.8", "G02", "G03", "G04", "G05",
                   "G06", "G07", "G08", "G09", "G10"))

read_cofog <- function(level) {
  read.xlsx("./data/COFOG.xlsx", level,
            colNames = TRUE,
            rows = c(6, cofog_vars$row)) |>
    mutate(vnames = cofog_vars$vname) |>
    select(-c("Código", "Grupo")) |>
    rename("2021" = "2021.(P)") |>
    pivot_longer(cols = 1:27, names_to = "year") |>
    mutate(year = as.integer(year)) |>
    pivot_wider(names_from = vnames, names_sep = "_",
                values_from = value) |>
    mutate(G01 = G01 - coalesce(G01.8, 0))  |>
    select(-G01.8) |>
    rowwise() |>
    mutate(GTot = sum(c_across(G01:G10), na.rm = TRUE))
}

gov_levels <- c("AA.PP.", "AA.CC.", "CC.AA.", "CC.LL.", "AA.SS.")
cofog <- map(set_names(gov_levels), read_cofog)

sum_cofog <- bind_rows(cofog[-1]) |>
  pivot_longer(G01:GTot) |>
  group_by(year, name) |>
  summarise(value = sum(value,  na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = name, values_from = value)
