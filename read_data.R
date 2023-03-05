library(tidyverse)
library(openxlsx)

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




cofog_vars <-
  tibble(row = c(7, 15, 16, 22, 29, 39, 46, 53, 60, 67, 76),
         vname = c("G01", "G01.8", "G02", "G03", "G04", "G05",
                   "G06", "G07", "G08", "G09", "G10"))
cofog <- local({
  df_list <- list(
    read.xlsx("./data/COFOG.xlsx", "AA.CC.",
              colNames = TRUE,
              rows = c(6, cofog_vars$row)) |>
      mutate(level = "AC", vnames = cofog_vars$vname),
    read.xlsx("./data/COFOG.xlsx", "CC.AA.",
              colNames = TRUE,
              rows = c(6, cofog_vars$row)) |>
      mutate(level = "CA", vnames = cofog_vars$vname),
    read.xlsx("./data/COFOG.xlsx", "CC.LL.",
              colNames = TRUE,
              rows = c(6, cofog_vars$row)) |>
      mutate(level = "CL", vnames = cofog_vars$vname),
    read.xlsx("./data/COFOG.xlsx", "AA.SS.",
              colNames = TRUE,
              rows = c(6, cofog_vars$row)) |>
      mutate(level = "SS", vnames = cofog_vars$vname))

  bind_rows(df_list)
})|>
  select(-c("Código", "Grupo")) |>
  rename("2021" = "2021.(P)") |>
  pivot_longer(cols = 1:27, names_to = "year") |>
  mutate(year = as.integer(year)) |>
  pivot_wider(names_from = c(vnames, level), names_sep = "_",
              values_from = value)

