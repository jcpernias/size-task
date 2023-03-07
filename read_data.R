library(tidyverse)
library(openxlsx)

# IGAE data: Total government

aapp <- local({
  aapp_vars <-
    tibble(row = c(7, 36, 43, 63, 64, 83),
           vname = c("Ingresos", "Gastos", "Intereses",
                     "SFiscal", "SPrimario", "PIB"))
  df_list <- list(
    read.xlsx("./data/A_AAPP.xlsx", "Tabla1a",
              colNames = TRUE,
              rows = c(6, aapp_vars$row[-nrow(aapp_vars)])),
    read.xlsx("./data/A_AAPP.xlsx", "Tabla1b",
              colNames = TRUE,
              rows = c(6, aapp_vars$row[nrow(aapp_vars)])) |>
      mutate(CONCEPTOS  = as.character(CONCEPTOS)))

  bind_rows(df_list)  |>
    select(-c("CÓDIGO", "CONCEPTOS")) |>
    mutate(vnames = aapp_vars$vname) |>
    rename("2021" = "2021(P)") |>
    pivot_longer(cols = 1:27, names_to = "year") |>
    mutate(year = as.integer(year)) |>
    pivot_wider(names_from = vnames, values_from = value)
})

# IGAE data: Taxes
imp <- local({
  imp_vars <-
    tibble(row = c(7, 74, 86, 95, 96, 97, 98),
           vname = c("TInd", "TRenta", "TCap", "TTotal",
                     "CotSS1", "CotSS2", "CotSS3"))

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
})

# IGAE data: COFOG
read_cofog <- function(level) {
  cofog_vars <-
    tibble(row = c(7, 15, 16, 22, 29, 39, 46, 53, 60, 67, 76),
           vname = c("G01", "G01.8", "G02", "G03", "G04", "G05",
                     "G06", "G07", "G08", "G09", "G10"))

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


library(RColorBrewer)
theme_set(theme_bw())

theme_update(panel.border = element_blank(),
             axis.line.x = element_line(linewidth = 0.2),
             axis.line.y = element_line(linewidth = 0.2),
             text = element_text(size = 10))

line_plot <- function(data, labels = NULL) {
  lplot <- ggplot(data, aes(x = year, y = value, color = name))  +
    geom_line() + geom_point() +
    labs(color = NULL) +
    xlab("") + ylab("") +
    scale_x_continuous(breaks = seq(1995, 2020, 5),
                       minor_breaks = NULL)

  if (is.null(labels)) {
    lplot <- lplot + scale_color_brewer(palette = "Set2")
  } else {
    lplot <- lplot +
      scale_color_brewer(palette = "Set2", labels = labels)
  }

  if (length(unique(data$name)) == 1) {
    lplot <- lplot + theme(legend.position = "none")
  }
  lplot
}

## a) Gasto público
aapp |> mutate(G = Gastos / PIB * 100) |>
  select(year, G) |>
  pivot_longer(G) |>
  line_plot()


## d) Ingresos públicos
aapp |> mutate(R = Ingresos / PIB * 100) |>
  select(year, R) |>
  pivot_longer(R) |>
  line_plot()


left_join(aapp, imp, by = "year") |>
  mutate(T = (TTotal + CotSS) / Ingresos * 100) |>
  select(year, T) |>
  pivot_longer(T) |>
  line_plot()


left_join(aapp, imp, by = "year") |>
  mutate(PF = (TTotal + CotSS) / PIB * 100) |>
  select(year, PF) |>
  pivot_longer(PF) |>
  line_plot()

## e) Impuestos directos, indirectos y cotizaciones
left_join(aapp, imp, by = "year") |>
  mutate(TI = TInd / PIB * 100,
         TD = (TRenta + TCap) / PIB * 100,
         SS = CotSS / PIB * 100) |>
  select(year, TI, TD, SS) |>
  pivot_longer(c(TI, TD, SS)) |>
  line_plot(labels = c(TI = "Impuestos Indirectos",
                       TD = "Impuestos Directos",
                       SS = "Cotizaciones S.S.")) +
  theme(legend.position = "bottom")



imp |> mutate(Total = TTotal + CotSS,
              TI = TInd / Total * 100,
              TD = (TRenta + TCap) / Total * 100,
              SS = CotSS / Total * 100) |>
  select(year, TI, TD, SS) |>
  pivot_longer(c(TI, TD, SS)) |>
  line_plot(labels = c(TI = "Impuestos Indirectos",
                       TD = "Impuestos Directos",
                       SS = "Cotizaciones S.S.")) +
  theme(legend.position = "bottom")


## f) Saldo fiscal y saldo primario
aapp |>
  mutate(SF = SFiscal / PIB * 100,
         SP = SPrimario / PIB * 100) |>
  select(year, SF, SP) |>
  pivot_longer(c(SF, SP)) |>
  line_plot(labels = c(SF = "Saldo Fiscal",
                       SP = "Saldo Primario")) +
  theme(legend.position = "bottom")



