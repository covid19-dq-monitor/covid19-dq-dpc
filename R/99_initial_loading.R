# inital loading

# 01_prepare_data
# TODO: Add cols()
# TODO: raw - delta - trend
cols <- cols(
  .default = col_double(),
  data = col_datetime(format = ""),
  stato = col_character(),
  codice_regione = col_character(),
  denominazione_regione = col_character(),
  casi_da_sospetto_diagnostico = col_integer(),
  casi_da_screening = col_integer(),
  casi_testati = col_integer(),
  note = col_character()
)
regioni_raw <-
  readr::read_csv(
    'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv',
    col_types = cols
  )
today <- max(regioni_raw$data)

#for raw
raw <- regioni_raw %>%
  filter(data == today)
# for delta
delta <- regioni_raw %>%
  filter(between(data, as.Date("2020-06-30"), today)) %>%
  arrange(denominazione_regione, data) %>%
  group_by(denominazione_regione) %>%
  mutate(
    delta_ricoverati = ricoverati_con_sintomi - lag(ricoverati_con_sintomi),
    delta_terapia_intensiva = terapia_intensiva - lag(terapia_intensiva),
    delta_ospedalizzati = totale_ospedalizzati - lag(totale_ospedalizzati),
    delta_isolamento_domiciliare = isolamento_domiciliare - lag(isolamento_domiciliare),
    delta_casi_testati = casi_testati - lag(casi_testati),
    delta_deceduti = deceduti - lag(deceduti),
    delta_casi_screening = casi_da_screening - lag(casi_da_screening),
    delta_casi_sospetto = casi_da_sospetto_diagnostico - lag(casi_da_sospetto_diagnostico),
    delta_totale_positivi = totale_positivi - lag(totale_positivi),
    delta_dimessi_guariti = dimessi_guariti - lag(dimessi_guariti),
    delta_totale_casi = totale_casi - lag(totale_casi),
    delta_tamponi = tamponi - lag(tamponi)
  ) %>%
  select(
    data,
    denominazione_regione,
    ricoverati_con_sintomi,
    terapia_intensiva,
    totale_ospedalizzati,
    starts_with("delta")
  )

summaries <- delta %>%
  group_by(denominazione_regione) %>%
  summarise(
    max_guariti = median(delta_dimessi_guariti),
    max_deceduti = median(delta_deceduti)
  )

consistProv <-
  readr::read_csv(
    'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv'
  ) %>%
  group_by(data, denominazione_regione) %>%
  summarise(totale_casi = sum(totale_casi)) %>%
  ungroup() %>%
  arrange(denominazione_regione, data) %>%
  group_by(denominazione_regione) %>%
  mutate(delta_totale_casi_cons = totale_casi - lag(totale_casi)) %>%
  filter(between(data, today - as.difftime(14, unit = "days"), today)) %>%
  select(data, denominazione_regione, delta_totale_casi_cons)

delta <-
  left_join(delta, consistProv, by = c("data", "denominazione_regione"))

# 02_apply_checks
# read rules from yaml

rules_delta <- validator(.file = "rules.yaml")

for (d in unique(delta$data)[2:length(unique(delta$data))]) {
  delta_day <- filter(delta, data == d)
  results_delta <- (confront(delta_day, rules_delta))
  fails_day <- filter(summary(results_delta), fails > 0) %>%
    select(name) %>%
    mutate(date = d)
  if(nrow(fails_day > 0)){
  vec_len <- rep(21, times = length(fails_day$name))
  vec_key <- rep(fails_day$name, times = vec_len)
  x <- values(results_delta)[[1]][, fails_day$name] %>%
    as.data.frame() %>%
    gather() %>%
    mutate(
      date = as.Date(as.POSIXct(d, origin = "1970-01-01", tz = "GMT"),
      region = rep(delta_day$denominazione_regione,
                   times = length(fails_day$name)),
      key = vec_key
    ) %>%
    filter(value == FALSE) %>%
    select(date, failed_check = key, region)
  }
  if(d == unique(delta$data)[2]){
    append <- x
  } else {
    append <- union(append, x)
  }
  if(as.Date(as.POSIXct(d,origin = "1970-01-01", tz= "GMT")) == Sys.Date()){
    latest <- x
  }
}
 append <- append %>% 
	 readr::write_csv("output/failed-checks-history.csv")
 latest %>% write.csv("output/failed-checks-latest.csv")
 
