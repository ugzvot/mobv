library(here)
library(magrittr)
library(readr)
library(dplyr)
library(lubridate)


# actually the time zone used in the data is CET (after analysing some diurnal data), but
# the data contains data points for 2020-03-29 02:00:00, which doesn't exists in CET.
# Parsing with Etc/GMT-1 works fine for daily sum.
data_2020 <- readr::read_csv(
  "https://data.stadt-zuerich.ch/dataset/6212fd20-e816-4828-a67f-90f057f25ddb/resource/44607195-a2ad-4f9b-b6f1-d26c003d85a2/download/sid_dav_verkehrszaehlung_miv_od2031_2020.csv",
  col_types = readr::cols(
    MessungDatZeit = readr::col_datetime(format = "%Y-%m-%dT%H:%M:%S"),
    LieferDat = readr::col_date(format = "%Y-%m-%d"),
    AnzFahrzeuge = readr::col_double(),
    HNr = readr::col_integer(),
    EKoord = readr::col_double(),
    NKoord = readr::col_double(),
    Knummer = readr::col_integer(),
    AnzDetektoren = readr::col_integer(),
    D1ID = readr::col_integer(),
    D2ID = readr::col_integer(),
    D3ID = readr::col_integer(),
    D4ID = readr::col_integer(),
    .default = readr::col_character()
  ),
  locale = locale(tz = "Etc/GMT-1"),
  na = c("", "NA", "Unbekannt")
)



data_2020 <- dplyr::transmute(data_2020,
  station = .data$ZSID,
  date = lubridate::date(.data$MessungDatZeit),
  count = .data$AnzFahrzeuge
)

# calculate count and data availability per day
data_2020 <- dplyr::group_by(data_2020, .data$station, .data$date) %>%
  dplyr::summarise(
    n_records = sum(!is.na(.data$count)) /  dplyr::n(),  # data availability over the day,
    count = sum(.data$count, na.rm = TRUE)
  )

# filter days with less then 21 hour data availability per day
data_2020 <- dplyr::filter(data_2020, .data$n_records >= 21 / 24)

.data <- data_2020

data_2020 <- dplyr::ungroup(data_2020)

# id = combination of station and weekday
data_2020 <- dplyr::transmute(data_2020,
  date = .data$date,
  station = .data$station,
  id = stringr::str_c(.data$station, "_", lubridate::wday(.data$date, label = FALSE, week_start = 1)),
  count = .data$count
)

readr::write_csv(data_2020, "../../data/zurich/StationDailyTimeSeries-miv.csv", na="")

# Compute and save baselines:
# Google: "The baseline is the median value, for the corresponding day of the
# week, during the 5-week period Jan 3–Feb 6, 2020.

data_2020 %>%
  dplyr::filter(
    .data$date >= lubridate::ymd("2020-01-03"),
    .data$date <= lubridate::ymd("2020-02-06")
  ) %>%
  dplyr::group_by(.data$id) %>%
  dplyr::summarise(value = median(.data$count, na.rm = TRUE)) %>%
  readr::write_csv("../../data/zurich/StationReference-miv.csv", na="")
