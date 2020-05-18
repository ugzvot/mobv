library(magrittr)
library(readr)
library(dplyr)
library(rlang)
library(sp)
library(tibble)


transform_crs <- function(data, coord, in_crs, out_crs, append = TRUE) {
  out_cols <- rlang::names2(coord)
  if (any(out_cols == "") || length(out_cols) != 2) {
    stop("coord must be a named character vector with 2 items")
  }

  if (isTRUE(append)) {
    data <- dplyr::bind_cols(data, dplyr::select(data, !!!coord))
  } else {
    data <- dplyr::rename(data, !!!coord)
  }
  sp::coordinates(data) <- out_cols
  sp::proj4string(data) <- in_crs
  data <- sp::spTransform(data, out_crs)
  tibble::as_tibble(data)
}

transform_LV95_to_WSG84 <- function(data, coord = c(lon = "x", lat = "y"), append = TRUE) {
  transform_crs(data, coord, sp::CRS("+init=epsg:2056"), sp::CRS("+init=epsg:4326"), append)
}

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

meta_cols <- c("ZSID", "ZSName", "MSID", "Achse", "HNr", "Hoehe", "Richtung", "NKoord", "EKoord",
  "Knummer", "Kname", "AnzDetektoren", "D1ID", "D2ID", "D3ID", "D4ID")

# The metadata is per direction not per road
# At the moment just take the first for each ZSID and we get one per road.
# As improvement we could calculate the total number of lanes (AnzDetektoren) per Road
# and build an extensive description
df_meta <- dplyr::select(data_2020, dplyr::one_of(meta_cols)) %>%
  dplyr::distinct(!!!rlang::syms(meta_cols)) %>%
  dplyr::group_by(.data$ZSID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  transform_LV95_to_WSG84(coord = c(lon = "EKoord", lat = "NKoord")) %>%
  dplyr::mutate_if(is.double, ~ round(., digits = 6))

# rename columns to data specifications
df_meta <- dplyr::transmute(df_meta,
  station_id = .data$ZSID,
  station_name = .data$ZSName,
  description = "",
  lat = .data$lat,
  lon= .data$lon,
)

readr::write_csv(df_meta, "../../data/zurich/StationLocations-miv.csv")


