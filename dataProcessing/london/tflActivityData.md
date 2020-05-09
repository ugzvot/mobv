---
id: litvis

elm:
  dependencies:
    gicentre/elm-vegalite: latest
    gicentre/tidy: latest
---

```elm {l=hidden}
import Tidy exposing (..)
import VegaLite exposing (..)
```

# Generating London Locality Data

Using the [TfL Santander public bicycle hire scheme](https://tfl.gov.uk/modes/cycling/santander-cycles) as a proxy for active travel presents several problems. There is, for example, uncertainty as to how representative public bicycle hire use might be of more general trends in active travel. Others relate to how best to process bicycle docking station data to produce meaningful measures of activity.

TfL release live data on the status of approximately 800 docking stations around London. Each station belongs to a 'village' describing its local neighbourhood (click on station to see others in the same village; double-click to see all stations):

```elm {l=hidden}
path : String
path =
    "https://jwolondon.github.io/mobv/data/london/"


specMap : List Spec
specMap =
    let
        riversData =
            dataFromUrl (path ++ "geo/thames.json") [ topojsonFeature "thames" ]

        parksData =
            dataFromUrl (path ++ "geo/parks.json") [ topojsonFeature "parks" ]
    in
    [ asSpec [ riversData, geoshape [ maColor "rgb(228,236,246)" ] ]
    , asSpec [ parksData, geoshape [ maColor "rgb(239,244,225)" ] ]
    ]


stationMap : String -> Spec
stationMap stationFile =
    let
        cfg =
            configure
                << configuration (coView [ vicoStroke Nothing ])

        stationData =
            dataFromUrl (path ++ stationFile) []

        sel =
            selection
                << select "legSel" seMulti [ seNearest True, seFields [ "village" ] ]

        enc =
            encoding
                << position Longitude [ pName "lon", pQuant ]
                << position Latitude [ pName "lat", pQuant ]
                << color
                    [ mSelectionCondition (selectionName "legSel")
                        [ mName "village"
                        , mNominal
                        , mLegend []
                        ]
                        [ mStr "grey" ]
                    ]
                << opacity
                    [ mSelectionCondition (selectionName "legSel")
                        [ mNum 0.8 ]
                        [ mNum 0.3 ]
                    ]
                << size
                    [ mSelectionCondition (selectionName "legSel")
                        [ mNum 48 ]
                        [ mNum 24 ]
                    ]
                << tooltips
                    [ [ tName "village", tNominal ]
                    , [ tName "name", tNominal ]
                    , [ tName "id", tQuant ]
                    ]

        specStation =
            asSpec [ sel [], enc [], circle [ maSize 48, maStroke "black", maStrokeWidth 0.6 ] ]
    in
    toVegaLite
        [ cfg []
        , stationData
        , width 800
        , height 550
        , layer (specMap ++ [ specStation ])
        ]
```

^^^elm {v=(stationMap "tflBicycleStations.csv") interactive}^^^

Groupings vary considerably in size and there is some inconsistency in geographic placement and in naming, so to reduce the number of localities, the following changes were made:

#### Naming Consistency & Abbreviation

| Old name                     | New name       |
| ---------------------------- | -------------- |
| Kings Cross                  | King's Cross   |
| Parsons Green                | Parson's Green |
| Queen Elizabeth Olympic Park | Olympic Park   |
| St. James's                  | St James's     |
| St. John's Wood              | St John's Wood |
| St.John's Wood               | St John's Wood |
| St Lukes                     | St Luke's      |
| St. Luke's                   | St Luke's      |
| St. Paul's                   | St Paul's      |
| The Borough                  | Borough        |
| The Regent's Park            | Regent's Park  |

#### Aggregation

| id  | Station                     | Old village        | New locality      |
| --- | --------------------------- | ------------------ | ----------------- |
| 5   | Sedding Street              | Sloane Square      | Knightsbridge     |
| 8   | Maida Vale                  | Maida Vale         | St John's Wood    |
| 25  | Doric Way                   | Somers Town        | Euston            |
| 27  | Bouverie Street             | Temple             | Holborn           |
| 47  | Warwick Avenue Station      | Maida Vale         | St John's Wood    |
| 79  | Arundel Street              | Temple             | Strand            |
| 80  | Webber Street               | Southwark          | Elephant & Castle |
| 104 | Crosswall                   | Tower              | Aldgate           |
| 116 | Little Argyll Street        | West End           | Soho              |
| 120 | The Guildhall               | Guildhall          | Bank              |
| 124 | Eaton Square                | Belgravia          | Knightsbridge     |
| 127 | Wood Street                 | Guildhall          | Bank              |
| 130 | Tower Gardens               | Tower              | Aldgate           |
| 140 | Finsbury Square             | Moorgate           | Liverpool Street  |
| 149 | Kennington Road Post Office | Oval               | Kennington        |
| 160 | Waterloo Place              | St James's         | West End          |
| 181 | Belgrave Square             | Belgravia          | Knightsbridge     |
| 183 | Riverlight North            | Nine Elms          | Pimlico           |
| 199 | Great Tower Street          | Monument           | Bank              |
| 203 | West Smithfield Rotunda     | Farringdon         | Barbican          |
| 207 | Grosvenor Crescent          | Belgravia          | Knightsbridge     |
| 215 | Moorfields                  | Moorgate           | Barbican          |
| 224 | Queensway                   | Kensington Gardens | Hyde Park         |
| 228 | St. James's Square          | St James's         | West End          |
| 255 | Clifton Road                | Maida Vale         | St John's Wood    |
| 259 | Bourne Street               | Belgravia          | Pimlico           |
| 276 | Lower Thames Street         | Monument           | Bank              |
| 298 | Curlew Street               | Shad Thames        | Bermondsey        |
| 302 | Putney Pier                 | Wandsworth         | Putney            |
| 307 | Black Lion Gate             | Kensington Gardens | Hyde Park         |
| 331 | Bunhill Row                 | Moorgate           | Barbican          |
| 350 | Queen's Gate                | Kensington Gardens | Hyde Park         |
| 393 | Snow Hill                   | Farringdon         | Holborn           |
| 404 | Palace Gate                 | Kensington Gardens | Hyde Park         |
| 423 | Eaton Square (South)        | Belgravia          | Knightsbridge     |
| 440 | Kennington Oval             | Oval               | Kennington        |
| 452 | St. Katharine's Way         | Tower              | Wapping           |
| 487 | Canton Street               | Poplar             | Limehouse         |
| 509 | Fore Street                 | Guildhall          | Bank              |
| 527 | Hansard Mews                | Holland Park       | Shepherd's Bush   |
| 559 | Abbotsbury Road             | Holland Park       | Kensington        |
| 566 | Westfield Ariel Way         | White City         | Avondale          |
| 587 | Monument Street             | Monument           | Bank              |
| 601 | BBC White City              | White City         | Avondale          |
| 606 | Addison Road                | Holland Park       | Ladbroke Grove    |
| 611 | Princedale Road             | Holland Park       | Ladbroke Grove    |
| 612 | Wandsworth Rd               | Isley Court        | Wandsworth Road   |
| 631 | Battersea Park Road         | Nine Elms          | Battersea Park    |
| 634 | Brook Green South           | Brook Green        | Hammersmith       |
| 637 | Spencer Park                | Wandsworth Common  | Clapham           |
| 650 | St. Mark's Road             | North Kensington   | Portobello        |
| 653 | Simpson Street              | Clapham            | Battersea         |
| 654 | Ashmole Estate              | Oval               | Kennington        |
| 675 | Usk Road                    | Clapham Junction   | Wandsworth        |
| 689 | Spanish Road                | Clapham Junction   | Wandsworth        |
| 704 | Mexfield Road               | East Putney        | Wandsworth        |
| 706 | Snowsfields                 | London Bridge      | Bermondsey        |
| 718 | Ada Street                  | Hackney Central    | Haggerston        |
| 719 | Victoria Park Road          | Hackney Central    | Victoria Park     |
| 728 | Putney Bridge Road          | East Putney        | Putney            |
| 732 | Duke Street Hill            | London Bridge      | Borough           |
| 739 | Hortensia Road              | West Brompton      | West Chelsea      |
| 748 | Hertford Road               | De Beauvoir Town   | Haggerston        |
| 757 | Harcourt Terrace            | West Brompton      | Earl's Court      |
| 768 | Clapham Common North Side   | Clapham Common     | Clapham           |
| 773 | Tallis Street               | Temple             | St Paul's         |
| 775 | Little Brook Green          | Brook Green        | Hammersmith       |
| 783 | Monier Road                 | Hackney Wick       | Olympic Park      |
| 790 | Stratford Station           | Olympic Park       | Olympic Park      |
| 797 | Ossulston Street            | Somers Town        | Euston            |
| 807 | Bevington Road West         | North Kensington   | Portobello        |
| 817 | Riverlight South            | Nine Elms          | Pimlico           |
| 838 | Fore Street Avenue          | Guildhall          | Bank              |

This reduces the number of localities to 85:

^^^elm {v=(stationMap "tflBicycleStationsWithLocalities.csv") interactive}^^^

The table of modified station localities is stored in an SQLite database:

```sql
CREATE TABLE stations(id INTEGER NOT NULL, name TEXT, village TEXT, lon REAL, lat REAL);
.mode csv
.import tflBicycleStationsWithLocalities.csv stations
```

From these we can create a set of locality centroids that we use as the basis of activity mapping, where each locality is associated with the bicycle activity of all the docking stations within it (noting the names `station_id` and `station_name` are used for consistency with other city data even though each is a collection of docking stations):

```sql
SELECT village AS station_id, village AS station_name, AVG(lat) AS lat, AVG(lon) AS lon
FROM stations
GROUP BY village;
```

```elm {v interactive siding}
localityMap : Spec
localityMap =
    let
        cfg =
            configure
                << configuration (coView [ vicoStroke Nothing ])

        localityData =
            dataFromUrl (path ++ "stationLocations-Bicycle.csv") []

        enc =
            encoding
                << position Longitude [ pName "lon", pQuant ]
                << position Latitude [ pName "lat", pQuant ]
                << color [ mName "station_name", mNominal, mLegend [] ]

        specStation =
            asSpec [ enc [], square [ maSize 60, maStroke "black", maStrokeWidth 0.6 ] ]

        encLabels =
            encoding
                << position Longitude [ pName "lon", pQuant ]
                << position Latitude [ pName "lat", pQuant ]
                << text [ tName "station_name", tNominal ]

        specLabels =
            asSpec [ encLabels [], textMark [ maDy 10, maFontSize 10 ] ]
    in
    toVegaLite
        [ cfg []
        , localityData
        , width 800
        , height 550
        , layer (specMap ++ [ specStation, specLabels ])
        ]
```

## Generating Locality Activity Values with SQLite

Bicycle activity is generated from the TfL feed that indicates the status of each docking station. We log this feed at 10 minute intervals storing the data in an SQL table:

```sql
CREATE TABLE usage(
  stationId INTEGER NOT NULL,
  availableBikes INTEGER,
  availableDocks INTEGER,
  installed INTEGER,
  locked INTEGER,
  temporary INTEGER,
  t TEXT);
```

To update with more recent data, ensure the most recent data are at the end of a day and then update from the following day onwards. For example

```sql
DELETE FROM usage WHERE t > '2020-05-05';
```

Download the current month from remote database, and add to usage:

```sql
.mode csv
.import useageSinceMay05.csv usage
```

### Join locality to usage

Because docking stations are referenced only by their id, we need to join the locality to each reading. We also create a new `day_of_year` column for temporal aggregation by truncating the timestamp of each reading:

```sql
CREATE TABLE usage2 AS
  SELECT stationId, village, availableBikes, t, substr(t,0,11) AS day_of_year
  FROM usage
  LEFT JOIN stations ON stationId = id;
```

### Calculate activity index

For each station: accumulate the number of times number of docked bikes changes in consecutive readings on a given day. Can use the SQLite [window operations](https://www.sqlite.org/windowfunctions.html) and then sum with an [aggregate function](https://www.sqlitetutorial.net/sqlite-aggregate-functions/).

Additionally we group the results by village and day of year and create an id that combines village name with day of week.

Note also that to be consistent with other datasets, we rename 'village' as 'station' even though it is an aggregation of several stations in a locality.

```sql
CREATE TABLE station_daily_time_series AS
  SELECT
    day_of_year AS date,
    village AS station,
    village||'_'||strftime('%w',day_of_year) AS id,
    SUM(activity) AS count
  FROM
    ( SELECT
        stationID,
        village,
        day_of_year,
        abs(first_value(availableBikes) OVER win - last_value(availableBikes) OVER win) AS activity
      FROM usage2 WINDOW win AS
        (PARTITION BY stationId ORDER BY t ROWS BETWEEN 1 PRECEDING AND CURRENT ROW)
    )
  GROUP BY village, day_of_year
  ORDER BY day_of_year,village;
```

### Calculate benchmark data

To calculate expected activity for each locality we take the mean activity for January grouped by day of week:

```sql
CREATE TABLE station_reference_bicycle AS
  SELECT id,AVG(count) AS count
  FROM station_daily_time_series
  WHERE date < '2020-02-01'
  GROUP BY id;
```

And then export:

```sql
.headers on
.mode csv
.output StationReference-Bicycle.csv
SELECT id,count FROM station_reference_bicycle ORDER BY id;
.quit
```

### Export station daily time series

```sql
.headers on
.mode csv
.output StationDailyTimeSeries-Bicycle.csv
SELECT date,station,id,count FROM station_daily_time_series ORDER BY date,id;
.quit
```

# Visualization

```elm {v interactive}
londonExample : Spec
londonExample =
    let
        localityData =
            dataFromUrl (path ++ "stationLocations-Bicycle.csv") []

        timeSeriesData =
            dataFromUrl (path ++ "StationDailyTimeSeries-Bicycle.csv") []

        referenceData =
            dataFromUrl (path ++ "StationReference-Bicycle.csv") []

        annotationData =
            dataFromUrl (path ++ "annotations.csv") []

        cfg =
            configure
                << configuration (coView [ vicoStroke Nothing ])

        -- Grid View
        sel =
            selection
                << select "brush" seMulti [ seEncodings [ chY ] ]

        trans =
            transform
                << lookup "id" referenceData "id" (luFields [ "value" ])
                << calculateAs "datum.value == 0 ? 0 : (datum.count - datum.value)/sqrt(datum.value)" "anomaly"
                << lookup "station" localityData "station_id" (luFields [ "station_name" ])
                << filter (fiExpr "datum.station_name != null")

        gridEnc =
            encoding
                << position X
                    [ pName "date"
                    , pTemporal
                    , pAxis
                        [ axDataCondition (expr "day(datum.value) == 6 || day(datum.value) == 0") (cAxGridOpacity 1 0)
                        , axTickCount 100
                        , axGridWidth 8
                        , axGridColor "#f6f6f6"
                        , axLabelExpr "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''"
                        , axTitle ""
                        ]
                    ]
                << position Y
                    [ pName "station_name"
                    , pNominal
                    , pSort [ soByField "count" opSum, soDescending ]
                    , pAxis [ axTitle "", axOffset 7, axDomain False, axTicks False ]
                    ]
                << color
                    [ mName "anomaly"
                    , mQuant
                    , mScale [ scScheme "blueOrange" [], scDomainMid 0 ]
                    , mLegend
                        [ leTitle "Anomaly"
                        , leDirection moHorizontal
                        , leOrient loTop
                        , leGradientThickness 8
                        ]
                    ]
                << opacity [ mSelectionCondition (selectionName "brush") [ mNum 1 ] [ mNum 0.5 ] ]
                << size [ mSelectionCondition (selectionName "brush") [ mNum 90 ] [ mNum 40 ] ]
                << tooltips
                    [ [ tName "station_name", tNominal, tTitle "locality" ]
                    , [ tName "date", tTemporal, tFormat "%a %e %b" ]
                    , [ tName "value", tQuant, tTitle "expected" ]
                    , [ tName "count", tQuant, tTitle "observed" ]
                    , [ tName "anomaly", tQuant, tFormat ".1f" ]
                    ]

        cellSpec =
            asSpec
                [ timeSeriesData
                , trans []
                , sel []
                , gridEnc []
                , square []
                ]

        leaderEnc =
            encoding
                << position X [ pName "date", pTemporal ]

        leaderSpec =
            asSpec [ leaderEnc [], rule [ maStrokeDash [ 4, 2 ], maOpacity 0.5 ] ]

        labelEnc =
            encoding
                << position X [ pName "date", pTemporal ]
                << position Y [ pNum 0 ]
                << text [ tName "notes", tNominal ]

        labelSpec =
            asSpec
                [ labelEnc []
                , textMark [ maAngle -50, maAlign haLeft, maOpacity 0.5, maFontSize 8, maDx 2 ]
                ]

        annotationSpec =
            asSpec [ annotationData, layer [ leaderSpec, labelSpec ] ]

        specGrid =
            asSpec
                [ width 800
                , heightStep 12
                , layer [ cellSpec, annotationSpec ]
                ]

        -- Time lines
        encTimeline =
            encoding
                << position X
                    [ pName "date"
                    , pTemporal
                    , pAxis
                        [ axDataCondition (expr "day(datum.value) == 6 || day(datum.value) == 0") (cAxGridOpacity 1 0)
                        , axTickCount 100
                        , axGridWidth 8
                        , axGridColor "#f6f6f6"
                        , axLabelExpr "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''"
                        , axTitle ""
                        ]
                    ]
                << position Y
                    [ pName "anomaly"
                    , pQuant
                    , pScale [ scDomain (doNums [ -50, 50 ]), scNice niFalse ]
                    , pTitle "Anomaly"
                    ]
                << color
                    [ mSelectionCondition (selectionName "brush")
                        [ mName "station_name"
                        , mNominal
                        , mLegend []
                        , mSort [ soByField "count" opSum, soDescending ]
                        ]
                        [ mStr "black" ]
                    ]
                << opacity [ mSelectionCondition (selectionName "brush") [ mNum 1 ] [ mNum 0.2 ] ]
                << size [ mSelectionCondition (selectionName "brush") [ mNum 1 ] [ mNum 0.3 ] ]

        lineSpec =
            asSpec
                [ timeSeriesData
                , trans []
                , sel []
                , encTimeline []
                , line [ maInterpolate miMonotone, maClip True ]
                ]

        ruleData =
            dataFromColumns []
                << dataColumn "origin" (nums [ 0 ])

        ruleEnc =
            encoding
                << position Y [ pName "origin", pQuant ]

        ruleSpec =
            asSpec [ ruleData [], ruleEnc [], rule [] ]

        leader2Spec =
            asSpec [ annotationData, leaderEnc [], rule [ maStrokeDash [ 4, 2 ], maOpacity 0.5 ] ]

        timelineSpec =
            asSpec
                [ width 800
                , height 400
                , layer [ lineSpec, ruleSpec, leader2Spec ]
                ]

        specTimeCharts =
            asSpec [ columns 1, concat [ specGrid, timelineSpec ] ]

        -- Map
        transStations =
            transform
                << filter (fiExpr "datum.lon != 0 || datum.lat != 0")
                << filter (fiSelection "brush")

        encStations =
            encoding
                << position Longitude [ pName "lon", pQuant ]
                << position Latitude [ pName "lat", pQuant ]

        specStations =
            asSpec [ localityData, encStations [], transStations [], circle [] ]

        encStationLabels =
            encStations
                << text [ tName "station_name", tNominal ]

        specStationLabels =
            asSpec
                [ localityData
                , transStations []
                , encStationLabels []
                , textMark [ maAlign haLeft, maDx 4, maOpacity 0.3, maFontSize 8 ]
                ]
    in
    toVegaLite
        [ cfg []
        , spacing 0
        , padding (paEdges 10 0 0 0)
        , vConcat
            [ specGrid
            , timelineSpec
            , asSpec [ width 800, height 650, layer (specMap ++ [ specStations, specStationLabels ]) ]
            ]
        ]
```