// DATA SOURCES

let path = "https://ugzvot.github.io/mobv/data/zurich/";

let bicycleStationData = path + "StationLocations-Bicycle.csv";
let bicycleTimeSeriesData = path + "StationDailyTimeSeries-Bicycle.csv";
let bicycleReferenceData = path + "StationReference-Bicycle.csv";

let footStationData = path + "StationLocations-Foot.csv";
let footTimeSeriesData = path + "StationDailyTimeSeries-Foot.csv";
let footReferenceData = path + "StationReference-Foot.csv";

let mivStationData = path + "StationLocations-miv.csv";
let mivTimeSeriesData = path + "StationDailyTimeSeries-miv.csv";
let mivReferenceData = path + "StationReference-miv.csv";

let annotationsData = path + "annotations.csv";

let blueLines = path + "geo/zurichRivers.json";
let blueLineFeature = "zurichLines";
let bluePolys = path + "geo/zurichLakes.json";
let bluePolyFeature = "zurichPolys";
let greenPolys = path + "geo/zurichGreen.json";
let greenPolyFeature = "zurichPolys";

// CHART SCALING

let bicycleAnomalyMax = 120;
let bicycleAnomalyMin = -120;
let footAnomalyMax = 140;
let footAnomalyMin = -140;
let mivAnomalyMax = 140;
let mivAnomalyMin = -140;



// -----------------------------------------------------------------------------

// BICYCLE LINKED VIEWS CHART

let vlSpecLinkedBicycle = {
  $schema: "https://vega.github.io/schema/vega-lite/v4.json",
  config: {
    view: {
      stroke: "",
    },
  },
  spacing: 0,
  padding: {
    left: 10,
    top: 0,
    right: 0,
    bottom: 0,
  },
  data: { url: `${bicycleTimeSeriesData}` },
  transform: [
    {
      lookup: "id",
      from: {
        data: { url: `${bicycleReferenceData}` },
        key: "id",
        fields: ["value"],
      },
    },
    {
      calculate:
        "datum.value == 0 ? 0 : (datum.count - datum.value)/sqrt(datum.value)",
      as: "anomaly",
    },
    {
      lookup: "station",
      from: {
        data: { url: `${bicycleStationData}` },
        key: "station_id",
        fields: ["station_name"],
      },
    },
    {
      filter: "datum.station_name != null",
    },
  ],
  hconcat: [
    {
      vconcat: [
        {
          width: 800,
          height: {
            step: 12,
          },
          layer: [
            {
              selection: {
                brush: {
                  type: "multi",
                  fields: ["station_name"],
                },
              },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                  axis: {
                    gridOpacity: {
                      condition: {
                        test: "day(datum.value) == 6 || day(datum.value) == 0",
                        value: 1,
                      },
                      value: 0,
                    },
                    tickSize: {
                      condition: {
                        test: "day(datum.value) == 1",
                        value: 6,
                      },
                      value: 3,
                    },
                    tickCount: 100,
                    gridWidth: 8,
                    gridColor: "#f6f6f6",
                    labelExpr:
                      "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''",
                    title: "",
                    labelFont: "Roboto Condensed",
                  },
                },
                y: {
                  field: "station_name",
                  type: "nominal",
                  sort: {
                    field: "count",
                    op: "mean",
                    order: "descending",
                  },
                  axis: {
                    title: "",
                    offset: 7,
                    domain: false,
                    ticks: false,
                    labelFont: "Roboto Condensed",
                  },
                },
                color: {
                  field: "anomaly",
                  type: "quantitative",
                  scale: {
                    scheme: "blueOrange",
                    domainMid: 0,
                  },
                  legend: {
                    title: "Anomaly",
                    direction: "horizontal",
                    orient: "top",
                    gradientThickness: 8,
                  },
                },
                opacity: {
                  condition: {
                    selection: "brush",
                    value: 1,
                  },
                  value: 0.5,
                },
                size: {
                  condition: {
                    selection: "brush",
                    value: 90,
                  },
                  value: 40,
                },
                tooltip: [
                  {
                    field: "station_name",
                    type: "nominal",
                    title: "locality",
                  },
                  {
                    field: "date",
                    type: "temporal",
                    format: "%a %e %b",
                  },
                  {
                    field: "value",
                    type: "quantitative",
                    title: "expected",
                    format: ".0f",
                  },
                  {
                    field: "count",
                    type: "quantitative",
                    title: "observed",
                  },
                  {
                    field: "anomaly",
                    type: "quantitative",
                    format: ".1f",
                  },
                ],
              },
              mark: "square",
            },
            {
              data: { url: `${annotationsData}` },
              layer: [
                {
                  encoding: {
                    x: {
                      field: "date",
                      type: "temporal",
                    },
                  },
                  mark: {
                    type: "rule",
                    strokeDash: [4, 2],
                    opacity: 0.5,
                  },
                },
                {
                  encoding: {
                    x: {
                      field: "date",
                      type: "temporal",
                    },
                    y: {
                      value: 0,
                    },
                    text: {
                      field: "notes",
                      type: "nominal",
                    },
                  },
                  mark: {
                    type: "text",
                    angle: 310,
                    align: "left",
                    opacity: 0.5,
                    fontSize: 8,
                    font: "Roboto Condensed",
                    dx: 2,
                  },
                },
              ],
            },
          ],
        },
        {
          width: 800,
          height: 400,
          layer: [
            {
              selection: {
                brush: {
                  type: "multi",
                  fields: ["station_name"],
                },
              },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                  axis: {
                    gridOpacity: {
                      condition: {
                        test: "day(datum.value) == 6 || day(datum.value) == 0",
                        value: 1,
                      },
                      value: 0,
                    },
                    tickSize: {
                      condition: {
                        test: "day(datum.value) == 1",
                        value: 6,
                      },
                      value: 3,
                    },
                    tickCount: 100,
                    gridWidth: 8,
                    gridColor: "#f6f6f6",
                    labelExpr:
                      "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''",
                    title: "",
                    labelFont: "Roboto Condensed",
                  },
                },
                y: {
                  field: "anomaly",
                  type: "quantitative",
                  scale: {
                    domain: [`${bicycleAnomalyMin}`, `${bicycleAnomalyMax}`],
                    nice: false,
                  },
                  title: "Anomaly",
                  labelFont: "Roboto Condensed",
                },
                color: {
                  condition: {
                    selection: "brush",
                    field: "station_name",
                    type: "nominal",
                    legend: null,
                  },
                  value: "black",
                },
                opacity: {
                  condition: {
                    selection: "brush",
                    value: 1,
                  },
                  value: 0.1,
                },
                size: {
                  condition: {
                    selection: "brush",
                    value: 2,
                  },
                  value: 0.2,
                },
              },
              mark: {
                type: "line",
                interpolate: "monotone",
                clip: true,
              },
            },
            {
              data: {
                values: [
                  {
                    origin: 0,
                  },
                ],
              },
              encoding: {
                y: {
                  field: "origin",
                  type: "quantitative",
                },
              },
              mark: "rule",
            },
            {
              data: { url: `${annotationsData}` },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                },
              },
              mark: {
                type: "rule",
                strokeDash: [4, 2],
                opacity: 0.5,
              },
            },
          ],
        },
      ],
    },
    {
      width: 400,
      height: 650,
      transform: [
        {
          aggregate: [
            {
              op: "count",
              field: "station_name",
              as: "numReadings",
            },
          ],
          groupby: ["station_name"],
        },
        {
          lookup: "station_name",
          from: {
            data: { url: `${bicycleStationData}` },
            key: "station_name",
            fields: ["lon", "lat"],
          },
        },
      ],
      layer: [
        {
          layer: [
            {
              data: {
                url: `${greenPolys}`,
                format: {
                  type: "topojson",
                  feature: `${greenPolyFeature}`,
                },
              },
              mark: {
                type: "geoshape",
                color: "rgb(239,244,225)",
              },
            },
            {
              data: {
                url: `${blueLines}`,
                format: { type: "topojson", feature: `${blueLineFeature}` },
              },

              mark: {
                type: "geoshape",
                color: "rgb(226,237,246)",
                strokeWidth: 4,
                filled: false,
              },
            },
            {
              data: {
                url: `${bluePolys}`,
                format: { type: "topojson", feature: `${bluePolyFeature}` },
              },
              mark: {
                type: "geoshape",
                color: "rgb(226,237,246)",
              },
            },
          ],
        },
        {
          selection: {
            brush: {
              type: "multi",
              fields: ["station_name"],
            },
          },
          encoding: {
            longitude: {
              field: "lon",
              type: "quantitative",
            },
            latitude: {
              field: "lat",
              type: "quantitative",
            },
            color: {
              field: "station_name",
              type: "nominal",
              legend: null,
            },
            opacity: {
              condition: {
                selection: "brush",
                value: 1,
              },
              value: 0.1,
            },
          },
          mark: {
            type: "square",
            size: 80,
          },
        },
        {
          encoding: {
            longitude: {
              field: "lon",
              type: "quantitative",
            },
            latitude: {
              field: "lat",
              type: "quantitative",
            },
            text: {
              field: "station_name",
              type: "nominal",
            },
            opacity: {
              condition: {
                selection: "brush",
                value: 1,
              },
              value: 0.1,
            },
          },
          mark: {
            type: "text",
            dy: 10,
            fontSize: 6,
            font: "Roboto Condensed",
          },
        },
      ],
    },
  ],
};

// -----------------------------------------------------------------------------

// FOOT LINKED VIEWS CHART

let vlSpecLinkedFoot = {
  $schema: "https://vega.github.io/schema/vega-lite/v4.json",
  config: {
    view: {
      stroke: "",
    },
  },
  spacing: 0,
  padding: {
    left: 10,
    top: 0,
    right: 0,
    bottom: 0,
  },
  data: { url: `${footTimeSeriesData}` },
  transform: [
    {
      lookup: "id",
      from: {
        data: { url: `${footReferenceData}` },
        key: "id",
        fields: ["value"],
      },
    },
    {
      calculate:
        "datum.value == 0 ? 0 : (datum.count - datum.value)/sqrt(datum.value)",
      as: "anomaly",
    },
    {
      lookup: "station",
      from: {
        data: { url: `${footStationData}` },
        key: "station_id",
        fields: ["station_name"],
      },
    },
    {
      filter: "datum.station_name != null",
    },
  ],
  hconcat: [
    {
      vconcat: [
        {
          width: 800,
          height: {
            step: 12,
          },
          layer: [
            {
              selection: {
                brush: {
                  type: "multi",
                  fields: ["station_name"],
                },
              },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                  axis: {
                    gridOpacity: {
                      condition: {
                        test: "day(datum.value) == 6 || day(datum.value) == 0",
                        value: 1,
                      },
                      value: 0,
                    },
                    tickSize: {
                      condition: {
                        test: "day(datum.value) == 1",
                        value: 6,
                      },
                      value: 3,
                    },
                    tickCount: 100,
                    gridWidth: 8,
                    gridColor: "#f6f6f6",
                    labelExpr:
                      "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''",
                    title: "",
                    labelFont: "Roboto Condensed",
                  },
                },
                y: {
                  field: "station_name",
                  type: "nominal",
                  sort: {
                    field: "count",
                    op: "mean",
                    order: "descending",
                  },
                  axis: {
                    title: "",
                    offset: 7,
                    domain: false,
                    ticks: false,
                    labelFont: "Roboto Condensed",
                  },
                },
                color: {
                  field: "anomaly",
                  type: "quantitative",
                  scale: {
                    scheme: "blueOrange",
                    domainMid: 0,
                  },
                  legend: {
                    title: "Anomaly",
                    direction: "horizontal",
                    orient: "top",
                    gradientThickness: 8,
                  },
                },
                opacity: {
                  condition: {
                    selection: "brush",
                    value: 1,
                  },
                  value: 0.5,
                },
                size: {
                  condition: {
                    selection: "brush",
                    value: 90,
                  },
                  value: 40,
                },
                tooltip: [
                  {
                    field: "station_name",
                    type: "nominal",
                    title: "locality",
                  },
                  {
                    field: "date",
                    type: "temporal",
                    format: "%a %e %b",
                  },
                  {
                    field: "value",
                    type: "quantitative",
                    title: "expected",
                    format: ".0f",
                  },
                  {
                    field: "count",
                    type: "quantitative",
                    title: "observed",
                  },
                  {
                    field: "anomaly",
                    type: "quantitative",
                    format: ".1f",
                  },
                ],
              },
              mark: "square",
            },
            {
              data: { url: `${annotationsData}` },
              layer: [
                {
                  encoding: {
                    x: {
                      field: "date",
                      type: "temporal",
                    },
                  },
                  mark: {
                    type: "rule",
                    strokeDash: [4, 2],
                    opacity: 0.5,
                  },
                },
                {
                  encoding: {
                    x: {
                      field: "date",
                      type: "temporal",
                    },
                    y: {
                      value: 0,
                    },
                    text: {
                      field: "notes",
                      type: "nominal",
                    },
                  },
                  mark: {
                    type: "text",
                    angle: 310,
                    align: "left",
                    opacity: 0.5,
                    fontSize: 8,
                    font: "Roboto Condensed",
                    dx: 2,
                  },
                },
              ],
            },
          ],
        },
        {
          width: 800,
          height: 400,
          layer: [
            {
              selection: {
                brush: {
                  type: "multi",
                  fields: ["station_name"],
                },
              },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                  axis: {
                    gridOpacity: {
                      condition: {
                        test: "day(datum.value) == 6 || day(datum.value) == 0",
                        value: 1,
                      },
                      value: 0,
                    },
                    tickSize: {
                      condition: {
                        test: "day(datum.value) == 1",
                        value: 6,
                      },
                      value: 3,
                    },
                    tickCount: 100,
                    gridWidth: 8,
                    gridColor: "#f6f6f6",
                    labelExpr:
                      "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''",
                    title: "",
                    labelFont: "Roboto Condensed",
                  },
                },
                y: {
                  field: "anomaly",
                  type: "quantitative",
                  scale: {
                    domain: [`${footAnomalyMin}`, `${footAnomalyMax}`],
                    nice: false,
                  },
                  title: "Anomaly",
                  labelFont: "Roboto Condensed",
                },
                color: {
                  condition: {
                    selection: "brush",
                    field: "station_name",
                    type: "nominal",
                    legend: null,
                  },
                  value: "black",
                },
                opacity: {
                  condition: {
                    selection: "brush",
                    value: 1,
                  },
                  value: 0.1,
                },
                size: {
                  condition: {
                    selection: "brush",
                    value: 2,
                  },
                  value: 0.2,
                },
              },
              mark: {
                type: "line",
                interpolate: "monotone",
                clip: true,
              },
            },
            {
              data: {
                values: [
                  {
                    origin: 0,
                  },
                ],
              },
              encoding: {
                y: {
                  field: "origin",
                  type: "quantitative",
                },
              },
              mark: "rule",
            },
            {
              data: { url: `${annotationsData}` },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                },
              },
              mark: {
                type: "rule",
                strokeDash: [4, 2],
                opacity: 0.5,
              },
            },
          ],
        },
      ],
    },
    {
      width: 400,
      height: 650,
      transform: [
        {
          aggregate: [
            {
              op: "count",
              field: "station_name",
              as: "numReadings",
            },
          ],
          groupby: ["station_name"],
        },
        {
          lookup: "station_name",
          from: {
            data: { url: `${footStationData}` },
            key: "station_name",
            fields: ["lon", "lat"],
          },
        },
      ],
      layer: [
        {
          layer: [
            {
              data: {
                url: `${greenPolys}`,
                format: {
                  type: "topojson",
                  feature: `${greenPolyFeature}`,
                },
              },
              mark: {
                type: "geoshape",
                color: "rgb(239,244,225)",
              },
            },
            {
              data: {
                url: `${blueLines}`,
                format: { type: "topojson", feature: `${blueLineFeature}` },
              },

              mark: {
                type: "geoshape",
                color: "rgb(226,237,246)",
                strokeWidth: 4,
                filled: false,
              },
            },
            {
              data: {
                url: `${bluePolys}`,
                format: { type: "topojson", feature: `${bluePolyFeature}` },
              },
              mark: {
                type: "geoshape",
                color: "rgb(226,237,246)",
              },
            },
          ],
        },
        {
          selection: {
            brush: {
              type: "multi",
              fields: ["station_name"],
            },
          },
          encoding: {
            longitude: {
              field: "lon",
              type: "quantitative",
            },
            latitude: {
              field: "lat",
              type: "quantitative",
            },
            color: {
              field: "station_name",
              type: "nominal",
              legend: null,
            },
            opacity: {
              condition: {
                selection: "brush",
                value: 1,
              },
              value: 0.1,
            },
          },
          mark: {
            type: "square",
            size: 80,
          },
        },
        {
          encoding: {
            longitude: {
              field: "lon",
              type: "quantitative",
            },
            latitude: {
              field: "lat",
              type: "quantitative",
            },
            text: {
              field: "station_name",
              type: "nominal",
            },
            opacity: {
              condition: {
                selection: "brush",
                value: 1,
              },
              value: 0.1,
            },
          },
          mark: {
            type: "text",
            dy: 10,
            fontSize: 6,
            font: "Roboto Condensed",
          },
        },
      ],
    },
  ],
};

// -----------------------------------------------------------------------------


// MIV LINKED VIEWS CHART

let vlSpecLinkedMiv = {
  $schema: "https://vega.github.io/schema/vega-lite/v4.json",
  config: {
    view: {
      stroke: "",
    },
  },
  spacing: 0,
  padding: {
    left: 10,
    top: 0,
    right: 0,
    bottom: 0,
  },
  data: { url: `${mivTimeSeriesData}` },
  transform: [
    {
      lookup: "id",
      from: {
        data: { url: `${mivReferenceData}` },
        key: "id",
        fields: ["value"],
      },
    },
    {
      calculate:
        "datum.value == 0 ? 0 : (datum.count - datum.value)/sqrt(datum.value)",
      as: "anomaly",
    },
    {
      lookup: "station",
      from: {
        data: { url: `${mivStationData}` },
        key: "station_id",
        fields: ["station_name"],
      },
    },
    {
      filter: "datum.station_name != null",
    },
  ],
  hconcat: [
    {
      vconcat: [
        {
          width: 800,
          height: {
            step: 12,
          },
          layer: [
            {
              selection: {
                brush: {
                  type: "multi",
                  fields: ["station_name"],
                },
              },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                  axis: {
                    gridOpacity: {
                      condition: {
                        test: "day(datum.value) == 6 || day(datum.value) == 0",
                        value: 1,
                      },
                      value: 0,
                    },
                    tickSize: {
                      condition: {
                        test: "day(datum.value) == 1",
                        value: 6,
                      },
                      value: 3,
                    },
                    tickCount: 100,
                    gridWidth: 8,
                    gridColor: "#f6f6f6",
                    labelExpr:
                      "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''",
                    title: "",
                    labelFont: "Roboto Condensed",
                  },
                },
                y: {
                  field: "station_name",
                  type: "nominal",
                  sort: {
                    field: "count",
                    op: "mean",
                    order: "descending",
                  },
                  axis: {
                    title: "",
                    offset: 7,
                    domain: false,
                    ticks: false,
                    labelFont: "Roboto Condensed",
                  },
                },
                color: {
                  field: "anomaly",
                  type: "quantitative",
                  scale: {
                    scheme: "blueOrange",
                    domainMid: 0,
                  },
                  legend: {
                    title: "Anomaly",
                    direction: "horizontal",
                    orient: "top",
                    gradientThickness: 8,
                  },
                },
                opacity: {
                  condition: {
                    selection: "brush",
                    value: 1,
                  },
                  value: 0.5,
                },
                size: {
                  condition: {
                    selection: "brush",
                    value: 90,
                  },
                  value: 40,
                },
                tooltip: [
                  {
                    field: "station_name",
                    type: "nominal",
                    title: "locality",
                  },
                  {
                    field: "date",
                    type: "temporal",
                    format: "%a %e %b",
                  },
                  {
                    field: "value",
                    type: "quantitative",
                    title: "expected",
                    format: ".0f",
                  },
                  {
                    field: "count",
                    type: "quantitative",
                    title: "observed",
                  },
                  {
                    field: "anomaly",
                    type: "quantitative",
                    format: ".1f",
                  },
                ],
              },
              mark: "square",
            },
            {
              data: { url: `${annotationsData}` },
              layer: [
                {
                  encoding: {
                    x: {
                      field: "date",
                      type: "temporal",
                    },
                  },
                  mark: {
                    type: "rule",
                    strokeDash: [4, 2],
                    opacity: 0.5,
                  },
                },
                {
                  encoding: {
                    x: {
                      field: "date",
                      type: "temporal",
                    },
                    y: {
                      value: 0,
                    },
                    text: {
                      field: "notes",
                      type: "nominal",
                    },
                  },
                  mark: {
                    type: "text",
                    angle: 310,
                    align: "left",
                    opacity: 0.5,
                    fontSize: 8,
                    font: "Roboto Condensed",
                    dx: 2,
                  },
                },
              ],
            },
          ],
        },
        {
          width: 800,
          height: 400,
          layer: [
            {
              selection: {
                brush: {
                  type: "multi",
                  fields: ["station_name"],
                },
              },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                  axis: {
                    gridOpacity: {
                      condition: {
                        test: "day(datum.value) == 6 || day(datum.value) == 0",
                        value: 1,
                      },
                      value: 0,
                    },
                    tickSize: {
                      condition: {
                        test: "day(datum.value) == 1",
                        value: 6,
                      },
                      value: 3,
                    },
                    tickCount: 100,
                    gridWidth: 8,
                    gridColor: "#f6f6f6",
                    labelExpr:
                      "timeFormat(datum.value, '%a') == 'Mon' ? timeFormat(datum.value, '%e %b') : ''",
                    title: "",
                    labelFont: "Roboto Condensed",
                  },
                },
                y: {
                  field: "anomaly",
                  type: "quantitative",
                  scale: {
                    domain: [`${mivAnomalyMin}`, `${mivAnomalyMax}`],
                    nice: false,
                  },
                  title: "Anomaly",
                  labelFont: "Roboto Condensed",
                },
                color: {
                  condition: {
                    selection: "brush",
                    field: "station_name",
                    type: "nominal",
                    legend: null,
                  },
                  value: "black",
                },
                opacity: {
                  condition: {
                    selection: "brush",
                    value: 1,
                  },
                  value: 0.1,
                },
                size: {
                  condition: {
                    selection: "brush",
                    value: 2,
                  },
                  value: 0.2,
                },
              },
              mark: {
                type: "line",
                interpolate: "monotone",
                clip: true,
              },
            },
            {
              data: {
                values: [
                  {
                    origin: 0,
                  },
                ],
              },
              encoding: {
                y: {
                  field: "origin",
                  type: "quantitative",
                },
              },
              mark: "rule",
            },
            {
              data: { url: `${annotationsData}` },
              encoding: {
                x: {
                  field: "date",
                  type: "temporal",
                },
              },
              mark: {
                type: "rule",
                strokeDash: [4, 2],
                opacity: 0.5,
              },
            },
          ],
        },
      ],
    },
    {
      width: 400,
      height: 650,
      transform: [
        {
          aggregate: [
            {
              op: "count",
              field: "station_name",
              as: "numReadings",
            },
          ],
          groupby: ["station_name"],
        },
        {
          lookup: "station_name",
          from: {
            data: { url: `${mivStationData}` },
            key: "station_name",
            fields: ["lon", "lat"],
          },
        },
      ],
      layer: [
        {
          layer: [
            {
              data: {
                url: `${greenPolys}`,
                format: {
                  type: "topojson",
                  feature: `${greenPolyFeature}`,
                },
              },
              mark: {
                type: "geoshape",
                color: "rgb(239,244,225)",
              },
            },
            {
              data: {
                url: `${blueLines}`,
                format: { type: "topojson", feature: `${blueLineFeature}` },
              },

              mark: {
                type: "geoshape",
                color: "rgb(226,237,246)",
                strokeWidth: 4,
                filled: false,
              },
            },
            {
              data: {
                url: `${bluePolys}`,
                format: { type: "topojson", feature: `${bluePolyFeature}` },
              },
              mark: {
                type: "geoshape",
                color: "rgb(226,237,246)",
              },
            },
          ],
        },
        {
          selection: {
            brush: {
              type: "multi",
              fields: ["station_name"],
            },
          },
          encoding: {
            longitude: {
              field: "lon",
              type: "quantitative",
            },
            latitude: {
              field: "lat",
              type: "quantitative",
            },
            color: {
              field: "station_name",
              type: "nominal",
              legend: null,
            },
            opacity: {
              condition: {
                selection: "brush",
                value: 1,
              },
              value: 0.1,
            },
          },
          mark: {
            type: "square",
            size: 80,
          },
        },
        {
          encoding: {
            longitude: {
              field: "lon",
              type: "quantitative",
            },
            latitude: {
              field: "lat",
              type: "quantitative",
            },
            text: {
              field: "station_name",
              type: "nominal",
            },
            opacity: {
              condition: {
                selection: "brush",
                value: 1,
              },
              value: 0.1,
            },
          },
          mark: {
            type: "text",
            dy: 10,
            fontSize: 10,
            font: "Roboto Condensed",
          },
        },
      ],
    },
  ],
};




// Reference each of the specs with an ID that can be used in the main HTML.
// If a new spec is added above, add its name along with a corresponding DOM id.


vegaEmbed("#visLinkedBicycle", vlSpecLinkedBicycle).catch(console.error);
vegaEmbed("#visLinkedFoot", vlSpecLinkedFoot).catch(console.error);
vegaEmbed("#visLinkedMiv", vlSpecLinkedMiv).catch(console.error);

