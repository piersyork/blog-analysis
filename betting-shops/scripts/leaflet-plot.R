pal <- leaflet::colorBin(
  palette = "Oranges", bins = c(0, 1, 10, 100, 284),
  domain = bookies_by_la_map$bookies_per_100k
)
leaflet::
bookies_by_la_map$bookies_per_100k |> max()


labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  bookies_by_la_map$area_name, bookies_by_la_map$bookies_per_100k
) |> lapply(htmltools::HTML)

bookies_by_la_map |>
  leaflet::leaflet() |>
  leaflet:: addPolygons(
    fillColor = ~pal(bookies_per_100k),
    weight = 0.2,
    opacity = 1,
    color = "black",
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = leaflet::highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = leaflet::labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) |>
  leaflet::addLegend(values = ~bookies_per_100k, opacity = 0.7, title = NULL,
                     labels = c("1", "10", "100", "284"), # "0 – 1", "2 – 10", "11 – 100", "101 – 284"
                     colors = pal(c(0, 1, 10, 100)),
                     position = "bottomleft", labFormat = leaflet::labelFormat()) |>
  leaflet::addControl("<b>Betting shops per 100k<b/>", position = "topright") |>
  leaflet::addTiles("", attribution = "<a href = 'https://piersyork.github.io/'>Piers York<a/>")
