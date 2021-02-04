brasil <- rgdal::readOGR("data/br-states.json")

casos <- read.csv("data/cases-brazil-states.csv")
casos$vacc_per <- casos$vaccinated_per_100k_inhabitants/1000
casos$death_per <- casos$deaths_per_100k_inhabitants/1000
casos$cases_per <- casos$totalCases_per_100k_inhabitants/1000

casos$date <- ymd(casos$date)

temp <- casos %>% group_by(state) %>%
  summarise(data = max(date))

temp1 <- merge(temp, casos, by.x = c("state", "data"), by.y = c("state", "date"), all.x = TRUE)
base <- merge(brasil, temp1, by.x ="id", by.y = "state" )

range <- ceiling(max(base$vacc_per, na.rm=TRUE) - min(base$vacc_per, na.rm=TRUE))
passo <- range/8
passo <- round(passo, 3)
bins <- 0
for (i in 1:8){
  t <- i*passo
  bins <- c(bins, t)
}

pal <- colorBin("YlGnBu", domain = base$vacc_per, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g vacinados por 100 habitantes",
  base$id, round(base$vacc_per, 2)
) %>% lapply(htmltools::HTML)

leaflet(brasil,
        options = leafletOptions(minZoom = 4)) %>%
  setView(lng = -47.883 , lat = -15.793, zoom = 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(base$vacc_per),
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~base$vacc_per, opacity = 0.7, title = "Vacinados por 100 habitantes",
            position = "bottomright")

