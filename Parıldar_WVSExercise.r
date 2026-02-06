packages <- c(
  "tidyverse", # data wrangling and plotting
  "rnaturalearth", # world map geometries
  "rnaturalearthdata",
  "sf", # spatial (GIS) data handling
  "leaflet", # interactive maps
  "plotly", # interactive plots
  "countrycode", # country code conversions
  "igraph", # network analysis
  "visNetwork", # interactive network visualization
  "janitor" # small data-cleaning helpers
)
# Install missing packages (if any)
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))

wvs <- read_csv("/Users/elifparildar/Downloads/WVS_Cross-National_Wave_7_csv_v6_0.csv")

glimpse(wvs)
dim(wvs)

### 2. CHECK COUNTRY COVERAGE
wvs %>%
  count(B_COUNTRY_ALPHA, sort = TRUE)

### 3. CLEAN SENSE OF CONTROL (Q48)
wvs_clean <- wvs %>% 
  mutate(
    control = ifelse(Q48 < 0, NA, Q48)
  )

summary(wvs_clean$control)

### 4. AGGREGATE CONTROL TO THE COUNTRY LEVEL 
country_control <- wvs_clean %>%
  filter(!is.na(control)) %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(
    mean_control = mean(control, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n > 500)

country_control <- country_control %>%
  mutate(
    iso3 = case_when(
      B_COUNTRY_ALPHA == "NIR" ~ "GBR",
      TRUE ~ B_COUNTRY_ALPHA
    )
  )

### 5. INTERACTIVE WORLD MAP OF CONTROL (LEAFLET)
world <- ne_countries(scale = "medium", returnclass = "sf")

world_control <- world %>%
  left_join(country_control, by = c("iso_a3" = "iso3"))

pal <- colorNumeric(
  palette = "PuRd",
  domain = world_control$mean_control,
  na.color = "#eeeeee"
)

leaflet(world_control) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(mean_control),
    color = "white",
    fillOpacity = 0.8,
    label = ~paste0(
      name, "<br>",
      "Avg Control: ", round(mean_control, 2), "<br>",
      "Sample size: ", n 
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#444",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~mean_control,
    title = "Sense of Control",
    position = "bottomright"
  )

### 6. TRUST AND LIFE SATISFACTION: CORRELATION ANALYSIS
wvs_clean <- wvs_clean %>%
  mutate(
    life_satisfaction = ifelse(Q49 < 0, NA, Q49)
  )

country_corr <- wvs_clean %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(
    avg_control = mean(control, na.rm = TRUE),
    avg_life_sat = mean(life_satisfaction, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n > 500) %>%
  mutate(
    country_name = countrycode(
      B_COUNTRY_ALPHA,
      "iso3c",
      "country.name"
    )
  )

country_corr_plot <- country_corr %>%
  drop_na(avg_control, avg_life_sat) %>%
  arrange(avg_control)

lm.fit <- lm(avg_life_sat ~ avg_control, data = country_corr_plot)

country_corr_plot <- country_corr_plot %>%
  mutate(fitted = predict(lm.fit, newdata = country_corr_plot))
         
### 7. INTERACTIVE CORRELATION PLOT (PLOTLY)
p_base <- ggplot(
  country_corr_plot,
  aes(
    x = avg_control,
    y = avg_life_sat,
    text = paste0 (
      "Country: ", country_name,
      "<br>Avg Control: ", round(avg_control, 2),
      "<br>Life Satisfaction: ", round(avg_life_sat, 2)
    )
  )
) +
  geom_point(size = 2.5, alpha = 0.8) +
  labs(
    title = "Sense of Control and Life Satisfaction",
    subtitle = "Country-level averages (WVS Wave 7)",
    x = "Average Sense of Control",
    y = "Average Life Satisfaction"
  ) +
  theme_minimal()

p_plotly <- ggplotly(p_base, tooltip = "text") %>%
  add_lines(
    data = country_corr_plot,
    x = ~avg_control,
    y = ~fitted,
    line = list(color = "blue", width = 2), 
    name = "Linear trend"
  )

p_plotly

### 8. INGLEHART-WELZEL STYLE CULTURAL MAP (PCA)

value_vars <- c(
  "Q17",  # obedience vs independence
  "Q48",  # control
  "Q46",  # happiness
  "Q49",  # life satisfaction
  "Q57"   # trust
)

values_clean <- wvs %>%
  select(B_COUNTRY_ALPHA, all_of(value_vars)) %>%
  mutate(across(all_of(value_vars), \(x) ifelse(x < 0, NA, x))) %>%
  drop_na()

values_country <- values_clean %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(
    across(all_of(value_vars), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    country_name = countrycode(
      B_COUNTRY_ALPHA,
      "iso3c",
      "country.name"
    )
  )

values_country <- values_country %>%
  mutate(
    B_COUNTRY_ALPHA = ifelse(B_COUNTRY_ALPHA == "NIR", "GBR", B_COUNTRY_ALPHA),
    country_name = countrycode(B_COUNTRY_ALPHA, "iso3c", "country.name")
  )



pca <- prcomp(
  values_country %>% select(all_of(value_vars)),
  scale. = TRUE
)

cultural_map <- values_country %>%
  bind_cols(as.data.frame(pca$x[, 1:2])) %>%
  rename(
    wellbeing_agency = PC1,
    autonomy_social_trust = PC2
  )

plot_ly(
  data = cultural_map,
  x = ~wellbeing_agency,
  y = ~autonomy_social_trust,
  type = "scatter",
  mode = "markers",
  text = ~paste0(
    country_name,
    "<br>Well-being & Agency: ", round(wellbeing_agency, 2),
    "<br>Autonomy & Social Trust: ", round(autonomy_social_trust, 2)
  ),
  hoverinfo = "text",
  marker = list(size = 10, opacity = 0.8)
) %>%
  layout(
    title = "Psychological Value Space Across Countries (WVS Wave 7)",
    xaxis = list(title = "Well-being ↔ Perceived Agency"),
    yaxis = list(title = "Autonomy ↔ Social Trust")
  )

### 9. CULTURAL SIMILARITY NETWORK (LOUVAIN CLUSTERS)
coords <- cultural_map %>%
  select(wellbeing_agency, autonomy_social_trust) %>%
  as.matrix()

dist_matrix <- dist(coords)
sim_matrix <- 1 / (1 + as.matrix(dist_matrix))

threshold <- 0.4

edges <- as.data.frame(sim_matrix) %>%
  mutate(from = rownames(sim_matrix)) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "weight"
  ) %>%
  filter(from != to, weight > threshold)

nodes <- cultural_map %>%
  mutate(
    id = row_number(),
    label = country_name
  ) %>%
  select(
    id, 
    label,
    B_COUNTRY_ALPHA,
    wellbeing_agency,
    autonomy_social_trust
  )

g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

communities <- cluster_louvain(g, weights = E(g)$weight)

nodes$community <- communities$membership 

community_colors <- colorRampPalette(
  RColorBrewer::brewer.pal(8, "Set2")
)(length(unique(nodes$community)))


nodes <- nodes %>%
  mutate(
    color = community_colors[community]
  )

visNetwork(nodes, edges) %>%
  visNodes(
    size = 15,
    color = list(
      background = nodes$color,
      border = "black"
    )
  ) %>%
  visEdges(smooth = TRUE) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = TRUE
  ) %>%
  visPhysics(solver = "forceAtlas2Based") %>%
  visLayout(randomSeed = 42)

