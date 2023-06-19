

library(shiny)
library(sf)
library(tidyverse)
library(janitor)
library(cowplot)
library(leaflet)
library(glue)
library(htmltools)
library(purrr)



world <- st_read("naturalearth/ne_110m_admin_0_countries.shp")

continent <- st_read("Archive4/bboxes_continent.shp")

gdp_pc <-
  read_csv("GDP_percapita_WB.csv", skip = 3) %>%
  clean_names() %>%
  pivot_longer(starts_with("x"), names_to = "Year", values_to = "pcGDP") %>%
  mutate(Year = as.numeric(str_remove(Year, "x")))

world <- world %>%
  left_join(gdp_pc, c("BRK_A3" = "country_code"))



# Manually specify the categories to be plotted using the quantile() function to create a vector with ranges:
bins <- quantile(world$pcGDP,
                 probs = c(0, 0.25, 0.5, 0.75, 1),
                 na.rm = TRUE)




labels <- tibble(
  lab1 = bins,
  lab2 = c(bins[2:length(bins)], NA)) %>%
  slice(1:n() - 1) %>% # I remove the last row, since it has no meaning
  mutate_all(round, digits = 0) %>% # I round to have no digits
  mutate_all(paste0, "$") %>% # I add the dollar sign after the digits
  mutate(labs = paste(lab1, lab2, sep = " -"))


world <- world %>%
  mutate(quantiles = cut(pcGDP,
                         breaks = bins,
                         labels = labels %>% pull(labs),
                         include.lowest = TRUE, # includes the lowest 'breaks' value
                         ordered_result = TRUE, # orders the factors/bins
  )) %>%
  filter(!is.na(BRK_A3), !is.na(Year))



world <- world %>%
  mutate(gdp_formatted = scales::dollar(pcGDP,
                                        big.mark = " ",
                                        suffix = "$",
                                        prefix = ""
  ))



# Use the colorBin() function to create a function called pal, that will apply
# the correct colour to each value of pcGDP, as specified by the bins argument
# and the palette argument.

pal <- function(colpal){
  colorBin(
  palette = colpal,
  domain = world$pcGDP,
  bins = bins
  )
}


# Create labels to appear upon clicking on a country:
labell <-
  glue::glue("<strong>{world$SOVEREIGNT}</strong><br/>Per capita GDP: {world$gdp_formatted}") %>%
  map(htmltools::HTML)






world_rob <- st_transform(world, crs = 'ESRI:54030')





###########################
## Plotting functions
###########################

plot_timeseries <- function(data1, data2, x_var, y_var, group_var){
  ggplot()+
  geom_line(data = data1, aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[group_var]]),
            color = "lightgrey",
            alpha = 0.2)+
  geom_line(data = data2, aes(x = .data[[x_var]], y = .data[[y_var]]),
             size =2, color = "#2b8cbe") +
  xlim(1960, 2019) +
  ylim(data2 %>% pull(y_var) %>% range(na.rm = TRUE))+
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       ylab = "Per capita GDP (Dollars)",
       xlab = "Year")
}





# map_zoomed using leaflet:
map_zoomed <- function(data1, fill_var, input_var, colpal){
  leaflet(data1) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~ pal(colpal),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
        ),
      label = labell,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
        )
      ) %>%
  addLegend(
    pal = pal, values = ~pcGDP,
    opacity = 0.7,
    title = "Per capita GDP",
    position = "bottomright"
    )
  # %>%
  #   addMiniMap()
}

