pal <- colorNumeric(palette = "RdYlBu", na.color = "#00000000", domain = c(0, 40),
                    reverse = T)

output$countyPos <- renderLeaflet(
  leaflet(data = ct_shp) %>%
    addPolygons(
      fillColor = ~pal(pos_perc),
      color = "#000", weight = 1,
      opacity = 0.8,
      fillOpacity = 0.8,
      label = ~htmltools::htmlEscape(paste0(County, " : ", round(pos_perc, 1), "%"))
    ) %>%
    addLegend(
      pal = pal, values = 0:4 * 10,
      title = "% Positive"
    )
)

output$statePos <- renderPlot(
  ggplot(THD_dat, aes(x = Date, y = Pos_perc*100)) +
    geom_line(lwd = 1.5, colour = "#D55E00") +
    geom_smooth(se = F, colour = "#0072B2", lty = 2) +
    theme_bw() +
    labs(x = "", y = "% Positive") +
    theme(axis.text = element_text(size = 13), 
          axis.title = element_text(size = 13), 
          plot.caption = element_text(size = 13))
)

