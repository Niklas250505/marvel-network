```{r  message=FALSE, warning=FALSE}

#Daten vorbereiten
centrality_data <- data.frame(
  Name        = V(g_marvel)$name,
  Degree      = degree(g_marvel),
  Betweenness = betweenness(g_marvel, directed = FALSE, normalized = TRUE)
)

#Top Akteure für Beschriftung 
top_degree  <- centrality_data %>% arrange(desc(Degree))      %>% head(10)
top_between <- centrality_data %>% arrange(desc(Betweenness)) %>% head(10)
labels_data <- unique(rbind(top_degree, top_between))

#Basis-Scatterplot
p <- ggplot(
  centrality_data,
  aes(
    x = Degree,
    y = Betweenness,
    size = Degree,
    text = paste0(
      "Held: ", Name, "<br>",
      "Degree: ", Degree, "<br>",
      "Betweenness: ", round(Betweenness, 5)
    )
  )
) +
  geom_point(
    color = "grey",
    alpha = 0.5
  ) +
  
  #Hervorhebung zentraler Akteure
  geom_point(
    data = labels_data,
    aes(x = Degree, y = Betweenness),
    color = "black",
    fill  = "red",
    shape = 21,
    size  = 3.5,
    inherit.aes = FALSE
  ) +
  
  #Labels
  geom_text(
    data = labels_data,
    aes(x = Degree, y = Betweenness, label = Name),
    vjust = -0.7,
    size  = 3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  
  scale_size_continuous(range = c(1.5, 6), guide = "none") +
  
  labs(
    title    = "Strategische Positionierung der Helden",
    x        = "Degree Centrality (Anzahl direkter Verbindungen)",
    y        = "Betweenness Centrality (Brückenfunktion)"
  ) +
  
  theme_minimal()

#Interaktivität
ggplotly(p, tooltip = "text")

```
