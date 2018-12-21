library(ggnetwork)
library(dplyr)
library(nycflights13)
library(igraph)
library(intergraph)
library(sna)
library(ggplot2)
library(plotly)
library(htmlwidgets)

flights %>% head()

df_edges <- flights %>% group_by(origin, dest) %>% summarize(weight = n())
df_edges %>% arrange(desc(weight)) %>% head()

# blue, red, green
colors = c("#3498db", "#e74c3c", "#2ecc71")
# seting alphabetical order; allows for predictable ordering later
origins = c("EWR", "JFK", "LGA")
df_colors = tbl_df(data.frame(origin=origins, color=origins))
df_edges <- df_edges %>% left_join(df_colors)

df_edges %>% arrange(desc(weight)) %>% head()
