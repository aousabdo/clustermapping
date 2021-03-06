library(ggnetwork)
library(dplyr)
library(nycflights13)
library(igraph)
library(intergraph)
library(sna)
library(ggplot2)
library(plotly)
library(htmlwidgets)


df_edges <- flights %>% group_by(origin, dest) %>% summarize(weight = n())

net <- graph.data.frame(df_edges, directed = T)

cluster_data_out$related_clusters_dt[, .(parent_cluster_name, cluster_name_t, related_avg)] %>% graph.data.frame() -> net


all_related_clusters <- get_all_related_clusters(clusters_list_input = clusters_list)
all_related_clusters[complete.cases(all_related_clusters), .(parent_cluster_name, cluster_name_t, related_avg)] %>% graph.data.frame() -> net


net <- add_vertices(net, 1, name = "Jesse")

clusters_with_no_related_clusters <- all_related_clusters[!complete.cases(all_related_clusters), parent_cluster_name]

for(i in clusters_with_no_related_clusters){
  net <- add_vertices(net, 1, name = i)
}
set.seed(pi)
V(net)$degree <- centralization.degree(net)$res
df_net <- ggnetwork(net, layout = "fruchtermanreingold", weights="related_avg", niter=5000)
# df_net <- ggnetwork(net, layout = "fruchtermanreingold", weights="weight", niter=5000)
p <- ggplot(df_net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(size=0.4, alpha=0.25) +
  geom_nodes(aes(size = degree, text=vertex.names)) +
  ggtitle("Network Graph of U.S. Flights Outbound from NYC in 2013") +
  theme_blank()
p
p %>% ggplotly(tooltip="text")

