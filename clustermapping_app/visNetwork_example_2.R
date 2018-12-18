source('global.R')
edges <- build_graph_vis(related_cluster_input = all_related_clusters
                         , clusters_avlbl_input = clusters_avlbl
                         , apply_filters = T)[[1]]

nodes <- build_graph_vis(related_cluster_input = all_related_clusters
                         , clusters_avlbl_input = clusters_avlbl
                         , apply_filters = T)[[2]]

# remove nodes with no connections
nodes_w_edges <- c(edges[, from], edges[, to]) %>% unique()
nodes <- nodes[id %in% nodes_w_edges]

selected_cluster <- edges[sample(unique(from), 1), from]
selected_nodes <- c(selected_cluster, edges[from == selected_cluster, to])

print(selected_cluster)
print(selected_nodes)


visNetwork(nodes, edges, height = "700px", width = "1000px") %>% 
  visNodes(size = 25, physics = F, x = 1:nrow(nodes)) %>%
  visLayout(improvedLayout = T) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T, degree=1
                                     , algorithm="hierarchical"
                                     # , hideColor="red"
                                     , labelOnly = FALSE
                                     ), 
             nodesIdSelection = list(enabled = T
                                     , selected=selected_cluster)
             #, selectedBy = list(variable="id",selected = 5, values = c(5,6,11))
             )  %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE
                 , hoverConnectedEdges = T, navigationButtons = T) %>% 
  # visPhysics(solver = "repulsion"
  #            , barnesHut = list(gravitaionalConstant=-2000
  #                               , springConstant=0.001
  #                               , avoidOverlap=1)) %>%
  print()

# sankeyNetwork(
#   Links = edges, Nodes = nodes,
#   Source = "from", Target = "to",
#   NodeID = "cluster_short_name", Value = "related_min",
#   fontSize = 16) %>%
#   htmlwidgets::prependContent(htmltools::tags$h1("Related Clusters"))

layout_ <- "layout.grid.3d"
layout_ <- "layout.kamada.kawai"
layout_ <- "layout.mds"
layout_ <- "layout.random"

visNetwork(nodes, edges, height = "1000px", width = "1000px") %>%
  visIgraphLayout(layout = layout_) %>%
  visNodes(size = 45) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T),
             nodesIdSelection = T) %>% print() 
