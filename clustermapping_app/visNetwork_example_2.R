source('global.R')
all_related_clusters <- get_all_related_clusters(clusters_list_input = clusters_list)
edges <- build_graph_vis(related_cluster_input = all_related_clusters
                         , clusters_avlbl_input = clusters_avlbl
                         , apply_filters = T)[[1]]

nodes <- build_graph_vis(related_cluster_input = all_related_clusters
                         , clusters_avlbl_input = clusters_avlbl
                         , apply_filters = T)[[2]]

nodes[, shape := 'circle']
# remove nodes with no connections
# nodes_w_edges <- c(edges[, from], edges[, to]) %>% unique()
# nodes <- nodes[id %in% nodes_w_edges]
# 
selected_cluster <- edges[sample(unique(from), 1), from]
selected_nodes <- c(selected_cluster, edges[from == selected_cluster, to])
# 
# print(selected_cluster)
# print(selected_nodes)

get_longest_string <- function(x){
  data <- unlist(str_split(x, "\n| ", n = Inf))
  index <- sapply(data, nchar)
  data <- data[which.max(index)]
  return(as.integer(nchar(data)))
}

# apply to nodes table
nodes[ , longest_nchar := sapply(label, function(x) get_longest_string(x))]
nodes[ , n_spaces := max(longest_nchar) - longest_nchar]
nodes[(n_spaces %% 2) != 0, n_spaces := n_spaces + 1]
nodes[, label := paste0(strrep("_", n_spaces/2), label, strrep("_", n_spaces/2))]

visNetwork(nodes, edges, height = "700px", width = "1000px") %>% 
  # visIgraphLayout(layout = 'layout.davidson.harel') %>%
  visNodes(physics = F, fixed = F) %>%
  # visLayout(improvedLayout = T, randomSeed = 123) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T, degree=1
                                     # , algorithm="hierarchical"
                                     # , hideColor="red"
                                     , labelOnly = FALSE
  ), 
  nodesIdSelection = list(enabled = T
                          , selected=selected_cluster)
  , collapse = TRUE
  )  %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE
                 , hoverConnectedEdges = T, navigationButtons = T) %>% 
  print()

visNetwork(nodes, edges, height = "700px", width = "1000px") %>% 
  visIgraphLayout(layout = "layout_with_kk", # or use igraph's `layout_*`s in quotes
                  # layout = "layout.norm",  # using saved coords? set this!
                  # layoutMatrix = coords,   # our previous coords
                  smooth = FALSE,            # set to F when bogged by bigger graphs
                  physics = TRUE             # set to F when bogged by bigger graphs
  ) %>% 
  visNodes(size = 50) %>%
  visEdges(color = list(highlight = "lightgray")) %>%
  visPhysics(repulsion = list(springlength = 50), # usually will take some tweaking
             maxVelocity = 2,
             solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -1000),
             timestep = 0.25)

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
