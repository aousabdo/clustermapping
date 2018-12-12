nodes <- data.frame(id = 1:10,
                    
                    # add labels on nodes
                    label = paste("Node", 1:10),
                    
                    # add groups on nodes 
                    group = c("GrA", "GrB"),
                    
                    # size adding value
                    value = 1:10,          
                    
                    # control shape of nodes
                    shape = c("square", "triangle", "box", "circle", "dot", "star",
                              "ellipse", "database", "text", "diamond"),
                    
                    # tooltip (html or character), when the mouse is above
                    title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),
                    
                    # color
                    color = c("darkred", "grey", "orange", "darkblue", "purple"),
                    
                    # shadow
                    shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE)
)             

# head(nodes)
# id  label group value    shape                     title    color shadow
#  1 Node 1   GrA     1   square <p><b>1</b><br>Node !</p>  darkred  FALSE
#  2 Node 2   GrB     2 triangle <p><b>2</b><br>Node !</p>     grey   TRUE

edges <- data.frame(from = c(1,2,5,7,8,10), to = c(9,3,1,6,4,7))

all_related_clusters <- get_all_related_clusters(clusters_list_input = clusters_list)

edges <- data.table(from = all_related_clusters[, parent_cluster_code], to = all_related_clusters[, related_cluster_code])

# we need to get rid of double edges, this is the case where from:to is the exace opposite of to:from
setkeyv(edges, c("from", "to"))

# add a column which would tell us if there is a duplicate. 
# this took me a while to figure out!!!
edges[, dup_col := ifelse(from < to, paste(from, to, sep = "_"), paste(to, from, sep = "_"))]

# before we apply the unique function, we need to get out the NA's rows 
tmp <- edges[is.na(to)]

edges <- unique(edges[complete.cases(edges)], by = "dup_col")

edges <- rbind(edges, tmp)

setkeyv(edges, c("from", "to")) %>%
  

IDs <- all_related_clusters[, unique(parent_cluster_code)]

nodes <- data.table(id = IDs
                    , label = clusters_avlbl[clusters_codes %in% IDs, clusters_names])

nodes[, title := paste0("<p><b>", label,"</b></p>") ]
nodes[, shadows := TRUE]

set.seed(123)
remove.nas <- TRUE
if(remove.nas){
  edges <- edges[complete.cases(edges)]
  nodes <- nodes[!(id %in% tmp$from)]
}

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_in_circle") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)

visNetwork(nodes, edges)%>%
  visIgraphLayout(layout = "layout_in_sphere") %>%
  visNodes(size = 10)
