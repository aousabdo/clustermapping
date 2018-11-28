library(ggplot2)

# function to calculate coords of a circle
circle <- function(center, radius, res = 1000) {
  # we will calculate the coords of a circle using the parametric equations of a circle
  # calcualte theta, the parametric angle
  th <- seq(0, 2*pi, len = res)
  
  # coordinates of the circle
  DT <- data.table(x = center[1] + radius*cos(th), y = center[2] + radius*sin(th))
  return(DT)
}

N <- 3

# example dataset
df  <- data.table(cat_var = paste("Cluster", LETTERS[1:N])
                  , value = 1000*(N:1)
                  , end = 500*(N:1)
                  , pos = sample(1:N, N)
                  )                       

setkey(df, pos)
setorder(df, pos)

max       <- max(df$value)
n.bubbles <- nrow(df)
scale     <- 0.4/sum(sqrt(df$value))

# calculate scaled centers and radii of bubbles           
radii <- scale*sqrt(df$value)
ctr.x <- cumsum(c(radii[1], head(radii, -1) + tail(radii, -1) + .01))

center_aligned <- TRUE

# starting (larger) bubbles
if(center_aligned){
  gg.1   <- do.call(rbind, lapply(1:n.bubbles, function(i) cbind(group = i, circle(c(ctr.x[i], radii[1]), radii[i]))))
  text.1 <- data.frame(x = ctr.x, y = radii[1], label = paste(df$cat_var, df$value, sep = "\n"))
}else{
  gg.1   <- do.call(rbind, lapply(1:n.bubbles, function(i) cbind(group = i, circle(c(ctr.x[i], radii[i]), radii[i]))))
  text.1 <- data.frame(x = ctr.x, y = radii, label = paste(df$cat_var, df$value, sep = "\n"))
}

add_inner_circles <- FALSE

# make the plot
p <- ggplot() + geom_polygon(data = gg.1, aes(x, y, group = group), fill = "seagreen") +
  geom_path(data = gg.1, aes(x, y, group = group), color = "grey50") +
  geom_text(data = text.1, aes(x, y, label = label), col = "white") +
  labs(x = "", y = "") + scale_y_continuous(limits = c(-0.1, 2.5*scale*sqrt(max(df$value)))) +
  coord_fixed() +
  theme(axis.text = element_blank()
        , axis.ticks = element_blank()
        , panel.grid = element_blank()
        , panel.background = element_blank())

if(add_inner_circles){
  # ending (smaller) bubbles
  radii  <- scale*sqrt(df$end)
  if(center_aligned){
    gg.2   <- do.call(rbind, lapply(1:n.bubbles, function(i) cbind(group = i, circle(c(ctr.x[i], radii[1]), radii[i]))))
    text.2 <- data.frame(x = ctr.x, y = 2*radii[1] + 0.02, label = df$end)
  }else{
    gg.2   <- do.call(rbind, lapply(1:n.bubbles, function(i) cbind(group = i, circle(c(ctr.x[i], radii[i]), radii[i]))))
    text.2 <- data.frame(x = ctr.x, y = 2*radii + 0.02, label = df$end)
  }
  
  p <- p + geom_polygon(data = gg.2, aes(x, y, group = group), fill = "green2") +
    geom_path(data = gg.2, aes(x, y, group = group), color = "grey50") +
    geom_text(data = text.2, aes(x, y, label = label), color = "white")
}

print(p)


