library(tidyverse)

# part 1 ####
input <- read_lines("Day17/input.txt")
#input <- "target area: x=20..30, y=-10..-5"

get_target <- function(input) {
  x = str_extract(input, "x=.+,") %>% 
    str_extract_all("[-+]*[0-9]+", simplify = T) %>% as.integer()
  y = str_extract(input, "y=.+") %>% 
    str_extract_all("[-+]*[0-9]+", simplify = T) %>% as.integer()
  return(list(x=x,y=y))
}

track_xy <- function(y_speed, x_speed, target) {
  y <- 0
  x <- 0
  
  while (y >= min(target$y) & abs(x) <= max(abs(target$x))) {
    #print(c(x,y))
    
    if (between(y, min(target$y), max(target$y)) & between(x, min(target$x), max(target$x))) {
      return(c(hit=T))
    }
    y <- y + y_speed
    x <- x + x_speed
    
    y_speed <- y_speed - 1
    if (x_speed !=0) {x_speed <- x_speed - (x_speed/abs(x_speed))}
  }
  #print(c(x,y))
  return(c(hit=F))
}

try_all_x <- function(try_x, y_speed, target) {
  for (x in try_x) {
    if(track_xy(x_speed = x, y_speed = y_speed, target = target)) {
      return(T)
    }
  }
  return(F)
}

try_all <- function(target) {
  
  max_yspeed <- abs(min(target$y))-1
  max_xspeed <- max(target$x)
  min_xspeed <- min(which(cumsum(1:min(target$x)) >= min(target$x)))
  try_x <- min_xspeed:max_xspeed
  try_y = 1:max_yspeed
  
  for (y in sort(try_y, decreasing = T)) {
    if (try_all_x(try_x = try_x, y_speed = y, target = target)) {return(y)}
  }
}

best_y_speed <- try_all(target = get_target(input))

max(cumsum(0:best_y_speed))

# part 2 ####
try_all_x2 <- function(try_x, y_speed, target) {
  sapply(try_x, function(x) {
    track_xy(x_speed = x, y_speed = y_speed, target = target)
  }) %>% sum()
}

try_all2 <- function(target) {
  
  max_yspeed <- abs(min(target$y))-1
  max_xspeed <- max(target$x)
  min_xspeed <- min(which(cumsum(1:min(target$x)) >= min(target$x)))
  try_x <- min_xspeed:max_xspeed
  try_y = min(target$y):max_yspeed
  
  sapply(try_y, function(y) {
    try_all_x2(try_x = try_x, y_speed = y, target = target)
  })
}

sum(try_all2(target = get_target(input)))

# Visualization ####
target <- get_target(input)

track_xy_vis <- function(y_speed, x_speed, target) {
  y <- 0
  x <- 0
  
  res <- list(x=c(),y=c())
  hit <- F
  remove <- F
  
  while (y >= min(target$y) & abs(x) <= max(abs(target$x))) {
    #print(c(x,y))
    res$y <- c(res$y, y)
    res$x <- c(res$x, x)
    
    # remove the ones with positive curvature
    #if (x_speed == 0 & y_speed >0) {remove<-T}
    
    # remove the ones that drop straight
    #if (x_speed == 0) {remove<-T} 
    
    if (between(y, min(target$y), max(target$y)) & between(x, min(target$x), max(target$x))) {
      
      if(!remove) {hit<-T}
      
    }
    
    y <- y + y_speed
    x <- x + x_speed
    
    y_speed <- y_speed - 1
    if (x_speed !=0) {x_speed <- x_speed - (x_speed/abs(x_speed))}
  }
  if(hit) {return(res)}
}

try_all_x_vis <- function(try_x, y_speed, target) {
  lapply(try_x, function(x) {
    track_xy_vis(x_speed = x, y_speed = y_speed, target = target)
  })
}

try_all_vis <- function(target) {
  
  max_yspeed <- abs(min(target$y))-1
  max_xspeed <- max(target$x)
  min_xspeed <- min(which(cumsum(1:min(target$x)) >= min(target$x)))
  try_x <- min_xspeed:max_xspeed
  try_y <- min(target$y):max_yspeed
  # remove the ones shot at angle < 0
  #try_y <- 0:max_yspeed
  
  
  lapply(try_y, function(y) {
    try_all_x_vis(try_x = try_x, y_speed = y, target = target)
  })
}

res_vis <- try_all_vis(target = target) %>% 
  unlist(recursive = F)

vis_df <- res_vis[sapply(res_vis, length)==2] %>% 
  reshape2::melt() %>%
  group_by(L1,L2) %>% 
  mutate(n=row_number()) %>% 
  spread(L2, value)

best <- vis_df %>%
  ungroup() %>% 
  filter(y==max(y)) %>% 
  pull(L1) %>%  unique()
  
#%>% 
# filter(L1 %in% best[1])

plot <- ggplot(data=vis_df ) +
  geom_point(x=0,y=0,col="red", size=1) +
  geom_line(aes(x=x,y=y, group=L1),
            size=0.1,
            col="white") +
  # geom_point(aes(x=x,y=y),
  #            size=.2,
  #            col="white") +
  annotate(geom = "rect", 
           col="green",
           size=.2,
           fill="transparent",
           xmin = min(target[["x"]]), 
           xmax = max(target[["x"]]),
           ymin = min(target[["y"]]),
           ymax = max(target[["y"]])) +
  coord_cartesian(xlim = c(0, max(target[["x"]])), 
                  ylim = c(min(target[["y"]]), NA))+
  theme_void() +
  theme(panel.background = element_rect(fill="#08306B"))

ggsave(plot = plot, 
       filename = "day17plot.png", 
       device = "png", 
       path = "Day17/",
       width = 4,
       height = 6, 
       units = "cm")  

