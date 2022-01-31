library(tidyverse)

# part 1 ####
input <- read_lines("Day17/input.txt")
input <- "target area: x=20..30, y=-10..-5"

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

