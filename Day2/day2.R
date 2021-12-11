library(tidyverse)

# Part 1 ####
input <- read_lines("Day2/input.txt")

get_dir <- function(str) str_extract(str, "[a-z]+")
get_value <- function(str) as.integer(str_extract(str, "[0-9]+"))

go1 <- function(input, x=0,y=0) {
  for (str in input) {
    value = get_value(str)
    if (get_dir(str)=="forward") {x = x + value}
    if (get_dir(str)=="up") {y = y - value}
    if (get_dir(str)=="down") {y = y + value}
  }
  x*y
} 

go1(input)

# Part 2 ####
go2 <- function(input, x=0,y=0,aim=0) {
  for (str in input) {
    value = get_value(str)
    if (get_dir(str)=="up") {aim = aim - value}
    if (get_dir(str)=="down") {aim = aim + value}
    if (get_dir(str)=="forward") {
      x = x + value
      y = y + aim*value
    }
  }
  x*y
} 

go2(input)


# Visualization ####
go_vis <- function(input, x=0,y=0,aim=0) {
  pos <- list(x=0,y=0)
  for (str in input) {
    value = get_value(str)
    if (get_dir(str)=="up") {aim = aim - value}
    if (get_dir(str)=="down") {aim = aim + value}
    if (get_dir(str)=="forward") {
      x = x + value
      y = y + aim*value
    }
    pos$x<-c(pos$x,x)
    pos$y<-c(pos$y,y)
  }
  tibble(x=pos$x,y=pos$y*-1)
} 
positions <- go_vis(input)

route <- ggplot() +
  geom_segment(data=NULL, aes(y=seq(1,min(positions$y),by=-100),
                              yend=seq(1,min(positions$y),by=-100),
                              x=1,
                              xend=max(positions$x),
                              col=seq(1,min(positions$y),by=-100), alpha=.5, size=10),
               show.legend = F) +
  geom_line(data=positions, 
            aes(x=x,y=y), 
            col="black",
            size=.1) +
  theme_void() +
  theme(legend.position = "none") 

ggsave(plot = route, 
       filename = "route.png", 
       device = "png", 
       path = "Day2/",
       width = 4,
       height = 4, units = "cm")  
