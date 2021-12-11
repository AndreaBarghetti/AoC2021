library(tidyverse)

# part 1 ####
input <- read_lines("Day9/input.txt")

space <- input %>% 
  map(str_split, "", simplify=T) %>%
  map(as.integer) %>% 
  do.call(rbind, .)

find_low_points <- function(space) {
  hlow <- apply(space,1, function(x) {
    x<lag(x, default = 10) & x<lead(x,default = 10)
  }) %>% t()
  vlow <- apply(space,2, function(x) {
    x<lag(x, default = 10) & x<lead(x,default = 10)
  })
  hlow&vlow
}

sum(space[find_low_points(space)]+1)

# part 2 ####
#basins are delimited by 9s

# replace 9s with 0 and all the rest with a 1
# then assign a unique number to each low point
space2 <- ifelse(space==9,0,1)
space2[which(find_low_points(space))] <- which(find_low_points(space))

# spread the values of low points to surroundings by max values
# without touching the edges (the 0s)
# and repeat until it's stable
spread_max <- function(space2) {
  space2 <- apply(space2, 1, function(x) {
    r <- pmap_dbl(list(lag(x,default =0), x, lead(x,default = 0)), max)
    r[x==0]<-0
    r
  }) %>% t()
  space2 <- apply(space2, 2, function(x) {
    r <- pmap_dbl(list(lag(x,default =0), x, lead(x,default = 0)), max)
    r[x==0]<-0
    r
  })
  space2
}

spread_max_all <- function(space2) {
  while(!all(space2==spread_max(space2))) {
    space2 <- spread_max(space2)
  }
  space2
}

maxed_space <- spread_max_all(space2)

maxed_space[!maxed_space%in%0:1] %>% 
  table() %>% 
  sort(decreasing = T) %>% 
  head(3) %>% 
  prod()

# animation ####
library(reshape2)

lowp_num <- sum(space2>1)
cols = rainbow(lowp_num, s=.6, v=.9)[sample(1:lowp_num,lowp_num)]
set_levels <- unique(as_factor(space2))

plot_space <- function(space) {
  space <- melt(space) %>% 
    mutate(value=factor(value, levels = set_levels))
  space %>%
    ggplot(aes(x=Var1,y=Var2)) +
    geom_tile(aes(fill=value),show.legend=F) +
    scale_fill_manual(values = c("white","black",cols), drop=FALSE) +
    theme_void()
}

film_basins <- function(space2,filename) {
  saveGIF({
    while(!all(space2==spread_max(space2))) {
      print(plot_space(space2))
      space2 <- spread_max(space2)
    }
    print(plot_space(space2))
  },
  loop=T,
  interval=0.5,
  ani.width = 500, 
  ani.height = 500,
  movie.name = filename, 
  outdir = getwd()
  )
}

# changed for for cooler animation
spread_max <- function(space2) {
  x <- apply(space2, 1, function(x) {
    r <- pmap_dbl(list(lag(x,default =0), x, lead(x,default = 0)), max)
    r[x==0]<-0
    samp <- rbinom(n = 10, size = 1, prob = .5) %>% as.logical()
    r[samp]<-x[samp]
    r
  }) %>% t()
  y <- apply(space2, 2, function(x) {
    r <- pmap_dbl(list(lag(x,default =0), x, lead(x,default = 0)), max)
    r[x==0]<-0
    samp <- rbinom(n = 10, size = 1, prob = .5) %>% as.logical()
    r[samp]<-x[samp]
    r
  })
  map2_dbl(x,y, max) %>% matrix(ncol=ncol(space2))
}

film_basins(space2, "basins.gif")
