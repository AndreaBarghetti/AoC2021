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

# how does it look like?
# edges in black, low points in red
space2 %>% 
  as_tibble() %>% 
  rownames_to_column("row") %>% 
  gather(col, value, contains("V")) %>% 
  mutate(col=str_remove(col,"V") %>% as.integer(),
         val = ifelse(value<=1,value,2)) %>% 
  ggplot(aes(x=row,y=col)) +
  geom_tile(aes(fill=as_factor(val)),show.legend=F) +
  scale_fill_manual(values = c("black","white","red")) +
  theme_void()

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

