library(tidyverse)

# part 1 ####
input <- read_lines("Day6/input.txt")

fishes <- str_split(input,",") %>% unlist() %>% as.integer()

fish_pop <- sapply(0:9, function(i) {sum(fishes==i)}) %>% 
  as.numeric()

next_day <- function(fish_pop) {
  new <- fish_pop[1]
  fish_pop <- c(fish_pop[2:9],new)
  fish_pop[7]<- fish_pop[7]+new
  fish_pop
}

grow_fish <- function(fish_pop, days=80) {
  for (i in 1:days) {
    fish_pop <- next_day(fish_pop)
    }
  fish_pop
}

sum(grow_fish(fish_pop,80))

# part 2 ####
library(animation)

sum(grow_fish(fish_pop, 256)) %>% format(scientific=F)


# Visualization
pops <- lapply(1:256, function(day) {grow_fish(fish_pop, day)})

pops[[1]]


saveGIF(
  {
    for (i in 1:256) {
      print(ggplot(data=NULL, aes(x=as.character(1:9), y=pops[[i]])) +
              geom_col(aes(fill=factor(as.character(1:9))), show.legend = F) +
              theme_bw() +
              scale_y_continuous(n.breaks = 2, breaks = c(0,max(pops[[i]]))) +
              scale_fill_brewer(palette = "Blues") +
              labs(y="Population Size",x="Reproductive Status") +
              ggtitle("Day", i))
    }
  },
  loop=T,
  interval=0.1,
  ani.width = 300, 
  ani.height = 300,
  movie.name = "fishes.gif", 
  outdir = getwd()
)

