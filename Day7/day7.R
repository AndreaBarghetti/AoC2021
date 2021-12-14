library(tidyverse)

# part 1 ####
input <- read_lines("Day7/input.txt")

crabs <- str_split(input,",") %>% unlist() %>% as.integer()

sum(abs(crabs - median(crabs)))


# part 2 ####
crab_space <- function(x) {
  map_int(x, function(x) {
    if (x==0) return(x)
    sum(1:x)
  })
}

# just try all positions and find min sum
map_int(min(crabs):max(crabs), function(x) {
  sum(crab_space(abs(crabs-x)))
}) %>% min()


# Visualization ####
library(emo)

crabvis <- ggplot(data=NULL, aes(x=seq_along(crabs), y=sample(crabs))) +
  geom_hline(yintercept = 372, col="black", linetype="dashed", size=.5) +
  geom_text(aes(label=emo::ji("crab")), size=runif(4,7, n =length(crabs))) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"))

ggsave(plot=crabvis, 
       filename = "crabs.svg", 
       device = "svg", 
       path = "Day7/",
       width = 25,
       height = 15, units = "cm")


plot_crabs <- function(crabs) {
  ggplot(data=NULL, aes(x=seq_along(crabs), y=crabs)) +
    #geom_hline(yintercept = 372, col="black", linetype="dashed", size=.5) +
    #geom_text(aes(label=emo::ji("crab")), size=3) +
    geom_point(size=3, shape=21, fill="pink", col="red", stroke=1) +
    theme_void() +
    theme(panel.background = element_rect(fill = "light blue",
                                          colour = "light blue",
                                          size = 0.5, linetype = "solid")) +
    coord_cartesian(ylim = c(0,2000))
}

film_crabs <- function(crabs) {
  
  speeds <- ((crabs - median(crabs))/10) %>% ceiling()
  median <- median(crabs)
  saveGIF({
    print(plot_crabs(crabs))
    crabs <- crabs - (((crabs-median) %% speeds) %>% replace_na(0))
    print(plot_crabs(crabs))
    
    for (i in 1:50) {
      not_aligned <- which(crabs != median(crabs))
      
      pick_some <- sample(not_aligned, size =  min(length(not_aligned), 300))
      
      crabs[pick_some] <- crabs[pick_some]-(speeds[pick_some])
      
      print(plot_crabs(crabs))
    }
  },
  loop=T,
  interval=0.1,
  ani.width = 500, 
  ani.height = 300,
  movie.name = "crabs.gif", 
  outdir = getwd())
}
film_crabs(crabs)



