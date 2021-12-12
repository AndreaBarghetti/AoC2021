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




