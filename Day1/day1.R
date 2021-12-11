library(tidyverse)
input <- read_lines("Day1/input.txt") %>% as.numeric()

# part1 ####
sum(input > lag(input), na.rm = T)

# part2 ####
slideinput <- input + lag(input) + lead(input)
sum(slideinput > lag(slideinput), na.rm = T)



# visualization
seafloor <- ggplot(data=NULL, aes(x=seq_along(input), y=max(input)-input)) +
  geom_segment(aes(y=1:max(input),
                   yend=1:max(input),
                   x=1,
                   xend=length(input),
                   col=1:max(input), alpha=.1),
               show.legend = F) +
  geom_area(show.legend = F) +
  theme_void() +
  theme(legend.position = "none")

ggsave(plot = seafloor, 
       filename = "seafloor.png", 
       device = "png", 
       path = "Day1/",
       width = 4,
       height = 4, units = "cm")
