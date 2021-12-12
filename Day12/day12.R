library(tidyverse)

# part 1 ####
input <- read_lines("Day12/input.txt")

dirs <- input %>% 
  map(str_split, "-", simplify=T) %>%
  do.call(rbind, .) %>%
  as_tibble()

detect_loops <- function(Paths) {
  map_lgl(Paths, function(Path) {
    Path %>% 
      str_subset("[a-z]+") %>% duplicated() %>% any()
  })
}

add_possible_paths <- function(paths,links) {
  extended_paths <- paths %>% 
    left_join(links, by=c("End"="Start")) %>% 
    mutate(Path=map2(Path, End.y, c)) %>% 
    select(Path, End=End.y) %>% 
    filter(!detect_loops(Path))
  extended_paths
}

discorer_paths <- function(dirs) {
  
  all_links <- bind_rows(dirs %>% rename(Start=V1,End=V2),
                         dirs %>% rename(Start=V2,End=V1)) %>% 
    filter(Start!="end")
  
  paths <- all_links %>% 
    filter(Start=="start") %>% 
    mutate(Path=map2(Start, End, c)) %>% 
    select(Path, End)  
  
  solved =tibble()
  while(nrow(paths)>0) {
    paths <- add_possible_paths(paths, all_links)
    solved <- bind_rows(solved, paths %>% 
                          filter(End=="end"))
    paths <- paths %>% 
      filter(End!="end")
  }
  solved
}

discorer_paths(dirs) %>% nrow()

# part 2 ####
# iterate solution 1 for each small cave,
# by splitting it into two small caves
# note: this creates identical paths that need to be filtered later

discorer_paths2 <- function(dirs) {
  small_caves <- unique(c(dirs$V1,dirs$V2)) %>% 
    setdiff(c("start","end")) %>% 
    str_subset("[a-z]+")
  
  all_dirs <- map(small_caves, function(cave) {
    extra_dir <- dirs %>% 
      filter(V1==cave | V2==cave) %>% 
      mutate_all(str_replace_all, cave, str_c(cave,"sub"))
    bind_rows(dirs, extra_dir)
  })
  
  r <- map(all_dirs, function(dirs) {
    discorer_paths(dirs)
  }) %>% purrr::reduce(bind_rows) %>% 
    unique()
  
  r
}

# this is not the fastest thing, but it works
discorer_paths2(dirs) %>% 
  mutate(string = map_chr(Path, str_c, collapse = ",")) %>% 
  pull(string) %>% 
  str_remove_all("sub") %>% 
  unique() %>% length()


# Visualization ####
library(ggraph)
library(igraph)
library(RColorBrewer)

dirvis <- dirs %>% 
  rename(from=V1, to=V2)

vers <- c(dirvis$from,dirvis$to) %>% unique() %>% 
  tibble(name=.) %>% mutate(size=case_when(str_detect(name, "[a-z]")~10,T~20))

my_graph <- igraph::graph_from_data_frame(dirvis, vertices = vers)

graph <- ggraph(my_graph, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(width=1, alpha=.5, colour="dark red") +
  geom_node_point(aes(fill=as.factor(size)),shape=21,size=8,show.legend = F, alpha=1) +
  #scale_size(limits = c(0,20)) +
  geom_node_text(aes(label=name), col="black", size=3) +
  theme_void() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm")) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3)) +
  scale_fill_brewer()
  
ggsave(plot = graph,
       filename = "graph.png", 
       device = "png", 
       path = "Day12/",
       width = 4,
       height = 4, units = "cm")



