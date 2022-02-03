library(tidyverse)

# part 1 ####
input <- read_lines("Day18/input.txt")

analize_string <- function(string) {
  
  values <- str_extract_all(string, "[0-9]+") %>%
    unlist() %>%  
    as.integer()
  
  string_sep <- string %>% 
    str_replace_all("[0-9]+","x") %>% 
    str_split("") %>% 
    unlist()
  
  depths <- c()
  depth <- 0
  
  for (x in string_sep) {
    if (x=="[") {
      depth <- depth+1
    }
    if (x=="]") {
      depth <- depth-1
    }
    if (x=="x") {
      depths <- c(depths, depth)
    }
  }
  
  return(list(values=values, depths=depths)) 
}

explode <- function(astring) {
  
  while (max(astring$depths>4)) {
   
    expl_pos <- min(which(astring$depths>4))
    
    left <- astring$values[expl_pos]
    right <- astring$values[expl_pos+1]
    
    astring$values[expl_pos] <- 0
    astring$values <- astring$values[-c(expl_pos+1)]
    
    astring$values[expl_pos-1]<-astring$values[expl_pos-1]+left
    astring$values[expl_pos+1]<-astring$values[expl_pos+1]+right
    
    astring$depths <-astring$depths[-c(expl_pos+1)]
    astring$depths[expl_pos] <- astring$depths[expl_pos]-1
  }
  astring$values <- astring$values[!is.na(astring$values)]
  astring
}

split <- function(astring) {
  split_pos <- min(which(astring$values>9))
  
  left <- floor(astring$values[split_pos]/2)
  right <- ceiling(astring$values[split_pos]/2)
  
  astring$values[split_pos] <- left
  astring$values <- c(astring$values[1:split_pos],right,astring$values[-c(1:split_pos)])
  
  astring$depths <- c(astring$depths[1:split_pos],astring$depths[split_pos],astring$depths[-c(1:split_pos)])
  astring$depths[c(split_pos,split_pos+1)] <- astring$depths[split_pos]+1
  
  astring
}

reduce_string <- function(astring) {
  
  astring <- explode(astring)
  
  while (any(astring$values>9)) {
    astring <- split(astring) %>% explode()
  }
  astring
}

sum_2strings <- function(astring1, astring2) {
 values <- c(astring1$values,astring2$values)
 depths <- c(astring1$depths,astring2$depths)+1
 astring <- list(values=values,depths=depths)
 reduce_string(astring)
}

sum_strings <- function(strings) {
  strings %>% 
    purrr::reduce(sum_2strings)
}

pair_sum <- function(pair) {
  pair[1]*3+pair[2]*2
}

check_magnitute <- function(astring) {
  max_depth <- max(astring$depths)
  while(max_depth>0) {
    pos <- min(which(astring$depths == max_depth))
    poss <- c(pos,pos+1)
    res <- pair_sum(astring$values[poss])
    astring$values <- astring$values[-pos]
    astring$values[pos] <- res
    astring$depths <- astring$depths[-pos]
    astring$depths[pos] <- astring$depths[pos]-1
    max_depth <- max(astring$depths) 
  }
  astring$values
}

string_to_magnitude <- function(string) {
  string %>% 
    map(analize_string) %>% 
    sum_strings() %>% 
    reduce_string() %>% 
    check_magnitute()
}

string_to_magnitude(input)

# part2 ####
pairs_to_test <- gtools::permutations(n = length(input), r = 2)
  
apply(pairs_to_test, 1,function(index){
  input[index] %>% 
    string_to_magnitude()
}) %>% max()


