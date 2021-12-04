library(tidyverse)
input <- read_lines("Day3/input.txt")

# part 1 ####
matrix <- map(input, str_split, "", simplify=T) %>% 
  map(as.integer) %>% 
  purrr::reduce(rbind) 


gamma <- colMeans(matrix) %>% round() 
epsi <- 1-gamma

binv_to_dec <- function(vector) {
  vector %>% 
    str_c(collapse = "") %>% 
    strtoi(2)
}

sapply(list(gamma,epsi), binv_to_dec) %>% 
  prod()

# part 2 ####
filter_for <- function(matrix, param=c("oxy", "co2")) {
  i<-1
  param = match.arg(param)
  while(!is.null(nrow(matrix))){
    filter <- round(mean(matrix[,i])+.00001)
    if (param=="co2") {filter<-abs(filter-1)}
    matrix <- matrix[matrix[,i]==filter,]
    i<-i+1
  }
  matrix %>% binv_to_dec()
}

prod(filter_for(matrix),
     filter_for(matrix, "co2"))