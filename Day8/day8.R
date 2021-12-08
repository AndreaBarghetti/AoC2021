library(tidyverse)

# part 1 ####
input <- read_lines("Day8/input.txt")

sort_names <- function(chr) {
  map_chr(chr, function(chr) {
    chr %>% str_split("") %>% unlist() %>%  unique() %>% sort() %>% str_c(collapse = "") 
  })
}

tidy_input <- function(x) { 
  x %>% 
    str_split(" \\| ") %>% unlist() %>% 
    str_split(" ") %>% 
    map(sort_names) %>%
    setNames(c("input","output"))
}

displays <- input %>% 
  map(tidy_input)

count_by_panels <- function(display) {
  sum(display$output %>% str_count %in% c(2,3,4,7))
}

sum(map_int(displays, count_by_panels))

# part 2 ####

# all possible combinations of a panel
library(combinat)

comb_matrix <- combinat::permn(letters[1:7]) %>% 
  map(function(n) {
    names(n) <- LETTERS[1:7]
    n
  }) %>% purrr::reduce(rbind)
rownames(comb_matrix) <- 1:nrow(comb_matrix)

allow_digits <- c("abcefg" = "0", 
                  "cf" = "1", 
                  "acdeg" = "2", 
                  "acdfg" = "3", 
                  "bcdf" = "4", 
                  "abdfg" = "5", 
                  "abdefg" = "6", 
                  "acf" = "7", 
                  "abcdefg" = "8", 
                  "abcdfg" = "9")

# this is slow but it works
# can make it faster by stopping at first occurrence of not allowed digit
# within each list of digits
fix_display <- function(display) {
  for (i in 1:nrow(comb_matrix)) {
    r <- all(str_replace_all(display$input %>% toupper(), 
                             comb_matrix[i,]) %>% sort_names() %in% names(allow_digits))
    if(r) {
      r <- str_replace_all(display$output %>% toupper(), 
                           comb_matrix[i,]) %>% sort_names()
      return(allow_digits[r] %>% 
               str_c(collapse = "") %>% 
               as.integer())
      break
    }
  }
}

fixed_outputs <- map(displays, fix_display)

fixed_outputs %>% 
  unlist() %>% 
  sum()

