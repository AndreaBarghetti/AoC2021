library(tidyverse)
library(magrittr)
library(rlang)

# part 1 ####
input <- read_lines("Day14/input.txt")
#input <- read_lines("Day14/test.txt")

polymer <- input[1]

subs <- input[-c(1:2)] %>% 
  map(function(str) {
    str_split(str, " -> ") %>% 
      unlist() %>% 
      setNames(c("p1","p2"))
  }) %>% 
  purrr::reduce(bind_rows) %>% 
  mutate(p3 = str_c(str_sub(p1,1,1),p2,str_sub(p1,2,2)))

subs_recode <- setNames(subs$p3, subs$p1)

split_polymer <- function(polymer) {
  map_chr(seq_along(1:(nchar(polymer)-1)),
          function(i) {
            str_sub(polymer, i, i+1)
          })
}

join_monomers <- function(monomers) {
  c(str_sub(monomers[1],1,1), monomers %>% str_sub(2,3)) %>% 
    str_c(collapse = "")
}

grow_monomers <- function(monomers) {
  map_chr(monomers, function(mono) {str_replace_all(mono, subs_recode[mono])})
}

expand_polymer <- function(polymer) {
  polymer %>% 
    split_polymer() %>% 
    grow_monomers() %>% 
    join_monomers()
}

polymerize <- function(polymer, cycles) {
  for (i in 1:cycles) {
    polymer <- expand_polymer(polymer)
  }
  polymer
}

polymer1 <- polymerize(polymer,10)

polymer1 %>% str_split("") %>% table() %>% sort() %>% range() %>% diff()

# part 2 ####

# must clean up and refactor it
# it got messy messy

# every time a monomer is split, replace that monomer with the 2 new ones
# add the new letter to the count
add_monomers <- function(monomers_count) {
  imap(monomers_count, function(mono_count, name) {
    long <- grow_monomers(name)
    mono_names <- c(str_sub(long, 1,2),str_sub(long, 2,3))
    setNames(rep(mono_count,2), mono_names)
  }) %>% purrr::reduce(bind_rows) %>% reshape2::melt() %>% 
    group_by(variable) %>% 
    summarise(value=sum(value, na.rm=T)) %$% 
    setNames(.$value, .$variable)
}

add_letters <- function(monomers_count) {
  new <- names(monomers_count) %>% 
    grow_monomers() %>% 
    map_chr(str_sub,2,2) %>% 
    tibble(letter=., value=monomers_count) %>% 
    group_by(letter) %>% 
    summarise(value=sum(value)) %$% 
    setNames(.$value, .$letter)
  update <- env_get(letter_counter_env, 'letter_counter') + new[sort(names(new))]
  env_poke(letter_counter_env, 'letter_counter', update)
}


monomers_count <- subs %>% 
  mutate(count=0) %$% 
  setNames(.$count, .$p1)

start_count <- split_polymer(polymer) %>% table()
monomers_count[names(start_count)] <- start_count

# put the count in its own env
start_letters <- names(monomers_count) %>% str_split("") %>% unlist() %>% unique() %>% sort() %>% setNames(rep(0,length(.)),.)
first_letters <- polymer %>% str_split("") %>% unlist() %>% table()
start_letters[names(first_letters)] <- first_letters

letter_counter_env <- rlang::env(
  letter_counter = start_letters
)

for (i in 1:40) {
  add_letters(monomers_count)
  monomers_count <- add_monomers(monomers_count)
}

env_get(letter_counter_env, "letter_counter") %>% range() %>% diff() %>% format(scientific=F)
