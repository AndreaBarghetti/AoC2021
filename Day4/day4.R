library(tidyverse)
library(R6)
# part 1 ####
input <- read_lines("Day4/input.txt")

numbers <- input[1] %>% str_split(",") %>% unlist() %>% as.integer()

clean_input <- input[-1]

get_tables <- function(input) {
  tables<-list()
  n_tables <- sum(sapply(clean_input, identical, ""))+1
  for (i in 1:n_tables) {tables[i] <-c()}
  b<-0
  for(row in clean_input) {
    if (row==""){
      b<-b+1
      next
    }
    tables[[b]]<-rbind(tables[[b]], str_extract_all(row,"[0-9]+") %>% unlist() %>% as.integer())
  }
  tables
}

tables <- get_tables(input)

bingo <- R6Class(classname = "bingo",
                 public =  list(
                   matrix=NA,
                   win = F,
                   id=NA,
                   check_win = function(n_sorted) {
                     byrow= any(apply(self$matrix,1,function(x)all(x %in% n_sorted)))
                     bycol = any(apply(self$matrix,2,function(x)all(x %in% n_sorted)))
                     if(any(byrow, bycol)) {self$win <- T}
                     invisible(self)
                   },
                   initialize = function(matrix, id) {
                     self$matrix <- matrix
                     self$id <- id
                   },
                   new_game = function() {
                     self$win <-F
                   },
                   print = function() {
                     cat(self$id)
                     invisible(self)
                   }
                   ))

tables6 <- imap(tables, function(table,id) {
  bingo$new(matrix=table, id=id)
})

check_if_any_win <-  function(extracted_numbers) {
  walk(tables6, function(x) {x$check_win(extracted_numbers)})
  value <- map_lgl(tables6, function(x) x$win)
  walk(tables6, function(x) {x$new_game()})
  value
}

play_bingo <- function(numbers) {
  winners <- NA
  for (i in 1:length(numbers)) {
    winners<-check_if_any_win(numbers[1:i])
    if(any(winners)) {
      extracted<-numbers[1:i]
      break}
  }
  list(winner=which(winners),
       extracted = extracted)
}

end <- play_bingo(numbers)

# get reusult
check_result <- tables6[[end$winner]]$matrix %in% end$extracted %>% matrix(nrow=5)
sum(tables6[[end$winner]]$matrix[!check_result]) * end$extracted[length(end$extracted)]


# part 2 ####
loose_bingo <- function(numbers) {
  winners <- NA
  for (i in 1:length(numbers)) {
    winners<-check_if_any_win(numbers[1:i])
    if(sum(winners)==99) {
      winners99 <- winners
    }
    if(sum(winners)==100) {
      extracted<-numbers[1:i]
      break
    }
  }
  looser <-(!winners99) & winners
  list(looser=which(looser),
       extracted=extracted)
}

end <- loose_bingo(numbers)

# get reusult
check_result <- tables6[[end$looser]]$matrix %in% end$extracted %>% matrix(nrow=5)
sum(tables6[[end$looser]]$matrix[!check_result]) * end$extracted[length(end$extracted)]
