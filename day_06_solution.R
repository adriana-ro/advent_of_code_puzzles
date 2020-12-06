#====================================================================================================================================================#
# notes: Advent of Code Day 6 
# author: Adriana Rodriguez
# date: 12/06/2020
#====================================================================================================================================================#

#========================#
# ==== Load Packages ====
#========================#

  # load packages 
  library(data.table)
  library(stringr)
  library(stringi)

#====================#
# ==== Load Data ====
#====================#

  # load the file 
  puzzle_6 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_6.txt", header = F)

#=================#
# ==== Part 1 ====
#=================#
  
  #===========================#
  # ==== assign groupings ====
  #===========================#
  
  # first, assign group number to parse information, starting with 0 
  group_num_stored  <- 0
  
  # every row in the puzzle data
  for(i in 1:nrow(puzzle_6)) {
    
    # if the row is empty/NA (and therefore indicating a separation from group 1 to group 2)
    if(all(puzzle_6[i] == "" | is.na(puzzle_6[i]))){
      
      # just set that group number to 999999999
      puzzle_6[i, group_num := 999999999]
      
      # because we don't want to store the 9999, just get the latest stored number
      group_num_stored <- stored_num
      
    }
    
    # if the row value is NOT empty or NA
    else {
      
      # subset to that row value and assign the stored group num and add 1
      puzzle_6[i, group_num := group_num_stored + 1]
    
      # store the number
      stored_num <- puzzle_6[i]$group_num
    }
    
  # end for loop 
  }
  
  # just remove the 999999999 group b/c not needed anymore
  puzzle_6 <- subset(puzzle_6, group_num != 999999999)
  
  #==========================#
  # ==== create function ====
  #==========================#
  
  # start function
  get_q_num_func <- function(in_data, in_group_num){
  
    # get vector
    vector <- paste(in_data[group_num == in_group_num]$V1, collapse = "")
  
    # split the string 
    split_vector <- str_split(vector, "")

    # return only unique values 
    unique_values <- stri_unique(split_vector[[1]])
    
    # get number 
    length <- length(unique_values)
    
    # add questions to the table 
    in_data[group_num == in_group_num, n_questions := length]
  
  # end function
  }
  
  # create xwalk 
  in_xwalk <- data.table(group_num = 1:max(puzzle_6$group_num))
  
  # store data and run function
  purrr::walk(1:nrow(in_xwalk), ~get_q_num_func(in_data = puzzle_6, 
                                            in_group_num = in_xwalk[.x]$group_num))
  
  
  #=======================#
  # ==== final checks ====
  #=======================#

  # deduplicate by group number
  dedupe <- puzzle_6[!duplicated(puzzle_6$group_num)]
  
  # get the sum of all the questions and solved! 
  sum(dedupe$n_questions) # 6532 
   
#=================#
# ==== part 2 ====
#=================#

  # start function
  get_n_yes_num_func <- function(in_data, in_group_num){
  
    # get vector
    vector <- paste(in_data[group_num == in_group_num]$V1, collapse = "")

    # get number of ppl responses 
    n_ppl_responses <- nrow(in_data[group_num == in_group_num])
      
    # split the string 
    split_vector <- str_split(vector, "")

    # create table for the list of vector, giving each letter it's own row 
    table_vector <- data.table(value_letter = split_vector[[1]])
    
    # count the number of times each letter appears in the data set 
    table_vector[, letter_counts := 1: .N, by = "value_letter"]
    
    # get letter where the count matches the number of people responses in the group
    length <- nrow(table_vector[letter_counts == n_ppl_responses])
    
    # add n "yes" questions to the table 
    in_data[group_num == in_group_num, n_yes_questions := length]
  
  # end function 
  }
  
  # store data and run function
  purrr::walk(1:nrow(in_xwalk), ~get_n_yes_num_func(in_data = puzzle_6, 
                                            in_group_num = in_xwalk[.x]$group_num))
  
  #=======================#
  # ==== final checks ====
  #=======================#

  # deduplicate by group number
  dedupe <- puzzle_6[!duplicated(puzzle_6$group_num)]
    
  # get the sum of all the yes, and solved! 
  sum(dedupe$n_yes_questions) # 3427 
   
   