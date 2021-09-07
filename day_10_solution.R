#====================================================================================================================================================#
# notes: Advent of Code Day 10
# author: Adriana Rodriguez
# date: 12/10/2020
#====================================================================================================================================================#

#========================#
# ==== Load Packages ====
#========================#

  # load packages 
  library(data.table)
  library(stringr)

#====================#
# ==== Load Data ====
#====================#

  # load the file 
  puzzle_10 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_10.txt", header = F, fill = T)

#=================#
# ==== Part 1 ====
#=================#
 
  
  # 
  puzzle_10 <- data.table(V1 = c(16,
                                 10,
                                 15,
                                 5,
                                 1,
                                 11,
                                 7,
                                 19,
                                 6,
                                 12,
                                 4))
  
  
  # add 0 
  add_0 <- data.table(V1 = 0)
  
  # stack 
  puzzle_10 <- rbind(puzzle_10, add_0)
  
  # sort 
  setorder(puzzle_10, "V1")  
  
  # make copy 
  puzzle_copy <- setnames(puzzle_10[2:nrow(puzzle_10)], "V1", "V2")
  
  # bind 
  puzzle_bind <- cbind(puzzle_10, puzzle_copy)
  
  # recode last place 
  puzzle_bind[nrow(puzzle_10), V2 := V1 + 3]
  
  # get difference 
  puzzle_bind[, diff := V2 - V1]
  
  # multiply the number of times the diff is 1 and number of times diff is 3 and solved! 
  nrow(puzzle_bind[diff == 1]) * nrow(puzzle_bind[diff == 3])
  
  