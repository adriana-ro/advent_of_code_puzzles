#====================================================================================================================================================#
# notes: Advent of Code Day 1
# author: Adriana Rodriguez 
# data: 12/01/2020
#====================================================================================================================================================#

#========================#
# ==== Load Packages ====
#========================#

  # load packages
  library(data.table)

#====================#
# ==== Load Data ====
#====================#

  # load file 
  puzzle_1 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_1.txt")

#=================#
# ==== Part 1 ====
#=================#

  # get vector of numbers 
  nums <- puzzle_1$V1

  # for every number in the vector of numbers 
  for(i in nums) {
  
    # add the number as a new column to the original file table  
    puzzle_1[, i_num := i]
    
    # sum the original number in the file and the number in the vector
    puzzle_1[, sum := nums + i_num]
    
    # also multiply the numbers 
    puzzle_1[, mult := nums * i_num]
    
    # subset to see if any added to 2020
    any_2020 <- subset(puzzle_1, sum == 2020)
    
    # if the subset is not 0 (i.e. the table finally has 2 numbers that add up to 2020)
    if(nrow(any_2020) != 0) {
      
      # then print the table to get the numbers and the multiplication output 
      print(any_2020) 
      
      # and stop the loop from running since we accomplished our mission
      stop()
    
      # end if statement
      }
  
  # end for loop
  }

#=================#
# ==== Part 2 ====
#=================#

  # create a 3-column table using the list from the file, and create every 3-way combination possible for the numbers 
  table <- data.table(expand.grid(v1 = puzzle_1$V1, v2 = puzzle_1$V1, v3 = puzzle_1$V1))

  # add the 3 numbers and then multiply 
  table[, sum := v1 + v2+ v3]
  table[, mult := v1 * v2* v3]

  # subset to the rows where the sum of the 3 numbers is 2020, then get the multiplication and solved! 
  table[sum == 2020]$mult

