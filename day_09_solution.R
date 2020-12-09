#====================================================================================================================================================#
# notes: Advent of Code Day 9
# author: Adriana Rodriguez
# date: 12/09/2020
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
  puzzle_9 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_9.txt", header = F, fill = T)

#=================#
# ==== Part 1 ====
#=================#
 
  ## example puzzle  
  #puzzle_9 <- data.table(V1 = c(35,
  #                              20,
  #                              15,
  #                              25,
  #                              47,
  #                              40,
  #                              62,
  #                              55,
  #                              65,
  #                              95,
  #                              102,
  #                              117,
  #                              150,
  #                              182,
  #                              127,
  #                              219,
  #                              299,
  #                              277,
  #                              309,
  #                              576))
  #
  
 
  # make copy 
  copy_puzzle <- copy(puzzle_9)
  
  # set i to 1 
  i = 1 
  
  # start repeat loop
  repeat {
    
  # get preamble 
  preamble_nums <- copy_puzzle[1:25,]
  
  # cbind the string of first 25 numbers with itself and expand to get every possible combination
  preamble_nums_expand <- as.data.table(expand.grid(V1 = preamble_nums$V1, V2 = preamble_nums$V1))
  
  # remove numbers that are the same 
  preamble_nums_expand <- subset(preamble_nums_expand, V1 != V2)
  
  # sum 
  preamble_nums_expand[, sum_nums := V1 + V2]
  
  # get valid sums 
  valid_sums <- unique(preamble_nums_expand$sum_nums)
  
  # other numbers 
  other_nums <- copy_puzzle[26:nrow(copy_puzzle),]

  # add validity flag 
  other_nums[V1%chin%valid_sums, flag_is_valid := 1]
  
  # if the first (i) row of the other nums table is NA and therefore invalid  
  if(is.na(other_nums[i]$flag_is_valid))   {
  
    # print the invalid number message 
    stop(paste0("invalid number is ", other_nums[i]$V1))
    
    
  } else { 
    
    # remove the first row from preamble b/c it no longer counts towards the first 25 valid numbers 
    copy_puzzle <- copy_puzzle[2:nrow(copy_puzzle),]
    
    }
  
  # end repeat function 
  }

      
#=================#
# ==== part 2 ====
#=================#

  # set initial numbers 
  first_num = 1
  second_num = 2
  
  # start repeat loop
  repeat {
  
  # get the sum of all the numbers between the first num threshold and second num threshold
  get_sum <- sum(as.numeric(unlist(str_match_all(puzzle_9$V1[first_num:second_num], "[0-9]+"))))
  
  # if the sum number equals the invalid number from part 1
  if(get_sum == other_nums[i]$V1){
    
    # stop the repeat loop and print out the sum of the min and max number 
    stop(paste0("first num position ", first_num, " second num position = ", second_num, 
                 " where the sum of highest and smallest numbers in that string is ", 
                 (max(puzzle_9$V1[first_num:second_num]) +  min(puzzle_9$V1[first_num:second_num]))
                 ))
     
    
  }
   
  # if the second num reaches the max threshold, time to start looking at the next threshold for first num, else just add 1  
  if(second_num == nrow(puzzle_9)) { first_num = first_num + 1} else { first_num = first_num}
  
  # if the second num reaches the max threshold, time to go back to 2, else just add 1 
  if(second_num == nrow(puzzle_9)) { second_num =  2} else { second_num = second_num + 1}
   
   
  # end repeat loop 
  }
    
    