#====================================================================================================================================================#
# notes: Advent of Code Day 2 
# author: Adriana Rodriguez
# date: 12/02/2020
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
  puzzle_2 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_2.txt", header = F)

#=================#
# ==== Part 1 ====
#=================#

  # scan for the first number threshold from the V1 string, then the second number 
  puzzle_2[, rules_min_num :=  transpose(str_split(V1,"-"))[1]]  
  puzzle_2[, rules_max_num :=  transpose(str_split(V1,"-"))[2]] 
  
  # remove the ":" from the V2 col letter string
  puzzle_2[, rules_letter := gsub(":", "", V2)]
  
  # recode V3 column name as password to make it easier to know what I'm referring to 
  setnames(puzzle_2, "V3", "password")
  
  # get the N number of times the specific letter appears in the password 
  puzzle_2[, letter_n_count := str_count(password, rules_letter)]

  # set initial flag of zero and make it 1 for cases where the password meets the rules requirements 
  puzzle_2[, flag_meets_reqs := 0]
  puzzle_2[as.numeric(letter_n_count) >= as.numeric(rules_min_num) & 
             as.numeric(letter_n_count) <= as.numeric(rules_max_num), flag_meets_reqs := 1]

  # get the number of rows in the file that meet the password requirement.. and solved! 
  nrow(puzzle_2[flag_meets_reqs == 1])

#=================#
# ==== Part 2 ====
#=================#

  # for every row in the puzzle file 
  for(i in 1:nrow(puzzle_2)){ 
    
    # get the letter in the password that is associated with the min and max position of the password rules
    puzzle_2[i , position_1 := transpose(str_split(password,""))[as.numeric(rules_min_num)]]
    puzzle_2[i , position_2 := transpose(str_split(password,""))[as.numeric(rules_max_num)]]
    
    # end for loop
    }

  # first validation rule - no same two letters can be in the positions  
  puzzle_2[, flag_diff_letter := 0]
  puzzle_2[position_1 != position_2 , flag_diff_letter := 1]

  # second validation rules - the exact rule letters needs to be in one of the rules position 
  puzzle_2[, flag_exact_letter := 0]
  puzzle_2[position_1 == rules_letter | position_2 == rules_letter, flag_exact_letter := 1]

  # set initional meets reqs flag to 0, and set to 1 when both validation rules are met
  puzzle_2[, flag_meets_reqs2 := 0]
  puzzle_2[flag_diff_letter == 1 & flag_exact_letter == 1, flag_meets_reqs2 := 1]
  
  # get the number of rows in the file that meet the password requirement part 2.. and solved! 
  nrow(puzzle_2[flag_meets_reqs2 == 1])
  