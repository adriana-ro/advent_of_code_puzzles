#====================================================================================================================================================#
# notes: Advent of Code Day 7 
# author: Adriana Rodriguez
# date: 12/07/2020
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
  puzzle_7 <- fread("/projects/general/rodriguez/advent_of_code/2020/puzzle_input1_day_7.txt", header = F, fill = T)

#=================#
# ==== Part 1 ====
#=================#
  
  # color columm 
  puzzle_7[, color_1 := V2]
  puzzle_7[, color_1 := V7]
  puzzle_7[, color_3 := V11]
  puzzle_7[, color_3 := V15]
  puzzle_7[, color_3 := V19]
  
  # type 
  puzzle_7[, type_1 := V1]
  puzzle_7[, type_1 := V6]
  puzzle_7[, type_1 := V10]
  puzzle_7[, type_1 := V14]
  puzzle_7[, type_1 := V18]

  # combine 
  puzzle_7[, combined_outer := paste0(V1, "_", V2)]
  puzzle_7[, combined_1 := paste0(V6, "_", V7)]
  puzzle_7[, combined_2 := paste0(V10, "_", V11)]
  puzzle_7[, combined_3 := paste0(V14, "_", V15)]
  puzzle_7[, combined_4 := paste0(V18, "_", V19)]
    
  
  # 
  puzzle_7[combined_1 == "shiny_gold" | combined_2 == "shiny_gold" |
           combined_3 == "shiny_gold" | combined_4 == "shiny_gold",
           flag_has_shiny_gold := 1]
  
  
  # there are 7 colors
  just_shiny_gold <- subset(puzzle_7, flag_has_shiny_gold == 1) 
  # not 3, not 7, not 8, not 6 # not 11 # not 9 # not 25 # not 32 # not 13
  
  # 
  just_shiny_gold[combined_1 == "shiny_gold", flag_sg_1 := 1]
  just_shiny_gold[combined_2 == "shiny_gold", flag_sg_2 := 1]
  just_shiny_gold[combined_3 == "shiny_gold", flag_sg_3 := 1]
  just_shiny_gold[combined_4 == "shiny_gold", flag_sg_4 := 1]
  
  # outer circle 
  just_shiny_gold <- just_shiny_gold[flag_sg_1 == 1, outer_circle := combined_outer]
  just_shiny_gold <- just_shiny_gold[flag_sg_2 == 1, outer_circle := combined_1]
  just_shiny_gold <- just_shiny_gold[flag_sg_3 == 1, outer_circle := combined_2]
  just_shiny_gold <- just_shiny_gold[flag_sg_4 == 1, outer_circle := combined_3]
  
  # get the outer bags 
  #outer_bags <- just_shiny_gold[, outer_bag := paste0(V1, "_", V2)]
  
  
  # get all valid colors 
  all_valid_combos <- c(just_shiny_gold$combined_outer)
  
  #puzzle_7[combined_outer%chin%outer_bags$outer_bag, flag_meets_outer_inner := 1]
  puzzle_7[combined_outer%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner := 1]
  puzzle_7[combined_1%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_2 := 1]
  puzzle_7[combined_2%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_3 := 1]
  puzzle_7[combined_3%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_4 := 1]
  puzzle_7[combined_4%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_5 := 1]
  
  puzzle_7[combined_1%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_2 := 1]
  puzzle_7[combined_2%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_3 := 1]
  puzzle_7[combined_3%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_4 := 1]
  puzzle_7[combined_4%chin%just_shiny_gold$outer_circle, flag_meets_outer_inner_5 := 1]
  
  
  
  just_all <- subset(puzzle_7, flag_has_shiny_gold == 1 | flag_meets_outer_inner == 1)
  just_all <- subset(puzzle_7, flag_has_shiny_gold == 1 | flag_meets_outer_inner == 1 | 
                       flag_meets_outer_inner_2 == 1 | flag_meets_outer_inner_3 == 1 | 
                       flag_meets_outer_inner_4 == 1 | flag_meets_outer_inner_5 == 1)
  
  # get unique colors 
  see <- unique(just_all$V2)
  #see <- unique(puzzle_7$V2)
  
  # get colors 
  colors_1 <-unique(puzzle_7$V2 )
  colors_2 <-unique(puzzle_7$V7 )
  colors_3 <-unique(puzzle_7$V11)
  colors_4 <-unique(puzzle_7$V15)
  colors_5 <-unique(puzzle_7$V19)
  
  # 35 different colors 
  colors_combined <- c(colors_1, colors_2, colors_3, colors_4, colors_5)

  unique(colors_combined)
  
  
  
  #===============#
  # ==== test ====
  #===============#

  
  # 
  test <- copy(puzzle_7)
  
  test <- subset(test, select = c(V1, V2, V6, V7, V10, V11, V14, V15, V18, V19))
  
  # 
  test[, outer_bag := paste0(V1, "_", V2)]
  test[, inner_bag_1 := paste0(V6, "_", V7)]
  test[, inner_bag_2 := paste0(V10, "_", V11)]
  test[, inner_bag_3 := paste0(V14, "_", V15)]
  test[, inner_bag_4 := paste0(V18, "_", V19)]
  
  
  
  test <- subset(test, select = -c(V1, V2, V6, V7, V10, V11, V14, V15, V18, V19))
  
  # 
  test[inner_bag_1 == "shiny_gold", flag_ib1 := 1]  
  test[inner_bag_2 == "shiny_gold", flag_ib2 := 1]  
  test[inner_bag_3 == "shiny_gold", flag_ib3 := 1]  
  test[inner_bag_4 == "shiny_gold", flag_ib4 := 1]  
  
  test[flag_ib1 == 1, ':=' (first_valid_color = outer_bag)]
  test[flag_ib2 == 1, ':=' (first_valid_color = outer_bag, second_valid_color = inner_bag_1)]
  test[flag_ib3 == 1, ':=' (first_valid_color = outer_bag, second_valid_color = inner_bag_1, third_valid_color = inner_bag_2)]
  test[flag_ib4 == 1, ':=' (first_valid_color = outer_bag, second_valid_color = inner_bag_1, third_valid_color = inner_bag_2, fourth_valid_color = inner_bag_3)]
  
  
  # 
  just_all_colors <- unique(c(test$outer_bag, test$inner_bag_1, test$inner_bag_2, test$inner_bag_3, test$inner_bag_4))
  
  all_1 <- unique(test$first_valid_color )
  all_2 <- unique(test$second_valid_color)
  all_3 <- unique(test$third_valid_color )
  all_4 <- unique(test$fourth_valid_color)
  
  
  colors <- unique(c(all_1, all_2, all_3, all_4))
  test[, ]
  