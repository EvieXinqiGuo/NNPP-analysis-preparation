# load the dataset(s)

# note to self: "Priscila Colino 1-Table 1.csv" was used to illustrate the point about lucky-match versus unlucky-no-info.

getwd()
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription")

library(tidyverse)
library(readr)
library(stringr)

# remove the table 1 cells
# not necessary, but might need to change the names - remove "table1" in the .csv names

# instead of checking, I'll just remove the cells that contains "Table 1",  string? grepl? gsub?

performance_calculation = function(x){
  
  DF = read.csv(x, header = FALSE)[1] %>%
    filter(V1 != "Table 1") # load CSV
  
  last_tap = max(which(!is.na(DF)))
  
  DF = data.frame(DF[1:last_tap,])
  names(DF) = "card_revealed"
  test_processing = DF %>% 
    mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) %>% # remove the space for words with two words 
    mutate(tap_order = 1:n()) %>% # create a tap order/sequence for later reference 1) the odd and the even are pairs
    mutate(pair_order = (tap_order +1)%/%2) %>% # track back to which are pairs 1) assigning the order of turning.
    mutate(tap_sequence = accumulate(card_revealed, c)) %>% # # accumulating the tap sequence to an ordered string
    mutate(times_appeared = as.numeric(ave(card_revealed, card_revealed, FUN = seq_along)) - 1) %>% # accumulating the times the an element has appeared before
    group_by(pair_order) %>% # grouping by pair_order to figure out if it is a match or not
    mutate(match_or_not = case_when( # seeing if each pair is a match or not
      length(unique(card_revealed)) == 1 ~ "match",
      TRUE ~ "mismatch")) %>%
    mutate(categorized_pair = case_when( 
      match_or_not == "match" & max(times_appeared) ==1 & min(times_appeared) == 0 ~ "lucky_match",
      match_or_not == "mismatch" & max(times_appeared) ==0 ~ "unlucky_no_info", 
      TRUE ~ "other")) %>%
    ungroup()
  
  table_clean = test_processing
  
  participant_performance = as.data.frame(table(table_clean$categorized_pair)) %>%
    rename(categorized_pair = Var1, 
           count_category = Freq)
  
  participant_performance$categorized_pair = factor(participant_performance$categorized_pair, 
                                                    levels = c("unlucky_no_info", "lucky_match", "other"))
  
  n_unluckyNoInfo_lucky = sum(participant_performance$count_category[participant_performance$categorized_pair != "other"])/2
  
  turn_assume_perfect = 24 - n_unluckyNoInfo_lucky
  actual_turn = nrow(table_clean)/2
  performance_difference = actual_turn - turn_assume_perfect
  performance_ratio = actual_turn/turn_assume_perfect
  turns_list = list("actual_turn" = actual_turn, "performance_difference" = performance_difference, "performance_ratio" = performance_ratio, "turn_assume_perfect" = turn_assume_perfect)
  return(turns_list)
}

L = list.files("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription", ".csv")

performance_calcuated = lapply(L, performance_calculation)

performance =  do.call(rbind, performance_calcuated)
merged_df = data.frame(cbind(as.data.frame(L), performance))
merged_df <- apply(merged_df,2,as.character)

write.csv(merged_df,"/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_merged_data/performance.csv", row.names = FALSE)