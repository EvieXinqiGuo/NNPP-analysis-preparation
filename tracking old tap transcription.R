library(readr)
library(tidyverse)
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription.csv")
old_tap_tracking_raw = read_csv("/previous tap tracking - tap_tracking.csv", col_names = FALSE)[, c(1, 2)] # 1 remove the useless column

# finding the index of cells that are associated with the cell that contains digits, which only happens to cells that has participants name and trial number
index_cutoff = which(grepl("[0-9]", old_tap_tracking_raw$X1))
#  1   32   62   86  116  143  175  202  230  251  280  309  335  372  399  427  457  485  508  530  553  581  608
# 630  652  678  705  734  757  789  818  850  879  901  934  961  992 1021 1050 1088 1117 1139 1167 1195 1227 1271
# 1272 1310 1357 1381 1408 1434 1461 1497 1541 1573

plus_1 = function(x) x+1
trial_start= lapply(list(index_cutoff[-length(index_cutoff)]), plus_1)
minus_1 = function(x) x-1
trial_end = lapply(list(index_cutoff[-1]), minus_1)

old_tap_tracking_raw$X1[index_cutoff]

trial_start_and_end= data.frame(trial_start = trial_start, trial_end= trial_end)
names(trial_start_and_end) = c("trial_start", "trial_end")
# 
#getstats<- function(names){
  
#  listofdfs <- list() #Create a list in which you intend to save your df's.
 
filenames = paste0(old_tap_tracking_raw$X1[index_cutoff], ".csv")
filenames = gsub("#", "", filenames)
filenames = gsub(":.*","",filenames)

  for(i in 1:nrow(trial_start_and_end)){
    write.csv(data.frame(old_tap_tracking_raw[trial_start_and_end[i, 1]:trial_start_and_end[i,2], ]), 
              filenames[i], row.names = FALSE)
  }

L = list.files("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription.csv/tracking old tap transcription", ".csv")
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription.csv/tracking old tap transcription")
interleaving = function(x){
  DF = read.csv(x, header = T) # load CSV
  names(DF)[c(1, 2)]= c("first_tap", "second_tap")
  
  idx = order(c(seq_along(DF$first_tap), seq_along(DF$second_tap)))
  
  interleaved_sequence = unlist(c(DF$first_tap,DF$second_tap))[idx]
  
  interleaved_sequence_df = data.frame(interleaved_sequence)
  
  write_csv(interleaved_sequence_df, x, col_names  = FALSE)
}


performance_calcuated = lapply(L, interleaving)