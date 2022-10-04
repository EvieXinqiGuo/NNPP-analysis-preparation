library(tidyverse)
library(dplyr)
library(stringr)
#### apply the same logic of word count and utterance count
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_audio_transcription")
L = list.files("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_audio_transcription", ".csv")

get_count = function(x) {
  DF = read.csv(x, header = T, na.strings=c("","NA"), sep = ",")
  
  names(DF) = c("Trial3", "Trial4")
  
  DF1 = DF[1:2] %>%
    mutate(wordCountTrial3 = sum(str_count(Trial3, "\\w+"), na.rm = T),
           wordCountTrial4 = sum(str_count(Trial4, "\\w+"), na.rm = T),
           utteranceCountTrial3 = nrow(na.omit(as.data.frame(Trial3))),
           utteranceCountTrial4 = nrow(na.omit(as.data.frame(Trial4)))) %>%
    select(wordCountTrial3, wordCountTrial4, utteranceCountTrial3, utteranceCountTrial4) %>%
    distinct() 
  return(DF1)}

O = lapply(L, get_count)

merged_df =  do.call(rbind, O)
merged_df = cbind(as.data.frame(L) ,merged_df)

write.csv(merged_df,"/Users/guoxinqieve/Applications/OneDrive - UC San Diego/WinterSpring2022_NNPP_transcription/processed_everyone/wordUtteranceCountEveryone.csv", row.names = FALSE)