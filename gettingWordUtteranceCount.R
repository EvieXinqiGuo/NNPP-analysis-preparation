library(tidyverse)
library(dplyr)
library(stringr)
#### apply the same logic of word count and utterance count
setwd("/Users/guoxinqieve/Applications/OneDrive - UC San Diego/WinterSpring2022_NNPP_transcription")
L = list.files("/Users/guoxinqieve/Applications/OneDrive - UC San Diego/WinterSpring2022_NNPP_transcription", ".csv")

O = lapply(L, function(x) {
  DF = read.csv(x, header = T, na.strings=c("","NA"), sep = ",")
  DF1 = DF %>%
    mutate(wordCountTrial3 = sum(str_count(Trial3, "\\w+"), na.rm = T),
           wordCountTrial4 = sum(str_count(Trial4, "\\w+"), na.rm = T),
           utteranceCountTrial3 = nrow(na.omit(as.data.frame(Trial3))),
           utteranceCountTrial4 = nrow(na.omit(as.data.frame(Trial4)))) %>%
    select(wordCountTrial3, wordCountTrial4, utteranceCountTrial3, utteranceCountTrial4) %>%
    distinct() 
  return(DF1)})

merged_df =  do.call(rbind, O)
merged_df = cbind(as.data.frame(L) ,merged_df)

write.csv(merged_df,"/Users/guoxinqieve/Applications/OneDrive - UC San Diego/WinterSpring2022_NNPP_transcription/processed_everyone/wordUtteranceCountEveryone.csv", row.names = FALSE)