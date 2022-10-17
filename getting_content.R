library(tidyverse)
library(dplyr)
library(stringr)
#### apply the same logic of word count and utterance count
setwd("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_utterance_categorization")
L = list.files(getwd(), ".csv")
# figuring which files do not start from "trial 3" or "trial 4"

get_content = function(x) {
  DF = read.csv(x, header = F, na.strings=c("","NA"), sep = ",") %>%
    mutate_if(is.character, tolower) %>% 
    mutate_if(is.character, gsub, pattern =" ", replace = "") %>% ## removing blank space
  mutate_if(is.character, gsub, pattern ="recognization", replace = "recognition")# typo during categorization
  start_index = which(DF[,1]=="trial3") 
  DF = DF[(start_index+1):nrow(DF),1:2]
  names(DF) = c("Trial3", "Trial4")
  trial3 = DF %>% count(Trial3) %>% mutate(trial = "3")
  trial4 = DF %>% count(Trial4) %>% mutate(trial = "4")
  names(trial3) = c("content_type", "count", "trial")
  names(trial4) = c("content_type", "count", "trial")
  content = as.data.frame(x) %>%
    cbind(rbind(trial3, trial4))
  names(content)[1] = "Participant"
  return(content)
  }

test = lapply(L, get_content)

merged_df =  do.call(rbind, test) %>%
  na.omit()

write.csv(merged_df,"/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_merged_data/content_Everyone.csv", row.names = FALSE)
