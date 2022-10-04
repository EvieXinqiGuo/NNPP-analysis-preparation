##
library(tidyverse)
library(dplyr)
##### load data
##### combine performance and transcribed data
raw_perforamance = read_csv("/Users/guoxinqieve/Applications/OneDrive - UC San Diego/WinterSpring2022_TotalTapTime.csv")
APS_raw = read_csv("/Users/guoxinqieve/Applications/OneDrive - UC San Diego/WinterSpring2022_NNPP_transcription/processed_everyone/wordUtteranceCountEveryone.csv")
##### see if there is an performance reversal effect

##### 
APS = APS_raw %>%
  rename(Participant = L) %>%
  mutate(across(where(is.character), tolower)) 

APS$Participant = substr(APS$Participant, 1, nchar(APS$Participant) - 4) ## this is removing the ".csv" from the name

colnames(APS) = gsub("Trial", "", colnames(APS) )

performance = raw_perforamance %>%
  mutate(across(where(is.character), tolower))  
  
setdiff(performance$Participant, APS$Participant)
APS_performance = inner_join(performance, APS, by = "Participant")

APS_performance = APS_performance %>%
  mutate(competency_tap = ifelse( !is.na(Tap1) & !is.na(Tap2), 
                                  (Tap1 + Tap2)/2, 
                                  max(Tap1, Tap2, na.rm = T)))
#### processing data - getting rid of outliers
#### making everything into lower case for Qualtrics data

library(sjPlot)

set_theme(
  geom.outline.color = "antiquewhite4", 
  geom.outline.size = 1, 
  geom.label.size = 2,
  geom.label.color = "grey50",
  title.color = "black", 
  title.size = 1.5, 
  axis.angle.x = 45, 
  axis.textcolor = "black", 
  base = theme_bw()
)

prepare_APS_performance = function(col) {
  df = APS_performance %>% 
    select(Participant, starts_with(col), competency_tap) %>%
    pivot_longer(cols = -c(Participant, competency_tap),
                 names_prefix = col,
                 names_to = "Sequence", 
                 values_to = col)
  return(df)
}

long_APS = prepare_APS_performance("Tap") %>%
 full_join(prepare_APS_performance("wordCount"), 
            by = c("Participant", "Sequence", "competency_tap")) %>%
  
  full_join(prepare_APS_performance("utteranceCount"), 
            by = c("Participant", "Sequence", "competency_tap")) %>%
  mutate(condition = ifelse(Sequence %in% c("1", "2") , "Neutral", "EPS"))

glimpse(long_APS)
long_APS = long_APS %>%
  mutate(wordRate= wordCount/Tap,
         utteranceRate = utteranceCount/Tap) %>%
  
test_model = lmer(data = long_APS %>% filter(condition == "EPS"), Tap ~ 1 + utteranceCount + (1|Participant))
null_model = lmer(data = long_APS %>% filter(condition == "EPS"), Tap ~ 1  + (1|Participant))

### 
 
anova(null_model, test_model)
plot_model(test_model, type = "pred", terms = c("utteranceRate"))
