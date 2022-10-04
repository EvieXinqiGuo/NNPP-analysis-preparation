library(readxl)
library(readr)

# Read sheets and use for filenames
sheets <- excel_sheets("/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription.csv/Tap_tracking_NNPP_Fall2022.xlsx")
filenames <- paste0(sheets, ".csv")

# read_excel only reads a single sheet, so lapply over each sheet name
dats <- lapply(sheets, read_excel, path = "/Users/guoxinqieve/Library/CloudStorage/OneDrive-UCSanDiego/Dissertation/NNPP (NO manipulation)/NNPP_data/NNPP_tap_transcription.csv/Tap_tracking_NNPP_Fall2022.xlsx")

# Save each data frame with different filename using write_csv
lapply(seq_along(dats), function(i) write_csv(dats[[i]], filenames[i]))
