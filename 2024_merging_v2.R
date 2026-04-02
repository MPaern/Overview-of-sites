# changed outdir file-----------

library(tidyverse)
library(ggplot2)

# I have all in directory IDfiles/v2
# put it together



files <- list.files(
  path = "data/IDfiles/v2/",
  pattern = "\\.csv$",   # use double backslash for regex
  recursive = TRUE,      # search subfolders
  full.names = TRUE      # return full path
)

all_cm_v2 <- files |>
  lapply(read_csv, col_types = spec_template) |>
  bind_rows()

all_cm_v2 <- files |>
  lapply(function(f) read_csv(f, col_types = spec_template) |> mutate(site = basename(dirname(f)))) |>
  bind_rows()

















# write working csv
write.csv(all_cm_v2, "cm_all_2024_v2.csv")

# read csv in

cm <- read.csv("cm_all_2024_v2.csv")