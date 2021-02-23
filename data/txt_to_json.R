devtools::load_all()
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

DATADIR <- './work/tmp_data/'
RUNDIR <- 'jan_2021_runs/'

all_mutation_files <- list.files(path = paste0(DATADIR, RUNDIR), 
                                 pattern = "*mutations.txt",
                                 full.names = T,
                                 recursive = T)

find_diff_effect_size_freq_diff <- function(data, p1_freq_var, p2_freq_var, effect_size_var) {
  data %>% 
    dplyr::mutate(effect_size_freq_diff = ({{ p1_freq_var }} - {{ p2_freq_var }}) * {{ effect_size_var }})
}

mutations <- read_delim(all_mutation_files[2], delim=" ") %>% filter(rep == 0)
