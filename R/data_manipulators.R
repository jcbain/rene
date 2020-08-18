#' Create a frame of of p and q for multiple populations
#'
#' Takes in a vector of columns that reference mutation population frequencies
#'   and converts them to p and q values where q is the original frequency at
#'   population i and p = 1 - q. The resulting frame also indexes the
#'   populations and pivots the table to long format such that each row is a
#'   reference to a single population under certain parameters.
#'
#' @param data A data frame contain 1+ columns of allele frequencies.
#' @param pop_freq_vars A vector of unquoted allele frequency vars.
#' @return A data frame with new columns of `pop`, `p` and `q`.
#' @export
create_population_pq <- function(data, pop_freq_vars) {
  n_pop_cols <- data %>% dplyr::select({{ pop_freq_vars }}) %>% ncol()
  pop_labels <- purrr::imap(1:n_pop_cols, ~ .y) %>% purrr::flatten_chr()

  data %>% dplyr::rename_at(vars({{ pop_freq_vars }}), ~ pop_labels) %>%
    tidyr::pivot_longer(-!dplyr::all_of(pop_labels), names_to="pop", values_to="freq") %>%
    dplyr::mutate_at(vars(pop), as.numeric)
}

#' Append the population sizes for each respective population to a data frame.
#'
#' Use this function when a data frame has a reference to a population (ex:
#'   population_id) but is lacking a population size for each population.
#'
#' @param df Data frame lacking population size column.
#' @param pop_df Data frame containing population size information with key
#'   pertaining to the population.
#' @param left_popref The name of the population reference column of `df`.
#' @param right_popref The name of the population reference column of `pop_df`.
#' @return The original data frame with a population size column.
#' @export
append_popsize = function(df, pop_df, left_popref=pop, right_popref=pop) {
  renamed_popdf <- dplyr::rename(pop_df, pop = {{right_popref}})
  renamed_df <- dplyr::rename(df, pop = {{left_popref}})
  dplyr::left_join(renamed_df, renamed_popdf, by='pop')
}

