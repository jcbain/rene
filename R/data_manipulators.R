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

#' Join template genome position to a single parameter
#' of mutations
#'
#' The default output mutations doesn't include a value for
#'  neutral positions as a way to save resources when simulations
#'  are running. This function joins on the template genome
#'  (essentially a template for the positions on the genome) to
#'  the mutation data so that each position per each generation
#'  has a row. This may or may not be the appropriate method
#'  depending on how you plan to use the data next.
#'
#' @param mutation_df A data frame of mutations for a given
#'  parameter set
#' @param template A data frame of all the positions on the
#'  genome corresponding to the mutations
#' @param generation_var The variable in the `mutation_df`
#'  corresponding to the the output generation variable
#' @param param_vars A vector of parameter variable names in
#'  the `mutation_df` frame
#' @return An expanded mutation data frame with empty (neutral)
#'  position rows
#' @export
expand_genome_to_template <- function(mutation_df, template, generation_var, param_vars) {
  uniq_generations <- mutation_df %>%
    dplyr::group_by({{ generation_var }}) %>%
    dplyr::summarize() %>% dplyr::pull()

  purrr::map(uniq_generations, function(gen) {
    mutation_generation <- mutation_df %>%
      dplyr::filter({{generation_var}} == gen)
    mutation_params <- mutation_df %>%
      dplyr::select(c({{ param_vars }})) %>% head(1)
    mutation_without_params <- mutation_df %>%
      dplyr::select(-c({{ param_vars }}))

    num_param_cols <- ncol(mutation_params)
    num_template_rows <- nrow(template)

    template_param_rows <- mutation_params %>%
      dplyr::slice(rep(1:num_param_cols, each = num_template_rows))
    full_template <- dplyr::bind_cols(template, template_param_rows)
    dplyr::left_join(full_template, mutation_generation)
  }) %>% purrr::reduce(dplyr::bind_rows)
}


