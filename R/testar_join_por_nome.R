#' @importFrom dplyr anti_join arrange pull
testar_join_por_nome <- function(df1, df2) {

  # Nomes sobrando no moodle
  sobra_moodle <- df2 %>%
    dplyr::anti_join(df1, by = 'nome') %>%
    dplyr::arrange(nome)

  # Nomes sobrando na tibble
  sobra_tibble <- df1 %>%
    dplyr::anti_join(df2, by = 'nome') %>%
    dplyr::arrange(nome)

  # Se houver nomes sobrando, emitir erro e sair
  if (nrow(sobra_moodle) + nrow(sobra_tibble) > 0) {
    msg <- 'Problema com a lista de alunos. Verifique e conserte.'
    if (nrow(sobra_moodle) > 0) {
      msg <- paste0(
        msg,
        '\n\n* Alunos só no Moodle:\n\n',
        paste0(sobra_moodle %>% dplyr::pull(nome), collapse = '\n')
      )
    }
    if (nrow(sobra_tibble) > 0) {
      msg <- paste0(
        msg,
        '\n\n* Alunos só na tibble:\n\n',
        paste0(sobra_tibble %>% dplyr::pull(nome), collapse = '\n')
      )
    }
    stop(msg)
  }

}
