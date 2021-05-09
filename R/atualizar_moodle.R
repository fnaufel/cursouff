##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param df_antes
##' @param arquivo
##'
##' @return
##' @author Fernando Naufel
##' @export
atualizar_moodle <- function(df_antes, arquivo) {

  df_depois <- ler_moodle(arquivo)

  # Dar precedência a email do moodle:
  df_antes <- df_antes %>% select(-email)

  # Se a tibble já tem moodle_id, usar esta coluna para join.
  # Além disso, usar nome da tibble
  if ('moodle_id' %in% names(df_antes)) {
    coluna <- 'moodle_id'
    df_depois <- df_depois %>% select(-nome)
  } else {
    # Senão, usar nome para join.
    coluna <- 'nome'
  }

  df_incluidos <- df_depois %>%
    # Linhas de depois que não estão em antes
    dplyr::anti_join(df_antes, by = coluna) %>%
    dplyr::arrange(nome)

  df_excluidos <- df_antes %>%
    # Linhas de antes que não estão em depois
    dplyr::anti_join(df_depois, by = coluna) %>%
    dplyr::arrange(nome)

  df <- df_antes %>%
    dplyr::left_join(df_depois, by = coluna) %>%
    dplyr::arrange(nome)

  if (nrow(df_incluidos) > 0) {
    warning('\nHá alunos no arquivo que não estão na tibble. Verifique.\n')
  }

  if (nrow(df_excluidos) > 0) {
    warning('\nHá alunos na tibble que não estão no arquivo. Verifique.\n')
  }

  list(
    atual = df,
    apenas_moodle = df_incluidos,
    apenas_tibble = df_excluidos
  )


}
