##' Atualiza tibble de alunos a partir de novo arquivo .xls do iduff
##'
##' Inclui os alunos do novo arquivo que não estavam na tibble e
##' marca como inativos os alunos que não aparecem no novo arquivo.
##'
##' Note que os alunos excluídos não são deletados da tibble, mas apenas marcados como inativos.
##'
##' Note também que função retorna uma lista com 3 tibbles: a turma atual, os alunos novos, e os alunos excluídos.
##'
##' @param df_antes tibble com a turma original
##' @param arquivo Nome do arquivo .xls com a turma atual
##'
##' @return Lista com 3 tibbles:
##'   * turma atual (com alunos excluídos marcados como inativos)
##'   * alunos novos
##'   * alunos excluídos
##'
##' @author Fernando Naufel
##' @export
atualizar_alunos_iduff <- function(df_antes, arquivo) {

  df_depois <- ler_alunos_iduff(arquivo)

  df_incluidos <- df_depois %>%
    dplyr::anti_join(df_antes, by = 'matricula') %>%
    dplyr::arrange(nome)

  df_excluidos <- df_antes %>%
    dplyr::anti_join(df_depois, by = 'matricula') %>%
    dplyr::mutate(ativo = FALSE) %>%
    dplyr::arrange(nome)

  df <- df_antes %>%
    dplyr::rows_insert(df_incluidos, by = 'matricula') %>%
    dplyr::rows_update(df_excluidos, by = 'matricula') %>%
    dplyr::arrange(nome)

  list(
    atual = df,
    incluidos = df_incluidos,
    desativados = df_excluidos
  )

}

# TODO: se o df passado não tiver dados do iduff, fazer join por email
