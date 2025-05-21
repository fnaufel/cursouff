#' @title Atualiza tibble de alunos a partir de novo arquivo .xls do iduff
#'
#' @description Inclui na tibble os alunos do novo arquivo que não estavam na
#'   tibble e remove da tibble os alunos que não aparecem no novo arquivo.
#'
#' @param df_antes Tibble com a turma original (com campos do iduff, pelo
#'   menos; talvez com os campos do moodle também).
#'
#' @param arquivo Nome do arquivo .xls exportado pelo iduff.
#'
#' @return Lista com 3 tibbles:
#'   * `$atual`: tibble atualizada
#'   * `$incluidos`: tibble com novos alunos
#'   * `$excluidos`: tibble com alunos excluídos
#'
#' @author fnaufel
#' @export
#' @importFrom dplyr anti_join arrange mutate rows_insert rows_delete left_join
#' @importFrom rlang is_empty
atualizar_iduff <- function(df_antes, arquivo) {

  if (! 'matricula' %in% names(df_antes))
    stop('Tibble deve ter campo matricula!')

  df_depois <- ler_iduff(arquivo)

  # Os joins vão ser feitos por matricula
  coluna <- 'matricula'

  df_incluidos <- df_depois %>%
    # Linhas de depois que não estão em antes
    dplyr::anti_join(df_antes, by = coluna) %>%
    dplyr::arrange(nome)

  df_excluidos <- df_antes %>%
    # Linhas de antes que não estão em depois
    dplyr::anti_join(df_depois, by = coluna) %>%
    dplyr::arrange(nome)

  # Se houver excluídos, deletar linhas de df_antes.
  # O teste é necessário, pois parece que rows_delete dá problema
  # com tibble vazia
  if (nrow(df_excluidos) > 0) {
    df_antes <- df_antes %>%
    dplyr::rows_delete(df_excluidos, by = coluna)
  }

  # Agora, acrescentar colunas que existam em df_depois mas não em df_antes.
  # Aqui, só as linhas de df_antes. Linhas novas de df_depois são inseridas
  # no próximo passo.
  novas_colunas <- setdiff(names(df_depois), names(df_antes))
  if (!rlang::is_empty(novas_colunas)) {

    df_novas_colunas <- df_depois %>%
      dplyr::select(dplyr::all_of(c(coluna, novas_colunas)))

    df_antes <- df_antes %>%
      dplyr::left_join(
        df_novas_colunas,
        by = coluna
      )
  }

  # Importante: alunos já presentes na tibble não têm nenhum campo alterado
  # (a não ser possíveis novas colunas adicionadas no passo anterior).
  df <- df_antes %>%
    # Inserir linhas novas
    dplyr::rows_insert(df_incluidos, by = coluna) %>%
    dplyr::arrange(nome)

  list(
    atual = df,
    incluidos = df_incluidos,
    excluidos = df_excluidos
  )

}

