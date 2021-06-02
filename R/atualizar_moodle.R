#' @title Atualiza tibble de alunos a partir de novo arquivo do Moodle
#'
#' @description Deve ser chamada para incluir na tibble as notas que foram
#'   atribuídas dos alunos no Moodle.
#'
#' @param df_antes Tibble com dados dos alunos.
#' @param arquivo Caminho do arquivo Excel exportado pelo Moodle via
#'   `Grades > Export: Excel spreadsheet`.
#'
#' @return Se não houve conflitos no join, retorna a tibble atualizada.
#'   Se houve conflitos, termina com erro.
#'
#' @details
#'   * Ignora o email do iduff em favor do email do Moodle. Isto porque os
#'     alunos costumam alterar seus emails no Moodle, enquanto o iduff costuma
#'     ter os emails de quando os alunos eram calouros.
#'   * Se a tibble já tem o campo `moodle_id`, usa como chave do join.
#'   * Senão, usa o campo `nome` como chave do join e emite aviso a respeito.
#'     Se houver aluno com nome em só uma das fontes (tibble ou arquivo Moodle),
#'     emite erro e cancela a operação.
#'
#' @author fnaufel
#' @export
#' @importFrom dplyr select anti_join arrange left_join
atualizar_moodle <- function(df_antes, arquivo) {

  df_depois <- ler_moodle(arquivo)

  # Dar precedência a email do moodle:
  df_antes <- df_antes %>% dplyr::select(-email)

  # Se a tibble já tem moodle_id, usar esta coluna para join.
  # Além disso, usar nome da tibble
  if ('moodle_id' %in% names(df_antes)) {
    coluna <- 'moodle_id'
    df_depois <- df_depois %>% dplyr::select(-nome)
  } else {
    # Senão, tentar usar nome para join e emitir aviso.
    # Se houver problema, cancelar e emitir erro.
    coluna <- 'nome'
    warning('Usando `nome` como identificador. Pode haver conflitos.')

    # Nomes sobrando no moodle
    df_incluidos <- df_depois %>%
      dplyr::anti_join(df_antes, by = coluna) %>%
      dplyr::arrange(nome)

    # Nomes sobrando na tibble
    df_excluidos <- df_antes %>%
      dplyr::anti_join(df_depois, by = coluna) %>%
      dplyr::arrange(nome)

    # Se houver nomes sobrando, emitir erro e sair
    if (nrow(df_incluidos) + nrow(df_excluidos) > 0) {
      msg <- 'Problema com a lista de alunos. Verifique e conserte.\n'
      if (nrow(df_incluidos) > 0) {
        msg <- paste(
          msg,
          '* Alunos só na tibble:\n',
          paste(df_incluidos %>% pull(nome), collapse = '\n')
        )
      }
      if (nrow(df_excluidos) > 0) {
        msg <- paste(
          msg,
          '* Alunos só no Moodle:\n',
          paste(df_excluidos %>% pull(nome), collapse = '\n')
        )
      }
      msg <- paste0(msg, '\n')
      stop(msg)
    }

  }

  # Tudo certo com a coluna do join (nome ou moodle_id)
  # Retornar nova tibble
  df_antes %>%
    dplyr::left_join(df_depois, by = coluna) %>%
    dplyr::arrange(nome)

}
