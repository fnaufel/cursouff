#' Faz join de `df_antes` com `df_moodle` por nome.
#'
#' O join é feito por nome, porque nenhum registro de `df_antes` tem o campo
#' `moodle_id`.
#'
#' Uma verificação é feita: se, no resultado, houver alunos sem `moodle_id`,
#' uma mensagem é exibida e a função é abortada.
#'
#' O resultado tem os campos `email` e todos os campos de notas com os conteúdos
#' do Moodle, não de `df_antes`.
#'
#' O resultado ainda tem as colunas de notas com os nomes dados por [ler_moodle].
#'
#' @param df_antes tibble com turma anterior.
#' @param df_moodle tibble exportada pelo Moodle, processada por [ler_moodle].
#' @return tibble
#' @author Fernando Naufel
#' @importFrom dplyr left_join filter pull
fazer_join_por_nome <- function(df_antes, df_moodle) {

  # Fazer join por nome
  df_atual <- df_antes %>%
    dplyr::left_join(
      df_moodle,
      by = 'nome'
    ) %>%
    dplyr::arrange(nome)

  # Todos têm moodle_id? Senão, erro
  sem_id <- df_atual %>%
    dplyr::filter(is.na(moodle_id))

  if (nrow(sem_id) > 0) {

    msg <- sem_id %>%
      dplyr::pull(nome) %>%
      paste0(collapse = '\n')

    stop(
      paste0(
        'Alunos da tibble não encontrados por nome no Moodle:\n\n',
        msg,
        '\n'
      )
    )
  }

  df_atual

}
