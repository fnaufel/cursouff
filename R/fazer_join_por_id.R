#' Faz join de `df_antes` com `df_moodle` por `moodle_id`.
#'
#' O resultado ainda tem as colunas de notas com os nomes dados por [ler_moodle].
#'
#' @param df_antes tibble com turma anterior.
#' @param df_moodle tibble exportada pelo Moodle, processada por [ler_moodle].
#' @return tibble.
#' @author Fernando Naufel
#' @importFrom dplyr left_join filter pull
fazer_join_por_id <- function(df_antes, df_moodle) {

  df_atual <- df_antes %>%
    dplyr::left_join(
      # Usar os nomes dos alunos da tibble antes, não do moodle
      df_moodle %>% dplyr::select(-nome),
      by = 'moodle_id'
    )

  df_atual

}
