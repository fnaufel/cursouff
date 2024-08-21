
#' Calcular média das atividades
#'
#' @param df Tibble com turma
#' @param a_considerar Quantidade de atividades a considerar. Se NA, considerar todas as notas com nome começando com T0 ou com T1.
#'
#' @return Tibble com nova coluna MA com a média das atividades
#' @author fnaufel
#' @export
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate across all_of if_else rowwise c_across ungroup select
calcular_MA <- function(df, a_considerar = NA) {

  atividades <- df %>% names()
  atividades <- atividades[stringr::str_detect(atividades, '^T[01]')]
  if (is.na(a_considerar)) {
    a_considerar = length(atividades)
  }
  atividades_zero <- paste0(atividades, '_zero')

  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(atividades),
        ~dplyr::if_else(is.na(.x), 0, .x),
        .names = '{.col}_zero'
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      melhores =
        list(
          sort(
            dplyr::c_across(dplyr::all_of(atividades_zero)),
            decreasing = TRUE
          )[1:a_considerar]
        ),
      MA = mean(melhores) %>% round(1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-melhores, -dplyr::all_of(atividades_zero))

}
