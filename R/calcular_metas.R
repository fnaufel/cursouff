
#' Calcular notas mínimas para alcançar referências
#'
#' @param df Data frame com as notas até agora
#' @param nomes_notas Nomes das notas a considerar
#' @param pesos_notas Pesos das notas a considerar
#' @param peso_restante Peso da nota que falta
#' @param referencias Valores de referência para o cálculo das metas
#'
#' @return Data frame original com colunas adicionais contendo as metas,
#'   uma coluna para cada valor em `referencias`
#'
#' @importFrom purrr map_dfc
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate across everything
#' @export
#'
calcular_metas <-  function(
  df,
  nomes_notas,
  pesos_notas,
  peso_restante = 1 - sum(pesos_notas),
  referencias = c('VS' = 40, 'AP' = 60)
) {


  # Recolher notas em um df
  notas <- df[nomes_notas] %>%
    # Substituir NA por 0
    purrr::map_dfc(~ tidyr::replace_na(., 0))

  matriz <- notas %>% as.matrix()

  # Notas acumuladas até agora (já considerando os pesos)
  acumulado <- as.vector(matriz %*% pesos_notas)

  # Calcular metas
  #
  # metas = (referencias - acumulado) / peso_restante
  #
  # Se meta > 100, NA
  # Se meta <   0,  0
  #
  meta <- referencias %>%
    purrr::map_dfc(
      ~ round(pmax((. - acumulado) / peso_restante, 0), 2)
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ifelse(. > 100, NA, .))
    )

  # Retorna df original, com coluna meta adicionada
  df %>%
    cbind(meta)

}
