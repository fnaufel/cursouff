#' @title Encurta conteúdo da coluna `nome` de um df
#' @param df Uma tibble com a coluna `nome`
#' @param n Quantidade de nomes a manter em cada linha
#'   (Prenome + n-1 sobrenomes)
#' @return Tibble com a coluna `nome` modificada
#' @author fnaufel
#' @export
#' @importFrom dplyr pull mutate arrange
#' @importFrom stringr word
encurtar_nomes <- function(df, n = 2) {

  if (n < 2) stop('n precisa ser >= 2')

  nomes <- df %>% dplyr::pull(nome)

  prenomes <- nomes %>% stringr::word(start = 1)

  sobrenomes <- nomes %>% stringr::word(start = -(n - 1), end = -1)

  if (any(is.na(sobrenomes)))
    stop('Há nomes curtos demais. Use n menor.')

  df %>%
    dplyr::mutate(nome = paste(prenomes, sobrenomes)) %>%
    dplyr::arrange(nome)

}
