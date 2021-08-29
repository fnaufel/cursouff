#' @title Constrói tabela de notas
#'
#' @param df Data frame
#'
#' @param legenda Vetor de strings para incluir no rodapé da tabela, Default: NULL
#'
#' @param incl_matr Boolean: incluir números de matrícula?, Default: FALSE
#'
#' @param incl_nome Boolean: incluir nomes dos alunos, Default: TRUE
#'
#' @param ordem Coluna para ordenar, Default: ifelse(incl_nome, "nome", "matricula")
#'
#' @return Tabela kable
#'
#' @author fnaufel
#'
#' @export
#'
#' @importFrom dplyr setdiff arrange all_of select any_of
#' @importFrom kableExtra kbl kable_paper footnote
tab_notas <- function(
  df,
  legenda = NULL,
  incl_matr = FALSE,
  incl_nome = TRUE,
  ordem = ifelse(incl_nome, 'nome', 'matricula')
) {

  colunas <- names(df)

  col_excluir <- c(
    'nome',
    'matricula',
    'cpf',
    'email',
    'disciplina',
    'codigo',
    'turma',
    'ano',
    'semestre',
    'moodle_id'
  )

  col_incluir <- dplyr::setdiff(colunas, col_excluir)

  if (incl_matr)
    col_incluir <- c('matricula', col_incluir)

  if (incl_nome)
    col_incluir <- c('nome', col_incluir)

  tabela <- df %>%
    dplyr::arrange(.data[[ordem]]) %>%
    dplyr::select(dplyr::any_of(col_incluir)) %>%
    kableExtra::kbl(
      format.args = list(big.mark = '.', nsmall = 2)
    ) %>%
    kableExtra::kable_paper(
      c('striped', 'hover'),
      full_width = FALSE
    )

  if (!is.null(legenda)) {
    tabela <- tabela %>%
      kableExtra::footnote(
        general = legenda,
        general_title = 'Legenda:\n',
        title_format = 'bold'
      )
  }

  tabela

}

