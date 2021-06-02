#' @title Lê arquivo .xls produzido pelo Moodle
#'
#' @description Monta uma tibble com os dados da planilha.
#'
#' @details No Moodle, a planilha é obtida em `Grades > Export: Excel spreadsheet`.
#' Se, em `Grade items to be included`, você escolher incluir notas de atividades,
#' estas notas também serão processadas por esta função.
#'
#' @param arquivo Nome do arquivo .xls(x).

#' @return tibble com as colunas
#'   * `email_address`
#'   * `moodle_id` (string)
#'   * `nome` (conteúdo todo em maiúsculas)
#'   * Notas das atividades escolhidas em `Grade items to be included`, no Moodle.
#'     Os nomes das notas são alterados pela função [mudar_nomes()].
#'   * `nota_final`: conteúdo de `course_total`, no Moodle
#'
#' @author fnaufel
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate rename select across rename_with everything arrange
#' @importFrom stringr str_to_upper
ler_moodle <- function(arquivo) {

  alunos_moodle <- readxl::read_excel(
    arquivo
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      moodle_id = id,
      nome = stringr::str_to_upper(paste(first_name, surname)),
      .keep = 'unused'
    ) %>%
    dplyr::rename(email = email_address) %>%
    dplyr::select(
      !c(id_number, institution, department, last_downloaded_from_this_course)
    ) %>%
    dplyr::mutate(
      dplyr::across(!c(email, moodle_id, nome), as.numeric)
    ) %>%
    dplyr::rename_with(mudar_nomes) %>%
    dplyr::select(email, moodle_id, nome, dplyr::everything()) %>%
    dplyr::arrange(nome)

  alunos_moodle

}


#' Mudar nomes das colunas onde o moodle põe as notas
#'
#' @param nome Nome da coluna (vetorizado).
#'
#' @return Nome(s) da(s) coluna(s) alterado(s).
#'   * `assignment_` ➜ `nota_`
#'   * `course_total` ➜ `nota_final`
#'   * O sufixo `_real`, se estiver presente, é removido
#'
#' @importFrom stringr str_replace str_remove
mudar_nomes <- function(nome) {

  nome %>%
    stringr::str_replace('^assignment_', 'nota_') %>%
    stringr::str_replace('^course_total', 'nota_final') %>%
    stringr::str_remove('_real$')

}
