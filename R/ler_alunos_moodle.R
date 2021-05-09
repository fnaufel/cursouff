##' Lê arquivo .xls produzido pelo Moodle
##'
##' Monta uma tibble com os dados da planilha.
##'
##' No Moodle, a planilha é obtida em `Grades > Export: Excel spreadsheet`.
##' Se, em `Grade items to be included` você escolher incluir notas de atividades,
##' estas notas também serão processadas por esta função.
##'
##' @param arquivo Nome do arquivo .xls(x)
##'
##' @return tibble com as colunas
##'   * `email_address`
##'   * `moodle_id` (string)
##'   * `nome` (conteúdo todo em maiúsculas)
##'   * Notas das atividades escolhidas em `Grade items to be included`, no Moodle
##'   * `total`: conteúdo de `course_total`, no Moodle
##'
##' @author Fernando Naufel
##' @export
ler_alunos_moodle <- function(arquivo) {

  alunos_moodle <- readxl::read_excel(
    arquivo
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      moodle_id = id,
      nome = stringr::str_to_upper(paste(first_name, surname)),
      .keep = 'unused'
    ) %>%
    dplyr::select(
      !c(id_number, institution, department, last_downloaded_from_this_course)
    ) %>%
    dplyr::mutate(
      dplyr::across(!c(email_address, moodle_id, nome), as.numeric)
    ) %>%
    dplyr::rename_with(remover_sufixos) %>%
    dplyr::select(email_address, moodle_id, nome, dplyr::everything()) %>%
    dplyr::arrange(nome)

  alunos_moodle

}


remover_sufixos <- function(nome) {

  nome %>%
    stringr::str_remove('^assignment_') %>%
    stringr::str_remove('^course_') %>%
    stringr::str_remove('_real$')

}


