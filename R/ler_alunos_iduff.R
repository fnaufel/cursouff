##' Lê arquivo .xls produzido pelo iduff
##'
##' Monta uma tibble com os dados da planilha.
##'
##' Conteúdo da planilha:
##'
##' * Linhas 1 a 6 têm `disciplina`, `código`, `turma` e `período`.
##'
##' * Linha 7 tem os cabeçalhos das colunas: `matrícula`, `cpf`, `nome`, `email`.
##'
##' @param arquivo Nome do arquivo .xls
##'
##' @return tibble com as colunas
##'   * `matricula`
##'   * `cpf`
##'   * `nome`
##'   * `email`
##'   * `disciplina`
##'   * `codigo`
##'   * `turma`
##'   * `ano`
##'   * `semestre`
##'   * `ativo`: todas as linhas contendo TRUE
##'
##' @author Fernando Naufel
##' @export
ler_alunos_iduff <- function(arquivo) {

  info <- readxl::read_excel(
    arquivo,
    col_names = c('item', 'vazio', 'valor'),
    skip = 1,
    n_max = 4
  ) %>%
    dplyr::select(item, valor) %>%
    dplyr::mutate(item = stringr::str_trim(item))

  alunos <- readxl::read_excel(
    arquivo,
    skip = 6
  ) %>%
    dplyr::rename(nome = 'Nome do Aluno') %>%
    janitor::clean_names() %>%
    dplyr::rename(email = e_mail) %>%
    dplyr::mutate(
      disciplina = info %>% dplyr::filter(item == 'Disciplina') %>%
        dplyr::pull(valor),
      codigo     = info %>% dplyr::filter(item == 'Código') %>%
        dplyr::pull(valor),
      turma      = info %>% dplyr::filter(item == 'Turma') %>%
        dplyr::pull(valor),
      ano        = info %>% dplyr::filter(item == 'Período') %>%
        dplyr::pull(valor) %>% stringr::str_sub(end = -2) %>% as.integer(),
      semestre   = info %>% dplyr::filter(item == 'Período') %>%
        dplyr::pull(valor) %>% stringr::str_sub(start = -2) %>% as.integer(),
      ativo = TRUE
    ) %>%
    dplyr::arrange(nome)

  alunos

}


