#' @title Atualiza tibble de alunos a partir de novo arquivo do Moodle
#'
#' @description Deve ser chamada para incluir na tibble as notas que foram
#'   atribuídas aos alunos no Moodle.
#'
#'   ATENÇÃO: AS NOTAS CONSIDERADAS SÃO SEMPRE AS DO MOODLE.
#'
#'   Alterações nas notas em `df_antes` serão descartadas.
#'
#'   Se, no resultado, estiver faltando algum aluno do Moodle,
#'   emitir mensagem de erro e abortar.
#'
#' @param df_antes Tibble com dados dos alunos.
#' @param arquivo Caminho do arquivo Excel exportado pelo Moodle via
#'   `Grades > Export: Excel spreadsheet`.
#' @param nomes_notas Vetor nomeado. Os índices são os nomes das
#'   atividades no Moodle (modificados). Os valores são strings com
#'   os nomes que desejamos que apareçam na tibble.
#'
#' @return Se não houve conflitos no join, retorna a tibble atualizada.
#'   Se houve conflitos, termina com erro.
#'
#' @details
#'   * Ignora o email do iduff em favor do email do Moodle. Isto porque os
#'     alunos costumam alterar seus emails no Moodle, enquanto o iduff costuma
#'     ter os emails de quando os alunos eram calouros.
#'   * Se a tibble já tem o campo `moodle_id`, usa este campo como chave do join
#'      para os registros onde ele não é NA.
#'   * Senão, usa o campo `nome` como chave do join e emite aviso a respeito.
#'     Se houver aluno com nome em só uma das fontes (tibble ou arquivo Moodle),
#'     emite erro e cancela a operação.
#'
#' @author fnaufel
#' @export
#' @importFrom dplyr filter rename_with
#' @importFrom tidyselect starts_with
atualizar_moodle <- function(df_antes, arquivo, nomes_notas = NULL) {

  df_moodle <- ler_moodle(arquivo)

  tem_id <- 'moodle_id' %in% names(df_antes)

  if (!tem_id) {
  # Não existe coluna moodle_id em df_antes

    warning(
      'Usando `nome` como chave para TODOS os alunos. Pode haver conflitos.\n'
    )

    # Remover colunas com as notas e email
    df_antes <-
      df_antes %>%
      dplyr::select(-email) %>%
      dplyr::select(matricula:semestre)

    # Fazer join por nome
    df_atual <- fazer_join_por_nome(df_antes, df_moodle)

  } else {

  # Existe a coluna `moodle_id` em df_antes
  # Mas pode haver linhas em df_antes com NA em `moodle_id`
  # (alunos já no iduff mas recém incluídos no moodle)

    # Remover colunas com as notas e email
    df_antes <-
      df_antes %>%
      dplyr::select(-email) %>%
      dplyr::select(matricula:semestre, moodle_id)

    # Dividir df_antes: com moodle_id e sem moodle_id
    df_antes_com_id <- df_antes %>%
      dplyr::filter(!is.na(moodle_id))

    df_antes_sem_id <- df_antes %>%
      dplyr::filter(is.na(moodle_id)) %>%
      dplyr::select(-moodle_id)

    # Fazer join da parte com ids
    df_atual <-
      fazer_join_por_id(df_antes_com_id, df_moodle)

    # Se houver parte sem ids, fazer separado e juntar
    if (nrow(df_antes_sem_id) > 0) {

      warning(
        'Usando `nome` como chave para ALGUNS alunos. Pode haver conflitos.\n'
      )

      df_atual_sem_id <-
        fazer_join_por_nome(df_antes_sem_id, df_moodle)

      df_atual <-
        df_atual %>%
        rbind(df_atual_sem_id) %>%
        arrange(nome)

    }

  }

  # Todos do moodle estão na tibble? Senão, erro
  sem_tibble <- df_moodle %>%
    dplyr::filter(
      !(moodle_id %in% df_atual$moodle_id)
    )

  if (nrow(sem_tibble) > 0) {
    msg <- sem_tibble %>%
      dplyr::pull(nome) %>%
      paste0(collapse = '\n')

    stop(
      paste0(
        'Alunos do Moodle não encontrados na tibble:\n\n',
        msg,
        '\n'
      )
    )
  }

  # Renomear as colunas das notas, se desejado
  if (!is.null(nomes_notas)) {
    df_atual <- df_atual %>%
      dplyr::rename_with(
        .fn = function(x) {
          ifelse(
            x %in% names(nomes_notas),
            nomes_notas[x],
            x
          )
        },
        .cols = tidyselect::starts_with('nota_')
      )
  }

  df_atual %>%
    dplyr::arrange(nome)

}
