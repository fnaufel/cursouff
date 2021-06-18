#' @title Atualiza tibble de alunos a partir de novo arquivo do Moodle
#'
#' @description Deve ser chamada para incluir na tibble as notas que foram
#'   atribuídas dos alunos no Moodle.
#'
#' @param df_antes Tibble com dados dos alunos.
#' @param arquivo Caminho do arquivo Excel exportado pelo Moodle via
#'   `Grades > Export: Excel spreadsheet`.
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
#' @importFrom dplyr left_join arrange filter select bind_rows pull rename
atualizar_moodle <- function(df_antes, arquivo) {

  df_moodle <- ler_moodle(arquivo)

  tem_id <- 'moodle_id' %in% names(df_antes)

  # Não existe coluna moodle_id em df_antes
  if (!tem_id) {

    warning('Usando `nome` como chave. Pode haver conflitos.\n')
    testar_join_por_nome(df_antes, df_moodle)

    df_atual <- df_antes %>%
      dplyr::left_join(df_moodle, by = 'nome') %>%
      dplyr::arrange(nome)

  } else {
    # Existe a coluna `moodle_id` em df_antes
    # Mas pode haver linhas em df_antes com NA em `moodle_id`
    # (alunos já no iduff mas recém incluídos no moodle)
    # Dividir df_antes: com moodle_id e sem moodle_id
    df_antes_com_id <- df_antes %>%
      dplyr::filter(!is.na(moodle_id))

    df_antes_sem_id <- df_antes %>%
      dplyr::filter(is.na(moodle_id))

    # Alunos com id: join por id
    df_atual1 <- df_antes_com_id %>%
      dplyr::left_join(
        # Usar os nomes dos alunos da tibble antes, não do moodle
        df_moodle %>% dplyr::select(-nome),
        by = 'moodle_id'
      )

    # Alunos sem id: join por nome
    warning(
      'Usando `nome` como chave para alguns alunos. Pode haver conflitos.\n'
    )
    df_atual2 <- df_antes_sem_id %>%
      # Tirar essa coluna de NAs para substituir pela coluna do moodle
      dplyr::select(-moodle_id) %>%
      dplyr::left_join(
        df_moodle,
        by = 'nome'
      )

    # Juntar tudo
    df_atual <-
      dplyr::bind_rows(df_atual1, df_atual2) %>%
      dplyr::arrange(nome)

    # Todos têm moodle_id? Senão, erro
    sem_id <- df_atual %>%
      dplyr::filter(is.na(moodle_id))
    if (nrow(sem_id) > 0) {
      msg <- sem_id %>% dplyr::pull(nome) %>% paste0(collapse = '\n')
      stop(
        paste0(
          'Alunos da tibble não encontrados por nome no Moodle:\n\n',
          msg,
          '\n'
        )
      )
    }

    # Todos do moodle estão na tibble? Senão, erro
    sem_tibble <- df_moodle %>%
      dplyr::filter(!(moodle_id %in% df_atual$moodle_id))
    if (nrow(sem_tibble) > 0) {
      msg <- sem_tibble%>% dplyr::pull(nome) %>% paste0(collapse = '\n')
      stop(
        paste0(
          'Alunos do Moodle não encontrados na tibble:\n\n',
          msg,
          '\n'
        )
      )
    }

  }

  # Usar o email do moodle em vez do iduff
  df_atual %>%
    dplyr::rename(email = email.y) %>%
    dplyr::select(-email.x)

}
