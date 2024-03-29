% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ler_moodle.R
\name{ler_moodle}
\alias{ler_moodle}
\title{Lê arquivo .xls produzido pelo Moodle}
\usage{
ler_moodle(arquivo)
}
\arguments{
\item{arquivo}{Nome do arquivo .xls(x).}
}
\value{
tibble com as colunas
\itemize{
\item \code{email_address}
\item \code{moodle_id} (string)
\item \code{nome} (conteúdo todo em maiúsculas)
\item Notas das atividades escolhidas em \verb{Grade items to be included}, no Moodle.
Os nomes das notas são alterados pela função \code{\link[=mudar_nomes]{mudar_nomes()}}.
\item \code{nota_final}: conteúdo de \code{course_total}, no Moodle
}
}
\description{
Monta uma tibble com os dados da planilha.
}
\details{
No Moodle, a planilha é obtida em \verb{Grades > Export: Excel spreadsheet}.
Se, em \verb{Grade items to be included}, você escolher incluir notas de atividades,
estas notas também serão processadas por esta função.
}
\author{
fnaufel
}
