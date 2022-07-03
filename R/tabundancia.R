#' Gera uma tabela de abundância de espécies
#'
#' @param campanha coluna que designa o indicador ordinal referente a campanha
#' @param x nome do data frame que será utilizado no arquivo
#' @param especie coluna com a identificacao de todos os individuos coletados
#'
#' @return uma tabela em excel
#' @export
#'
#' @examples tabundancia(consultoria)
tabundancia <- function(x, campanha = campanha, especie = especie) {
  abundancia <- x %>%
    dplyr::select({{campanha}}, {{especie}}) %>%
    dplyr::group_by({{campanha}}, {{especie}}) %>%
    dplyr::summarise(
      'individuo' = dplyr::n()) %>%
    tidyr::pivot_wider(
      names_from = {{campanha}},
      values_from = individuo
    ) %>%
    dplyr::arrange({{especie}})
  tabela <- writexl::write_xlsx(abundancia, "C:/Curso R/pacotes/zoocomr/results/tabela.xlsx")
  tabela}
