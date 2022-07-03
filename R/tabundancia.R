#' Gera uma tabela de abundância de espécies
#'
#' @param consultoria nome da tabela que será utilizada
#' @param campanha seleciona a campanha de consultoria, quando não selecionada,
#' inclui todas
#'
#' @return uma tabela em excel
#' @export
#'
#' @examples tabundancia()
tabundancia <- function(consultoria, campanha) {
  consultoria <- readxl::read_excel("C:/Curso R/pacotes/tabela.xlsx") %>%
    janitor::clean_names()
  abundancia <- consultoria %>%
    dplyr::select(campanha, especie) %>%
    dplyr::group_by(campanha, especie) %>%
    dplyr::summarise(
      'individuo' = dplyr::n()) %>%
    tidyr::pivot_wider(
      names_from = campanha,
      values_from = individuo
    ) %>%
    dplyr::arrange(especie)
  tabela <- writexl::write_xlsx(abundancia, "C:/Curso R/pacotes/zoocomr/results/tabela.xlsx")
  tabela}
