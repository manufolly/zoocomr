#' Gera um gráfico de abundância de espécies
#'
#' @param consultoria nome da tabela que será utilizada
#' @param campanha seleciona a campanha de consultoria, quando não selecionada,
#' inclui todas
#'
#' @return um gráfico
#' @export
#'
#' @examples gabundancia()
gabundancia <- function(consultoria, effort) {
  consultoria <- readxl::read_excel("C:/Curso R/pacotes/tabela.xlsx") %>%
    janitor::clean_names()
  grafico <-consultoria %>%
    dplyr::filter(campanha == effort) %>%
    dplyr::select(campanha, especie) %>%
    dplyr::count(campanha, especie) %>%
    ggplot2::ggplot() +
    geom_col(aes(x = especie, y = n), fill = "#ADC6CB") +
    scale_y_continuous(expand = expansion(mult = c(0, .025))) +
    labs(x = "Espécies",
         y = "Abundância") +
    ggthemes::theme_few() +
    ggplot2::theme(axis.text = element_text(size = 6, family = "Arial")) +
    ggplot2::theme(axis.text.x = element_text(angle=90, face="italic")) +
    ggplot2::theme(axis.title = element_text(size = 8))
  grafico}
