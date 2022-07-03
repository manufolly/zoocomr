#' Gera um gráfico de abundância de espécies
#'
#' @param x nome do data frame que será utilizado no arquivo
#' @param filtro_campanha seleciona a campanha de consultoria, quando não selecionada,
#' inclui todas
#' @param campanha coluna que designa o indicador ordinal referente a campanha
#' @param especie coluna com a identificacao de todos os individuos coletados
#'
#' @return um gráfico
#' @export
#'
#' @examples gabundancia(consultoria, filtro_campanha = "primeira")
#'
gabundancia <- function(x, campanha = campanha, especie = especie,
                        fill = "#ADC6CB", filtro_campanha = NULL, theme_few, axis.text,
                        axis.text.x, axis.title) {
  grafico <-x %>%
    dplyr::select({{campanha}}, {{especie}}) %>%
    dplyr::count({{campanha}}, {{especie}}) %>%
    dplyr::filter({{campanha}} == filtro_campanha) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = {{especie}}, y = n), fill = "#ADC6CB") +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult = c(0, .025))) +
    ggplot2::labs(x = "Espécies",
         y = "Abundância") +
    ggthemes::theme_few() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 6, family = "Arial")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, face="italic")) +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 8))
  grafico}
