#' Gera um gráfico de abundância de espécies
#'
#' @param x nome do data frame que será utilizado no arquivo
#' @param filtro_campanha seleciona a campanha de consultoria, quando não selecionada,
#' inclui todas
#' @param campanha coluna que designa o indicador ordinal referente a campanha
#' @param especie coluna com a identificação de todos os indivíduos coletados
#'
#' @return um gráfico
#' @export
#'
#' @examples
#' \dontrun{
#'gabundancia(consultoria, filtro_campanha = "primeira")
#'}
gabundancia <- function(x, campanha = campanha, especie = especie,
                        fill = "#ADC6CB", filtro_campanha = NULL,
                        theme_few = ggthemes::theme_few(),
                        axis.text = ggplot2::element_text(size = 6, family = "Arial"),
                        axis.text.x = ggplot2::element_text(angle=90, face="italic"),
                        axis.title = ggplot2::element_text(size = 8)) {
  grafico <-x %>%
    dplyr::select({{campanha}}, {{especie}}) %>%
    dplyr::count({{campanha}}, {{especie}}) %>%
    dplyr::filter({{campanha}} == filtro_campanha) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = {{especie}}, y = n), fill = fill) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult = c(0, .025))) +
    ggplot2::labs(x = "Espécies",
         y = "Abundância") +
    ggthemes::theme_few() +
    ggplot2::theme(axis.text = axis.text) +
    ggplot2::theme(axis.text.x = axis.text.x) +
    ggplot2::theme(axis.title = axis.title)

  grafico}

