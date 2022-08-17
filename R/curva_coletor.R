#' Curva de acúmulo de espécies
#'
#' @param x nome do data frame que será utilizado no arquivo
#' @param campanha coluna com as campanhas coletadas
#' @param especie coluna com a identificacao de todos os individuos coletados
#' @param at número de linhas (campanha) onde foram coletadas
#' @param labels especificar, em forma de vetor, as campanhas coletadas
#'
#' @return um gráfico
#' @export
#'
#' @examples
#' \dontrun{
#' coletor(consultoria, campanha, especie, at=c(1:4), labels=c("Primeira", "Segunda", "Terceira", "Quarta"))
#' }
coletor <- function(x, campanha, especie, at, labels)
  {
rare <- x %>%
    dplyr::select({{campanha}}, {{especie}}) %>%
    dplyr::count({{campanha}}, {{especie}}) %>%
    tidyr::pivot_wider(
      names_from = especie,
      values_from = n) %>%
    janitor::clean_names()
rarefacao <- base::replace(rare,is.na(rare),0) %>%
    dplyr::select(!{{campanha}})

sp1 <- vegan::specaccum(rarefacao)
sp2 <- vegan::specaccum(rarefacao, method = "collector")
grafico <- graphics::plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
                 main = "curva do coletor", xlab = "Campanhas", ylab = "Espécies", xaxt='n', ann=FALSE) %>%
    graphics::plot(axis(side=1, at=at, labels=labels))

grafico
}
