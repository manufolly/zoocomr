#' Gera um gráfico de similaridade com o índice de Bray-Curtis
#'
#' @param x nome do data frame que será utilizado no arquivo
#' @param localidade coluna que designa os locais onde os exemplares foram coletados
#' @param especie coluna com a identificação de todos os indivíduos coletados
#' @param method seleciona o método do pacote vegan::vegdist, podendo ser:"manhattan", "euclidean", "canberra", "clark",
# "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn",
# "mountford", "raup", "binomial", "chao", "cao", "mahalanobis", "chisq",
# "chord", "aitchison", or "robust.aitchison"
#' @param method2 método do hclust
#' @param labels descreve as legendas das localidades que serão as identificações dos terminais dos gráficos
#' @param ylab legenda do eixo y
#'
#' @return um gráfico
#' @export
#'
#' @examples similaridade(consultoria, localidade = localidade, especie = especie, method = "bray", method2 = "average", labels = c("Chiador", "Duas Barras", "Macaé", "Nova Friburgo",
#'          "Sumidouro", "Trajano de Moraes"), ylab = "Similaridade (índice de Bray-Curtis)", xlab = "localidades", sub = "similaridade")

similaridade <- function(x, localidade = localidade, especie = especie,
                    method = "bray", method2 = "average",
                    labels = c("Chiador", "Duas Barras", "Macaé", "Nova Friburgo",
                       "Sumidouro", "Trajano de Moraes"),
                    ylab = "Similaridade (índice de Bray-Curtis)", xlab, sub)
  {
riqueza <- x %>%
    dplyr::select({{localidade}}, {{especie}}) %>%
    dplyr::count({{localidade}}, {{especie}}) %>%
    tidyr::pivot_wider(
      names_from = especie,
      values_from = n) %>%
    janitor::clean_names()
riqueza <- base::replace(riqueza,is.na(riqueza),0)
cluster_bray <- vegan::vegdist((riqueza[,2:68]), method = method)
dendro <- stats::hclust(d = cluster_bray, method = method2)
grafico <- graphics::plot(dendro, main = "Dendrograma",
               labels = labels,
               ylab = ylab,
               xlab=xlab, sub=sub)
grafico
}
