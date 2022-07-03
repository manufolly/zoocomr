#' Title
#'
#' @param consultoria
#' @param method seleciona o método do pacote vegan::vegdist, podendo ser:"manhattan", "euclidean", "canberra", "clark",
# "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn",
# "mountford", "raup", "binomial", "chao", "cao", "mahalanobis", "chisq",
# "chord", "aitchison", or "robust.aitchison"
#' @param labels
#' @param ylab
#'
#' @return
#' @export
#'
#' @examples
cluster <- function(consultoria, method, labels, ylab) {
consultoria <- readxl::read_excel("C:/Curso R/pacotes/tabela.xlsx") %>%
    janitor::clean_names()
riqueza <- consultoria %>%
    dplyr::select(localidade, especie) %>%
    dplyr::count(localidade, especie) %>%
    tidyr::pivot_wider(
      names_from = especie,
      values_from = n) %>%
    janitor::clean_names()
riqueza <- base::replace(riqueza,is.na(riqueza),0)
 cluster_bray <- vegan::vegdist((riqueza[,2:68]), method="bray")
dendro <- stats::hclust(d = cluster_bray, method = "average")
graphics::plot(dendro, main = "Dendrograma",
               labels = c("Chiador", "Duas Barras", "Macaé", "Nova Friburgo",
                          "Sumidouro", "Trajano de Moraes"),
               ylab = "Similaridade (índice de Bray-Curtis)",
               xlab="", sub="")
}
