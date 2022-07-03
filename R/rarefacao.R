cluster <- function(consultoria, datatype, endpoint) {
consultoria <- readxl::read_excel("C:/Curso R/pacotes/tabela.xlsx") %>%
    janitor::clean_names()
rare <- consultoria %>%
    dplyr::select(localidade, especie) %>%
    dplyr::count(localidade, especie) %>%
    tidyr::pivot_wider(
      names_from = localidade,
      values_from = n) %>%
    janitor::clean_names()
rarefacao <- base::replace(rare,is.na(rare),0) %>%
    dplyr::mutate(chiador = as.numeric(chiador),
                  duas_barras = as.numeric(duas_barras),
                  macae = as.numeric(macae),
                  nova_friburgo = as.numeric(nova_friburgo),
                  sumidouro = as.numeric(sumidouro),
                  trajano_de_moraes = as.numeric(trajano_de_moraes))
#head(rarefacao)
resultado <- rarefacao %>%
iNEXT::iNEXT(q=0, datatype = "abundance",
               endpoint = NULL)
iNEXT::ggiNEXT(resultado, type = 1) +
  geom_vline(xintercept = 166, lty = 2) +
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
  scale_colour_manual(values = c("darkorange", "darkorchid",
                                 "cyan4")) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  labs(x = "Número de indivíduos", y = " Riqueza de espécies") +
  ggthemes::theme_few()
}
