#' Gera uma tabela com os indices de riqueza (Margalef, Menhinick)
#' e diversidade (Shannon, Simpson, Pielou)
#'
#' @param x nome do data frame que será utilizado no arquivo
#' @param localidade coluna com os locais de coleta
#' @param especie coluna com a identificacao de todos os individuos coletados
#' @param mutate_at mutate_at(c(2:{{69}}), as.numeric) deverá fornecer o número
#' de colunas total de espécies da sua tabela
#' @param diversidade função que produz a tabela com os indíces de diversidade e riqueza
#' @param Localidade descrição das localidades da tabela final
#'
#' @return uma tabela em excel
#' @export
#'
#' @examples
#' \dontrun{
#' riqueza(consultoria)
#' }
riqueza <- function(x, localidade = localidade, especie = especie,
                    mutate_at, diversidade,
                    Localidade = c("Chiador", "Duas Barras", "Macaé",
                                     "Nova Friburgo", "Sumidouro",
                                     "Trajano de Moraes"))
  {
riqueza <- x %>%
  dplyr::select({{localidade}}, {{especie}}) %>%
  dplyr::count({{localidade}}, {{especie}}) %>%
  tidyr::pivot_wider(
    names_from = {{especie}},
    values_from = n) %>%
  janitor::clean_names()
riqueza <- base::replace(riqueza,is.na(riqueza),0)
riqueza <- riqueza %>%
  dplyr::mutate_at(c(2:{{69}}), as.numeric)
riqueza_sp <- vegan::specnumber(riqueza)
abundancia <- base::apply((riqueza[,2:{{68}}]), 1, sum)
Margalef <- base::round((riqueza_sp - 1)/log(abundancia), 2)
Menhinick <- base::round(riqueza_sp/sqrt(abundancia), 2)
shannon_res <- vegan::diversity((riqueza[,2:68]), index = "shannon",
                                MARGIN = 1)
simpson_res <- vegan::diversity((riqueza[,2:68]), index = "simpson",
                                MARGIN = 1)
Pielou <- shannon_res/log(vegan::specnumber(riqueza))
diversidade <- base::data.frame(
  Localidade = Localidade,
  Margalef = Margalef,
  Menhinick = Menhinick,
  Shannon = shannon_res,
  Simpson = simpson_res,
  Pielou = Pielou)
diversidade <- writexl::write_xlsx(diversidade, "C:/Curso R/pacotes/zoocomr/results/diversidade.xlsx")

diversidade
}
