#' Gera um gráfico com duas colunas (uma de número de indivíduos e uma
#' de temperatura média) por localidade
#'
#' @param consultoria nome da tabela que será utilizada
#'
#' @return um gráfico de número de espécies e temperatura
#' @export
#'
#' @examples temperatura()
temperatura <- function(consultoria) {
  consultoria <- readxl::read_excel("C:/Curso R/pacotes/tabela.xlsx") %>%
    janitor::clean_names()
  temperatura <- consultoria %>%
  dplyr::group_by(localidade) %>%
  dplyr::summarise('n' = dplyr::n_distinct(especie),
                   'temp_media' = round(mean(temperatura_c, na.rm=T)))
temperatura <- base::replace(temperatura,is.na(temperatura),0) %>%
  dplyr::mutate(n = as.numeric(n),
                temp_media = as.numeric(temp_media))
fig <- plotly::plot_ly(temperatura, x = ~localidade, y = ~n,
                       type = 'bar', name = 'Número de indivíduos')
fig <-fig %>%  plotly::add_trace(y = ~temp_media, name = 'Temperatura média')
fig <- fig %>% plotly::layout(yaxis = list(title = 'Número de indivíduos'), barmode = 'group')
fig}
