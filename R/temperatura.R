#' Gera um gráfico com duas colunas (uma de número de indivíduos e uma
#' de temperatura média) por localidade
#'
#' @param x nome do data frame que será utilizado no arquivo
#' @param localidade coluna que designa os locais onde os exemplares
#' foram coletados e serão agrupados por essa coluna
#' @param especie coluna com a identificação de todos os indivíduos coletados
#' @param temperatura_c coluna com a temperatura dos indivíduos coletados
#'
#' @return um gráfico de número de espécies e temperatura
#' @export
#'
#' @examples temperatura(consultoria)
temperatura <- function(x, localidade, especie, temperatura_c)
  {
temperatura <- x %>%
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

fig
}
