riqueza <- function(consultoria) {
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
chao1 <- vegan::estaccumR(rare, permutations = 100)
summary(chao1, display = "chao")
resultados <- base::summary(chao1, display = c("S", "chao"))
res_chao <- base::cbind(resultados$chao[, 1:4], resultados$S[, 2:4])
res_chao <- base::as.data.frame(res_chao)
colnames(res_chao) <- c("Amostras", "Chao", "C_inferior", "C_superior",
                        "Riqueza", "R_inferior", "R_superior")
ggplot2::ggplot(res_chao, aes(y = Riqueza, x = Amostras)) +
  geom_point(aes(y = Chao, x = Amostras + 0.1), size = 4,
             color = "darkorange", alpha = 0.7) +
  geom_point(aes(y = Riqueza, x = Amostras), size = 4,
             color = "cyan4", alpha = 0.7) +
  geom_point(y = 7.5, x = 36, size = 4, color = "darkorange",
             alpha = 0.7) +
  geom_point(y = 8.0, x = 39, size = 4, color = "cyan4", alpha = 0.7) +
  geom_label(y = 7.5, x = 50, label = "Riqueza estimada - Chao 1") +
  geom_label(y = 8.0, x = 50, label = "Riqueza observada") +
  geom_line(aes(y = Chao, x = Amostras), color = "darkorange") +
  geom_line(aes(y = Riqueza, x = Amostras), color = "cyan4") +
  geom_linerange(aes(ymin = C_inferior, ymax = C_superior,
                     x = Amostras + 0.1), color = "darkorange") +
  geom_linerange(aes(ymin = R_inferior, ymax = R_superior,
                     x = Amostras), color = "cyan4") +
  scale_x_continuous(limits = c(1, 68), breaks = seq(10, 68, 10)) +
  labs (x = "Número de amostras", y = "Riqueza estimada - Chao 1") +
  ggthemes::theme_few()

#Jacknife 1 (baseados na incidência das espécies)
jack1 <- vegan::poolaccum(rarefacao, permutations = 100)
summary(jack1, display = "jack1")
resultados_jack1 <- base::summary(jack1, display = c("S", "jack1"))
res_jack1 <- base::cbind(resultados_jack1$jack1[, 1:4],
                         resultados_jack1$S[, 2:4])
res_jack1 <- base::as.data.frame(res_jack1)
colnames(res_jack1) <- c("Amostras", "JACK1", "JACK1_inferior",
                         "JACK1_superior", "Riqueza", "R_inferior",
                         "R_superior")
ggplot2::ggplot(res_jack1, aes(y = Riqueza, x = Amostras)) +
  geom_point(aes(y = JACK1, x = Amostras + 0.1), size = 4,
             color = "darkorange", alpha = 0.7) +
  geom_point(aes(y = Riqueza, x = Amostras), size = 4,
             color = "cyan4", alpha = 0.7) +
  geom_point(y = 9.9, x = 1, size = 4, color = "darkorange",   alpha = 0.7) +
  geom_point(y = 8.6, x = 1, size = 4, color = "cyan4", alpha = 0.7) +
  geom_label(y = 9.9, x = 4.2,
             label = "Riqueza estimada - Jackknife 1") +
  geom_label(y = 8.6, x = 3.2, label = "Riqueza observada") +
  geom_line(aes(y = JACK1, x = Amostras), color = "darkorange") +
  geom_line(aes(y = Riqueza, x = Amostras), color = "cyan4") +
  geom_linerange(aes(ymin = JACK1_inferior, ymax = JACK1_superior,
                     x = Amostras + 0.1), color = "darkorange") +
  geom_linerange(aes(ymin = R_inferior, ymax = R_superior,
                     x = Amostras), color = "cyan4") +
  scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, 1)) +
  labs(x = "Número de amostras",
       y = "Riqueza estimada - Jackknife 1") +
  ggthemes::theme_few()


#Jackniffe 2 (baseados na incidência das espécies)

jack2 <- vegan::poolaccum(rarefacao, permutations = 100)
summary(jack2, display = "jack2")

resultados_jack2 <- base::summary(jack2, display = c("S", "jack2"))
res_jack2 <- base::cbind(resultados_jack2$jack2[, 1:4],
                         resultados_jack2$S[, 2:4])
res_jack2 <- base::as.data.frame(res_jack2)
colnames(res_jack2) <- c("Amostras", "JACK2", "JACK2_inferior",
                         "JACK2_superior", "Riqueza", "R_inferior","R_superior")

ggplot2::ggplot(res_jack2, aes(y = Riqueza, x = Amostras)) +
  geom_point(aes(y = JACK2, x = Amostras + 0.1), size = 4,
             color = "darkorange", alpha = 0.7) +
  geom_point(aes(y = Riqueza, x = Amostras), size = 4,
             color = "cyan4", alpha = 0.7) +
  geom_point(y = 16, x = 9, size = 4, color = "darkorange",
             alpha = 0.7) +
  geom_point(y = 14, x = 9, size = 4, color = "cyan4", alpha = 0.7) +
  geom_label(y = 16, x = 12.5,
             label = "Riqueza estimada - Jackknife 2") +
  geom_label(y = 14, x = 11.5, label = "Riqueza observada") +
  geom_line(aes(y = JACK2, x = Amostras), color = "darkorange") +
  geom_line(aes(y = Riqueza, x = Amostras), color = "cyan4") +
  geom_linerange(aes(ymin = JACK2_inferior, ymax = JACK2_superior,
                     x = Amostras + 0.1), color = "darkorange") +
  geom_linerange(aes(ymin = R_inferior, ymax = R_superior,
                     x = Amostras), color = "cyan4") +
  scale_x_continuous(limits = c(1, 15), breaks = seq(1, 15, 1)) +
  labs(x = "Número de amostras",
       y = "Riqueza estimada - Jackknife 2") +
  ggthemes::theme_few()
}
