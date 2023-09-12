#Carregando os pacotes
library(tidyverse)
library(rstatix)

#Carregando a base mtcars
base <- mtcars

#Transformações
base$am <- as.factor(base$am)
base <- base %>%
  mutate(am = ifelse(am == 0, "Automático", "Manual"))
base <- base %>% 
  rownames_to_column(var = "Marcas")

#Gráficos
base %>% 
  ggplot(aes(x = am, y = mpg)) +
  geom_boxplot() +
  labs(title = "Consumo em milhas por galão", 
        subtitle  = "Veículos automáticos e manuais") +
  theme_bw()

base %>% 
  group_by(Marcas, am) %>%
  summarise(mpg = mean(mpg)) %>% 
  ggplot(aes(x = reorder(Marcas, -mpg, decreasing = T), y = mpg)) +
  geom_col() +
  coord_flip() +
  facet_grid(~am) + 
  labs(title = "Média de consumo em milhas por galão") +
  xlab("") +
  ylab("") +
  theme_bw()

#Teste de normalidade Shapiro Francia
base %>% 
  group_by(am) %>% rstatix::shapiro_test(mpg)

#Teste de homogeneidade das variâncias
base %>% 
  rstatix::levene_test(mpg ~ am, center = mean)

#Teste T para duas amostras independentes
base %>% 
  rstatix::t_test(mpg ~ am, var.equal = FALSE) #se levene_test > 5% então TRUE


#esse link tem mais informações sobre o t-test
#https://fernandafperes.com.br/blog/teste-t-independente/
