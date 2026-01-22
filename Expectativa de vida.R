library(tidyverse)
library(dados)
library(ggthemes)

# view(dados_gapminder)
# Esse dataset conta com a expectativa de vida de diversos países em
# diferentes anos, mas nosso intuito é apenas verificar em algum ano qualquer como os
# fatores expectativa de vida e tamanho da população se relacionam (ou não).

#Filtragem do dataset original
dataset_filtrado <- dados_gapminder |>
  filter(ano == max(ano)) |>
  select(pais, expectativa_de_vida, populacao, continente)

# Criação de um gráfico de dispersão 
grafico1 <- ggplot( data = dataset_filtrado,
        mapping = aes(x = populacao, 
                      y = expectativa_de_vida,
                      color = continente)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) + 
  geom_smooth(method = 'loess', color = 'black', se = FALSE) +
  scale_color_colorblind() +
  labs(
    title = paste("População e Expectativa de Vida (2007)"),
    x = "População",
    y = "Expectativa de vida (anos)",
    color = "Continente"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),      
    axis.text = element_text(size = 10),       
    plot.title = element_text(size = 14, face = "bold"),  
    legend.title = element_text(size = 11),    
    legend.text = element_text(size = 10)      
  )

#Nesse gráfico, cada ponto representa um país diferente. 
#A posição horizontal do ponto mostra o tamanho da população 
#(países menores à esquerda, maiores à direita), enquanto a 
#posição vertical mostra a expectativa de vida (países com vida 
#mais longa em cima, mais curta embaixo).
#A linha preta suave confirma essa falta de relação: ela é quase 
#reta e horizontal, indicando que a expectativa de vida não aumenta 
#nem diminui conforme a população cresce. 

ggsave("grafico_populacao_expectativa.png", 
       plot = grafico1,
       width = 10,    
       height = 6,    
       dpi = 300)     

# Criação de um gráfico de distribuição de probabilidade
grafico2 <- ggplot(dataset_filtrado, aes(x = expectativa_de_vida, fill = continente)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuição da Expectativa de Vida por Continente",
       x = "Expectativa de vida (anos)",
       y = "Densidade"
       ) +
  theme(
    axis.title = element_text(size = 12),      
    axis.text = element_text(size = 10),       
    plot.title = element_text(size = 14, face = "bold"),  
    legend.title = element_text(size = 11),    
    legend.text = element_text(size = 10)     
  )

ggsave("grafico_densidade.png", 
       plot = grafico2,
       width = 10,    
       height = 6,    
       dpi = 300)     


# Nesse gráfico, cada continente possui sua própria densidade, em que a base
# da curva representa como a expectativa de vida se distribui, os picos representam 
# os valores mais comuns de idade e os vales o oposto, ou seja, os valores menos comuns
# para a expecativa de vida daquele continente.



# Não podemos afirmar que o tamanho da população interfere no cálculo 
# da expectativa de vida, uma vez que os pontos estão bem espalhados verticalmente
# por todo o gráfico. Portanto, a expectativa está atrelada a outros fatores (sociais, 
# economicos, ou etc), tendo em vista que países de mesmo continente possuem menor variação
# entre si de suas expectativas de vida


