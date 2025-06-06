#Dependências
library(dplyr) #Manipulação dos dados
library(ggplot2) #Gráficos
library(psych) #Teste de Alfa de Cronbach

#Carregando dados
d <- read.csv("./dados.csv")

#Transformando períodos em uma variável ordinal
d$Degree.start <- factor(d$Degree.start)

#Verificando alfa de Cronbach
l <- c()
for (Q in paste0("Q", 1:7)) {
  l <- append(l, Q)
}
psych::alpha(d[, l])
rm(l,Q)

#Transformando questões em variáveis ordinais
l <- c("Discordo totalmente", "Discordo parcialmente", "Sou indiferente", "Concordo parcialmente", "Concordo totalmente")
for (Q in paste0("Q", 1:7)) {
  d[Q] <- factor(d[,Q], levels = 1:5, labels = l)
}
rm(l,Q)

#Gerando índice
d$ICDR <- as.numeric(lapply(d[ , paste0("Q", 1:7)], function(x) ((mean(as.numeric(x)) - 1) / 4)))

#Teste de normalidade e correções para o ICDR
correct <- function (x) {
  l <- list(
    og       = function(x) x, #Valor original
    log10    = function(x) log10(x), #Correção no log10
    sqrt     = function(x) sqrt(x), #Correção na raiz quadrada
    exp      = function(x) exp(x), #Correção no exponencial
    sqr      = function(x) x^2 #Correção na elevação ao quadrado
  )
  
  c <- data.frame(i = character(), y = numeric()) #Criação de data frame para o gráfico
  
  for (i in names(l)){
    y <- l[[i]](x) #Aplica correção
    p <- shapiro.test(y)$p.value #Executa o teste de Shapiro-Wilk
    r <- ifelse(p >= 0.05, "Há normalidade", "Não há normalidade") #Checa se o p-valor obtido indica normalidade
    cat(paste0("Correção: ", i, " | p-valor: ", round(p, 10), " | ", r, "\n\n")) #Retorna resultado organizado no console
    
    c <- rbind(c, data.frame(i, y)) #Salva resultado no data frame
  }
  
  ggplot(c, aes(y)) + #Gráfico para visualizar resultados
    geom_density() + #Curva de densidade
    stat_function(fun = dnorm, arg = list(mean = mean(y)), color = "red", linetype = "dashed") + #Curva normal perfeita, com média igual a dos dados utilizados
    facet_wrap(i ~ .) + #Faceta para mostrar os diferentes tipos de correção no mesmo gráfico
    theme_minimal() + #Tema minimalista
    labs(
      x = "ICDR",
      y = "density"
    )
}

correct(d$ICDR) #Aplicando correções

#--------------------------------------------------
#Testes

#Teste T para índice x gênero (exclui gêneros com poucos dados)
d[d$Gender %in% c("Mulher cisgênero", "Homem cisgênero"),] %>%
  t.test(ICDR ~ Gender, data = .)

#Teste T para índice x trabalho
t.test(ICDR ~ Work, data = d)

#Teste de Wilcoxon para índice x gênero (exclui gêneros com poucos dados)
d[d$Gender %in% c("Mulher cisgênero", "Homem cisgênero"),] %>%
  wilcox.test(ICDR ~ Gender, data = .)

#Spearman para índice x gênero (exclui gêneros com poucos dados)
ifelse(d$Gender == "Mulher cisgênero", 1, ifelse(d$Gender == "Homem cisgênero", 0, NA)) %>% #Trata os gêneros como dados quantitativos
  cor.test(d$ICDR, y = ., method = "spearman")

#Spearman para índice x período
cor.test(d$ICDR, as.numeric(d$Degree.start), method = "spearman")

#Anova para índice x curso
l <- d %>%
  group_by(Degree) %>% #Agrupando por curso
  summarise(
    n = n() #Contabilizando os cursos
  ) %>%
  filter(n >= 3) %>% #Filtrando cursos com 3 ou mais observações
  pull(Degree) #Extração

oneway.test(ICDR ~ Degree, d[d$Degree %in% l,]) #Anova
rm(l) #Apagando variáveis

#Spearman para índice x idade
cor.test(d$ICDR, d$Age, method = "spearman")

#--------------------------------------------------
#Gráficos

#Índice x período com divisão de gênero
ggplot(d[d$Gender %in% c("Mulher cisgênero", "Homem cisgênero"),], aes(Degree.start, ICDR)) +
  geom_boxplot(outliers = F) + #Boxplot
  geom_smooth(aes(group = Gender), se = F, color = "red", linetype = "dashed", alpha = .5) + #Linha de tendência
  facet_grid(Gender ~ .) + #Faceta de gênero
  labs(
    title = "Distribuição do ICDR pelos períodos, com divisão de gênero",
    x = "Período de ingresso"
  ) +
  theme_minimal() #Tema minimalista

#Média do Índice x Gênero
l <- d %>%
  filter(Gender %in% c("Mulher cisgênero", "Homem cisgênero")) %>% #Filtrando gêneros
  group_by(Degree) %>% #Agrupando por curso
  summarise(
    n_Gender = n_distinct(Gender),
    n = n() #Contabilizando gêneros
  ) %>%
  filter(n_Gender >= 2, n >= 4) %>% #Filtrando cursos que possuem um mínimo de observações pra cada gênero
  pull(Degree)

d[d$Degree %in% l & d$Gender %in% c("Mulher cisgênero", "Homem cisgênero"),] %>%
  group_by(Gender) %>% #Agrupamento por gẽnero
  summarise(ICDR = mean(ICDR)) %>% #Cálculo da média por gênero
  ggplot() +
  aes(reorder(Gender, ICDR), ICDR, fill = Gender) +
  geom_col() + #Colunas
  geom_hline(yintercept = mean(d$ICDR), linetype = "dashed", color = "red") + #Linha horizontal para média geral
  coord_cartesian(ylim = c(.87, .9)) + #Zoom no eixo y
  labs(
    title = "Média do ICDR por gênero",
    subtitle = "Ajustado no eixo y",
    x = "Gênero"
  ) +
  guides(fill = F) + #Apagando legendas inúteis
  theme_minimal() #Tema minimalista

#Índice x Curso com divisão de gênero
df %>%
  group_by(Degree, Gender) %>% #Agrupamento por curso e gênero
  summarise(ICDR = mean(ICDR)) %>% #Cálculo da média para cada gênero em cada curso
  ggplot() +
  aes(Gender, ICDR, color = Degree, group = Degree) +
  geom_line() + #Linha para comparação entre observações
  geom_point(size = 3) + #Ponto
  geom_hline(yintercept = mean(d$ICDR), linetype = "dashed", color = "red") + #Linha horizontal com média geral
  labs(
    title = "Distribuição do ICDR por cursos, com divisão de gênero",
    color = "Curso",
    x = "Gênero") +
  theme_minimal() #Tema minimalista

#Distribuição de respondentes por curso
l <- d %>% count(Degree, sort = TRUE) %>%
  top_n(5, n) #Filtando cursos com mais respondentes
d %>%
  filter(Degree %in% l$Degree) %>%
  count(Degree) %>%
  mutate(prop = n / sum(n), #Contabilizando % de respondentes do total
         label = paste0("(", round(prop * 100), "%)")) %>%
  ggplot(aes("", prop, fill = Degree)) +
  geom_bar(stat = "identity", width = 1) + #Barras
  coord_polar("y") + #Cordenadas polares
  geom_text(aes(label = label), position = position_stack(vjust = .5), size = 4, color = "white") + #Texto descritivo
  labs(fill = "Curso") +
  theme_void() #Tema vazio

rm(l) #Apagando variáveis
