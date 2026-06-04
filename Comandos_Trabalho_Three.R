# Questão 1 - Distribuição Binomial

#GERAR A AMOSTRA E DEFINIÇÃO DOS PARÂMETROS
x_amostra <- 1000
ensaios <- 10
prob_sucessos <- 0.8

#Amostra Binomial 
amostra <- rbinom(n = x_amostra, size = ensaios, prob = prob_sucessos)
amostra

tabela_proporcoes <- table(factor(amostra, levels = 0:ensaios)) / x_amostra

#GRÁFICO 1: DISTRIBUIÇÃO DA AMOSTRA (Frequência Relativa)
barplot(tabela_proporcoes,
        main = "Distribuição Binomial da Amostra", 
        xlab = "Número de sucessos", 
        ylab = "Frequência Relativa (proporção)",
        col = "#008B8B",
        border = "black")

# GRÁFICO 2: DISTRIBUIÇÃO ACUMULADA DA AMOSTRA

acumulada_empirica <- ecdf(amostra)

plot(acumulada_empirica,
     main = "Distribuição Acumulada da Amostra",
     xlab = "Número de sucessos",
     ylab = "Probabilidade acumulada F(x)",
     col = "#341539",
     lwd = 2, 
     verticals = TRUE,
     do.points = TRUE)

# ---------------------------------------------------------------------------------------------------------------
# Questão 2

# parâmetros
y <- 1000
media_lambda <- 4

# gerando amostra poisson
amostra_pois <- rpois(n = y, lambda = media_lambda)
amostra_pois

# FREQUÊNCIA RELATIVA - DISTRIBUIÇÃO DA AMOSTRA
limite_superior <- max(amostra_pois)

tabela_proporcoes <- table(factor(amostra_pois, levels = 0:limite_superior)) / y

# gráfico distribuição Poisson
barplot(tabela_proporcoes,
        col = "lightgreen",
        main = "Distribuição Poisson",
        xlab = "Eventos",
        ylab = "Probabilidade")

# GRÁFICO - Distribuição acumulada
acumulada_empirica <- ecdf(amostra_pois)

plot(acumulada_empirica, 
     main = "Distribuição Acumulada da Amostra (Poisson)",
     xlab = "Número de Ocorrências (X)", 
     ylab = "Probabilidade Acumulada F(x)",
     col = "#4682B4",
     lwd = 2,
     verticals = TRUE, 
     do.points = TRUE)


# Questão 3

# DEFINIÇÃO DOS PARÂMETROS
n_amostra <- 1000  
media     <- 80    
desvio    <- 5

# GERAR A AMOSTRA NORMAL
amostra_normal <- rnorm(n = n_amostra, mean = media, sd = desvio)
amostra_normal


# HISTOGRAMA E CURVA DE DENSIDADE (Tudo no mesmo gráfico)
hist(amostra_normal, 
     prob = TRUE,
     main = "Histograma e Densidade da Amostra (Normal)",
     xlab = "Peso (kg)", 
     ylab = "Densidade",
     col = "lightgray", 
     border = "white",
     ylim = c(0, 0.1))

# Densidade empírica dos dados da amostra
densidade_amostra <- density(amostra_normal)

lines(densidade_amostra, 
      col = "darkred", 
      lwd = 3)

#Questão 4

# Parâmetros
n_relatorios <- 20
p_acerto <- 0.85
meta_acertos <- 20 * 0.80 

# Cálculo da probabilidade de P(X >= 16)
prob_satisfatorio <- pbinom(meta_acertos - 1, size = n_relatorios, prob = p_acerto, lower.tail = FALSE)

# Exibe o resultado
print(prob_satisfatorio)


# Questão 5

# P(X >= 2) = 1 - P(X <= 1)
prob_a <- ppois(q = 1, lambda = 2, lower.tail = FALSE)
print(prob_a)

#Questão 5 item B
# P(X >= 5) = 1 - P(X <= 4)
prob_b <- ppois(q = 4, lambda = 2, lower.tail = FALSE)
print(prob_b)

# Questão 6

# item a - P(X > 1900)
prob_a_six <- pnorm(q = 1900, mean = 2060, sd = 150, lower.tail = FALSE)
print(prob_a_six)

# item b - P(1800 < X < 1900)
prob_b_six <- pnorm(q = 1900, mean = 2060, sd = 150) - pnorm(q = 1800, mean = 2060, sd = 150)
print(prob_b_six)

# item c - conhecemos a probabilidade - quantil
# Encontrar o valor de X para P(X < x) = 0.025
horas_minimas <- qnorm(p = 0.025, mean = 2060, sd = 150)
print(horas_minimas)

# item d - Distribuição binomial - sucesso (Normal) - 4 tentativas
p_sucesso <- pnorm(q = 1800, mean = 2060, sd = 150, lower.tail = FALSE)

prob_d <- pbinom(q = 1, size = 4, prob = p_sucesso)
print(prob_d)

# Questão 7
# item a - P(X < 2)
prob_1 <- pnorm(q = 2, mean = 2.5, sd = 0.4)
print(prob_1)

# item b - P(2 < X < 3) - P(X <= 3) - P(X <= 2)
prob_2 <- pnorm(q = 3, mean = 2.5, sd = 0.4) - pnorm(q = 2, mean = 2.5, sd = 0.4)
print(prob_2)

# item c - (X > 3.2)
prob_lenta <- pnorm(q = 3.2, mean = 2.5, sd = 0.4, lower.tail = FALSE)
print(prob_lenta)

#item d - média (valor esperado) - prob sucesso
# Valor Esperado = n * p
total_consultas <- 500
media_lentas <- total_consultas * prob_lenta
print(media_lentas)

# item e - prob acumulada - temp corresp segundos - quantil - P(X <= x) = 0.90
tempo_90 <- qnorm(p = 0.90, mean = 2.5, sd = 0.4)
print(tempo_90)

# Questão 8 - Função para cálculo do tamanho da amostra (População Finita)
calcular_n_finito <- function(N, e, alpha, p_hat) {
  
  Z <- qnorm(1 - alpha / 2)
  
  numerador <- N * p_hat * (1 - p_hat) * (Z^2)
  denominador <- (N - 1) * (e^2) + p_hat * (1 - p_hat) * (Z^2)
  
  n_bruto <- numerador / denominador
  
  n_final <- ceiling(n_bruto)
  
  return(n_final)
}

# --- EXEMPLO DE USO (Teste da função) ---
# Imagine uma população de 10.000 pessoas, margem de erro de 5% (0.05),
# nível de significância de 5% (alpha = 0.05) e proporção estimada de 50% (0.5).
tamanho_amostra <- calcular_n_finito(N = 10000, e = 0.05, alpha = 0.05, p_hat = 0.5)
cat("O tamanho mínimo da amostra necessário é:", tamanho_amostra, "\n")

# Questão 9


n_amostra <- 1000      
renda_media <- 3500     
variancia <- 490000    
confianca <- 0.95      

# Cálculo do desvio-padrão a partir da variância
desvio_padrao <- sqrt(variancia) 
desvio_padrao


set.seed(123)
amostra_renda <- rnorm(n = n_amostra, mean = renda_media, sd = desvio_padrao)


#MEDIDAS ESTATÍSTICAS DA AMOSTRA
media_amostral <- mean(amostra_renda)
variancia_amostral <- var(amostra_renda)
desvio_amostral <- sd(amostra_renda)
mediana_amostral <- median(amostra_renda)

cat("--- Medidas Estatísticas da Amostra ---\n")
cat("Média Estimada: R$", round(media_amostral, 2), "\n")
cat("Mediana: R$", round(mediana_amostral, 2), "\n")
cat("Variância Estimada:", round(variancia_amostral, 2), "\n")
cat("Desvio-Padrão Estimado: R$", round(desvio_amostral, 2), "\n\n")


# INTERVALO DE CONFIANÇA 
# distribuição Normal (Z)
alpha <- 1 - confianca
Z <- qnorm(1 - alpha / 2)

# Cálculo do Erro Padrão da Média
erro_padrao <- desvio_padrao / sqrt(n_amostra)

# Limites do Intervalo
limite_inferior <- media_amostral - (Z * erro_padrao)
limite_superior <- media_amostral + (Z * erro_padrao)

cat("--- Intervalo de Confiança (95%) ---\n")
cat("IC: [ R$", round(limite_inferior, 2), "; R$", round(limite_superior, 2), "]\n")

# Questão 10
N <- 1000
p_hat <- 0.01
e <- 0.025
alpha <- 0.05

# Cálculo do Z
Z <- qnorm(1 - alpha / 2)

# Aplicação da fórmula
numerador <- N * p_hat * (1 - p_hat) * Z^2
denominador <- (N - 1) * e^2 + p_hat * (1 - p_hat) * Z^2
n_bruto <- numerador / denominador

# Arredondamento por excesso
n_final <- ceiling(n_bruto)

# Exibe a resposta
cat("Tamanho mínimo da amostra por lote:", n_final, "pastilhas.\n")


# ---------------------------------------------------------------------------------------------------------------

# ANOTAÇÕES DA AULA


# gráfico acumulado
plot(y, acumulada_empirica,
     type = "b",
     pch = 19,
     col = "blue",
     main = "Distribuição Acumulada Poisson",
     xlab = "Eventos",
     ylab = "Probabilidade acumulada")


# Probabilidade acumulada
Fx <- pbinom(x, size = 11, prob = 0.35)

# Exibir probabilidades acumuladas
Fx

# Gráfico da distribuição acumulada
plot(x, Fx,
     type = "h",
     main = "Distribuição Acumulada Binomial",
     xlab = "Número de sucessos",
     ylab = "Probabilidade Acumulada",
     col = "red",
     lwd = 3)

# Probabilidade de X = 7
dbinom(7, 11, 0.35)

# Probabilidade de X > 6
1 - pbinom(5, 11, 0.35)




#Comandos trabalho 3
#Questão da lista de exercícios mas serve para o trabalho - Questão 2
# Documento que tem Binomial e Poisson - Questão 10

x = 0:17 # valores que criamos

x

média = 2.3

poisson=dpois(x, 2.3) # recebe as probabilidades de cada um dos valores - ser menor ou igual - probabilidade - x
# p (x<= 2) - exemplo

poisson

plot(x,poisson, xlab= "N◦ de erros por milímetro",ylab="Probabilidade de Poisson",main="Distribuição de Poisson")
# gráfico dos nossos valores e probabilidades que acabamos de gerar; título do gráfico; e linha - line - sem 
# ele, só vai haver os pontos que representam nossos resultados


lines(x,poisson)

#p(x<=2)=0.4231901

ppois(2,3)

# p(x=0)=0.0001234098

dpois(0,9)

#P(x>=75)=1-p(x<=74)=0.9521754

1-ppois(74,90)



# 1. DEFINIÇÃO DOS PARÂMETROS
x_amostra <- 1000  # Tamanho da amostra gerada (n observações)
media_lambda <- 4   # Taxa média de ocorrência (lambda)

# 2. GERAR A AMOSTRA POISSON



# 3. GRÁFICO 1: DISTRIBUIÇÃO DA AMOSTRA (Frequência Relativa)
# Definimos o limite do gráfico até o valor máximo encontrado na amostra




barplot(tabela_proporcoes,
        main = "Distribuição Poisson da Amostra", 
        xlab = "Número de Ocorrências (X)", 
        ylab = "Frequência Relativa (proporção)",
        , # Azul aço
        border = "black")




# ---------------------------------------------------------------------------------------------------------------

# ANOTAÇÕES DA AULA

# Valores possíveis da variável
#x <- 0:25

# Probabilidade pontual
#fx <- dbinom(x, size = 11, prob = 0.35)

# Exibir probabilidades
#fx

# Gráfico da distribuição Binomial
#plot(x, fx,
#type = "h",
#main = "Distribuição Binomial",
#xlab = "Número de sucessos",
#ylab = "Probabilidade",
#col = "blue",
#lwd = 3)