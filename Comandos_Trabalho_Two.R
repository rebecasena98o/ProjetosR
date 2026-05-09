# Gráfico de Dispersão 

# Gráfico simples
plot(x = dados$variavel_x, y = dados$variavel_y,
     main = "Título", xlab = "Eixo X", ylab = "Eixo Y",
     col = "blue", pch = 16)

# Gráfico com Pacote e personalização

library(ggplot2)
ggplot(dados, aes(x = variavel_x, y = variavel_y)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Gráfico de Dispersão", x = "Eixo X", y = "Eixo Y") +
  theme_minimal()



#--------------------------------------------------------------------------------


#Determinação das medidas de posição, dispersão e separação

#idade armazena media de idade das vitimas
idadeMedia <- mean(idade_valida)
idadeMedia

#Mediana
mediana_idade <- median(idade_valida, na.rm = TRUE)
mediana_idade

#--------------------------------------------------------------------------------

#Moda
moda <- function(x) {
  
  ux <- unique(x)  # valores únicos
  # ocorre com maior frequência no meu conjunto de dados
  
  ux[which.max(tabulate(match(x, ux)))]  # valor mais frequente
  
}

moda_idade <- moda(idade_valida)
moda_idade




#--------------------------------------------------------------------------------

# MEDIDAS DE DISPERSÃO
#IDADE VÍTIMA
#Amplitude

#amplitude_max <- as.numeric(max(CVLI_2024_a_2025_1_$`Idade da Vítima`, na.rm = TRUE))
#amplitude_min <- as.numeric(min(CVLI_2024_a_2025_1_$`Idade da Vítima`, na.rm = TRUE))
#amplitude_max_valida <- amplitude_max[!is.na(amplitude_max)]
#amplitude_min_valida <- amplitude_min[!is.na(amplitude_min)]
#amplitude_direta <- amplitude_max_valida - amplitude_min_valida
#amplitude_direta

#código limpo e de forma direta
idadesV <- as.numeric(CVLI_2024_a_2025_1_$`Idade da Vítima`)
summary(idadesV) # verifica se o resultado está correto antes de rodar
amplitude_direta <- max(idadesV, na.rm = TRUE) - min(idadesV, na.rm = TRUE)
amplitude_direta

#--------------------------------------------------------------------------------

#Variância 

varianca <- as.numeric(CVLI_2024_a_2025_1_$`Idade da Vítima`) #variância armazena 
#todas as idades como número e "não informado" vira NA

varianca_valida <- varianca[!is.na(varianca)] # armazena idades ignorando todos os NA

variancaOne <- var(varianca_valida) #armazena media de idade das vitimas
variancaOne

#--------------------------------------------------------------------------------

#Desvio Padrão

Desvio <- as.numeric(CVLI_2024_a_2025_1_$`Idade da Vítima`) #Desvio armazena
#todas as idades como número e "não informado" vira NA

Desvio_valido <- Desvio[!is.na(Desvio)] # Armazena idades ignorando todos os NA

DesvioPadrao <- sd(Desvio_valido) #Armazena desvio padrão das idades das vítimas
DesvioPadrao # resultado

#--------------------------------------------------------------------------------

#coeficiente de variação
cv <- (DesvioPadrao/idadeMedia) * 100 
cv

#--------------------------------------------------------------------------------

#separatrizes - valores
#que dividem um conjunto de dados 
#(ordenado do menor para o maior) em partes iguais

# Quartis (Q): Dividem os dados em 4 partes (25% cada)

OneQ <- as.numeric(CVLI_2024_a_2025_1_$`Idade da Vítima`)
OneQ_valido <- OneQ[!is.na(OneQ)]
Quartis <- quantile(OneQ_valido)
Quartis


#_______________________________________________________

x <- c(2.00,2.40,2.80,3.30,3.70,4.10,4.50,4.90,5.40,5.80,6.20,6.60,7.10,7.50,7.90,8.30,8.70,9.20,9.60,10.00)
y <- c(10.99,10.34,15.38,20.78,15.64,17.54,26.69,25.33,22.28,28.22,26.09,27.98,32.70,25.98,28.63,35.17,35.26,42.47,39.47,39.35)

r1 <- sum(x*y)-sum(x)*sum(y)/length(x)
r1

r2 <- sqrt((sum(x^2)-sum(x)^2/length(x))*(sum(y^2)-sum(y)^2/length(y)))
r2

RXY <- r1/r2
RXY

k <- RXY^2
k

plot(x,y)

regressão <- lm(y~x)
regressão

z <- plot(x,y)

grid(z)

abline(regressão)

mean(x)
mean(y)

moda <- function(v){
  u <- unique(v)
  u[which.max(tabulate(match(v,u)))]
}
moda(x)
moda(y)

range(x)
range(y)

max(x) - min(x)
max(y) - min(y)

var(x)
var(y)

sd(x)
sd(y)

sd(x)/mean(x)
sd(y)/mean(y)

quantile(x)
quantile(y)

min(x)
max(x)

min(y)
max(y)



#_______________________________________________________

library(ggplot2)
#x - Tamanho da Entrada (milhares)
#y - Tempo de Execução (ms)
dados <- data.frame(
  x = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  y = c(11,20,31,40,49,62,70,81,91,101,112,121,133,140,150,160,172,180,191,200)
)

x <- dados$x
y <- dados$y
n <- length(x)

#modelo
modelo <- lm(y ~ x, data = dados)
coef(modelo)
summary(modelo)

#Gera gráfico bonito usando ggplot
ggplot(dados, aes(x = x, y = y)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "",
       x = "Tamanho da Entrada (milhares) - X",
       y = "Tempo de Execução (ms) - Y") +
  theme_minimal()

#Gera grafico padrão
z <- plot(x, y)
abline(modelo)
grid()

#Calcula métrica R1
r1 <- sum(x*y) - (sum(x)*sum(y))/n
r1
#Calcula métrica R2
r2 <- sqrt((sum(x^2) - (sum(x)^2)/n) * (sum(y^2) - (sum(y)^2)/n))
r2

#coeficiente de correlação de Pearson entre x e y
RXY <- r1/r2
RXY
#coeficiente de determinação (R²), proporção da variação de y explicada por x
k <- RXY^2
k


#função para calculo da moda
moda <- function(v){
  u <- unique(v)
  u[which.max(tabulate(match(v,u)))]
}
#media, moda e mediana
mean(x); mean(y)
median(x); median(y)
moda(x); moda(y)

# Variância
var(x); var(y)

# Desvio Padrão
sd(x); sd(y)

# Coeficiente de Variação
sd(x)/mean(x)
sd(y)/mean(y)

#quartis

quantile(x)
quantile(y)

#amplitude com range
range(x); range(y)
diff(range(x)); diff(range(y))

#amplitude com min e max

min(x); max(x)
min(y); max(y)

#amplitude de x
max(x) - min(x)
#amplitude de y
max(y) - min(y)

#________________________________________________________________

library(ggplot2)
dados <- data.frame(
  x = c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32),
  y = c(510,495,480,470,460,450,440,430,420,410,405,395,385,375,370)
)
x <- dados$x
y <- dados$y
n <- length(x)


#Gera gráfico bonito usando ggplot
ggplot(dados, aes(x = x, y = y)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "",
       x = " temperatura de resfriamento (em °C) - X",
       y = "consumo médio de energia (em kWh) - Y") +
  theme_minimal()

#modelo
modelo <- lm(y ~ x, data = dados)
coef(modelo)
summary(modelo)

#Gera grafico padrão
z <- plot(x, y)
abline(modelo)
grid()

#Calcula métrica R1
r1 <- sum(x*y) - (sum(x)*sum(y))/n
r1
#Calcula métrica R2
r2 <- sqrt((sum(x^2) - (sum(x)^2)/n) * (sum(y^2) - (sum(y)^2)/n))
r2

#coeficiente de correlação de Pearson entre x e y
RXY <- r1/r2
RXY
#coeficiente de determinação (R²), proporção da variação de y explicada por x
k <- RXY^2
k


#função para calculo da moda
moda <- function(v){
  u <- unique(v)
  u[which.max(tabulate(match(v,u)))]
}
#media, moda e mediana
mean(x); mean(y)
median(x); median(y)
moda(x); moda(y)


var(x); var(y)
sd(x); sd(y)

sd(x)/mean(x)
sd(y)/mean(y)

#quartis

quantile(x)
quantile(y)

#amplitude com range
range(x); range(y)
diff(range(x)); diff(range(y))

#amplitude com min e max

min(x); max(x)
min(y); max(y)

#amplitude de x
max(x) - min(x)
#amplitude de y
max(y) - min(y)

#___________________________________________________________

library(writexl)

dados <- mtcars[, c("mpg","wt","hp","disp")]

modelo <- lm(mpg ~ wt + hp + disp, data = dados)
modelo

resumo <- summary(modelo)
resumo

anova_modelo <- anova(modelo)
anova_modelo

anova_tabela <- as.data.frame(anova_modelo)
colnames(anova_tabela) <- c("GL", "SQ", "MQ", "F", "p_valor")
anova_tabela$Fonte <- rownames(anova_tabela)
anova_tabela <- anova_tabela[, c("Fonte", "GL", "SQ", "MQ", "F", "p_valor")]
anova_tabela[, -1] <- round(anova_tabela[, -1], 3)
anova_tabela

coeficientes <- as.data.frame(resumo$coefficients)
coeficientes$Variavel <- rownames(coeficientes)
coeficientes <- coeficientes[, c("Variavel","Estimate","Std. Error","t value","Pr(>|t|)")]
colnames(coeficientes) <- c("Variavel","Coeficiente","Erro_Padrao","t","p_valor")
coeficientes <- round(coeficientes, 4)
coeficientes

R2 <- resumo$r.squared
R2_ajustado <- resumo$adj.r.squared

R2
R2_ajustado

previsao <- predict(modelo, newdata = data.frame(wt = 3.0, hp = 120, disp = 200))
previsao

resultado <- list(
  ANOVA = anova_tabela,
  Coeficientes = coeficientes,
  R2 = R2,
  R2_Ajustado = R2_ajustado,
  Previsao = previsao
)

write_xlsx(resultado, "analise_mtcars.xlsx")



