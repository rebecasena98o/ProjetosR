#Determinando o coeficiente de correlação

propaganda <- c(30, 21, 35, 42, 37,  20, 8, 17, 35, 25)
vendas <- c(430, 335, 520, 490, 470, 210, 195, 270, 400, 480)

X <- sum(propaganda)
Y <- sum(vendas)

RxyOne <- sum(propaganda * vendas) - X * Y/10

SQx <- sum(propaganda^2) - (X^2 / 10)
SQy <- sum(vendas^2) - (Y^2 / 10)

RxyTwo <- sqrt(SQx * SQy)

# Coeficiente de Correlação (r)
r <- RxyOne / RxyTwo
r

cor(propaganda, vendas)

x <- c(30,21,35,42,37,20,8,17,35,25)

y <- c(430,335,520,490,470,210,195,270,400,480)


x <- c(30,21,35,42,37,20,8,17,35,25)


y <- c(430,335,520,490,470,210,195,270,400,480)


r1=sum(x*y)-sum(x)*sum(y)/10

r1


r2= sqrt((sum(x^2)-sum(x)^2/10)*(sum(y^2)-sum(y)^2/10))

r2

RXY=r1/r2

RXY


dados = data.frame(x,y) #criando um data.frame > is.data.frame(dados) #verifica se dados é um data.frame [1] TRUE >

cor(x,y)


plot(x,y)

regressão=lm(y~x) #ou apenas "regressão=lm(y~x)" > regressão Call: lm(formula = y ~ x, data = dados)


regressão

Coefficients: (Intercept) x #reta de regressão 15.65995 0.01591

# Yest = 117,07 + 9,738 x


z = plot(x,y)

grid(z) #aplicando grid ao gráfico >

abline(regressão)

dados <- data.frame( faturamento =
                       c(520,610,580,700,650,480,720,500,690,560,750,530,670,600,710),
                     volume = c(10,12,11,13,12,9,14,10,13,11,15,10,13,12,14),
                     preco = c(52,51,53,54,55,53,52,50,53,51,50,53,52,50,51),
                     desconto = c(5,4,6,3,2,7,3,6,2,5,2,6,3,4,2)
)
dados

#Coeficientes - B0, B1, B2, B3
# Modelo de regressão múltipla
modelo = lm(faturamento ~ volume + preco + desconto, data = dados)
modelo

#Análise de variância
anova_tabela <- as.data.frame(anova(modelo))
anova_tabela
library(writexl)
anova_tabela <- as.data.frame(anova(modelo))
colnames(anova_tabela) <- c("GL", "SQ", "MQ", "F", "p_valor")
anova_tabela$Fonte <- rownames(anova_tabela)
anova_tabela <- anova_tabela[, c("Fonte", "GL", "SQ", "MQ", "F", "p_valor")]
anova_tabela[, -1] <- round(anova_tabela[, -1], 3)
anova_tabela