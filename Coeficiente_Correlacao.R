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