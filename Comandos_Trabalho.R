# gráfico de barra - pega a base - nomes de variáveis - vetor e frequência - cores - título do gráfico
barplot(table(CVLI_2024_a_2025$`5. Tipo de Residência`), xlab="Tipo residencia", ylab="Quantidade", ylim=c(0,80), col=c("green", "yellow","red"), main="Tipo de residencia")

# Frequência absoluta da variável "Tipo de Residência"

freq <- table(CVLI_2024_a_2025$`Dia da Semana`)
freq

# gráfico de setor - tipo de residência 

pie(table(CVLI_2024_a_2025$Gênero), col=c("pink", "blue"), main = "Gráfico de Gênero")

pie(table(CVLI_2024_a_2025$`Meio Empregado`), col=c("orange", "purple"), main = "Gráfico de Meio Empregado")


#Gráfico de barras
barplot(table(CVLI_2024_a_2025$`Dia da Semana`))
barplot(table(CVLI_2024_a_2025$`Escolaridade da Vítima`))


freq <- table(CVLI_2024_a_2025$`Dia da Semana`)

barplot(freq,
        xlab="Dia da Semana",
        ylab="Quantidade",
        main="Ocorrências por Dia da Semana",
        col=c("red","blue","green","yellow","purple","orange","pink"))

freq <- table(CVLI_2024_a_2025$`Escolaridade da Vítima`)

barplot(freq,
        xlab="Escolaridade da Vítima",
        ylab="Quantidade",
        main="Ocorrências por Dia da Semana",
        col=c("red","lightblue","lightgreen","lightyellow","purple","orange","pink"))

freq <- table(CVLI_2024_a_2025$`Escolaridade da Vítima`)

barplot(freq,
        xlab="Escolaridade da Vítima",
        ylab="Quantidade",
        main="Ocorrências por Escolaridade",
        col=c("#FFB3BA","#BAFFC9","#BAE1FF","#FFFFBA","#E6CCFF","#FFD9B3","#C2F0FC"))



#Freq. relativa
tabW=prop.table(table(CVLI_2024_a_2025$`Dia da Semana`))
tabW

#Freq. relativa - arredonda para 2 casas
round(prop.table(table(CVLI_2024_a_2025$`Dia da Semana`)) * 100, 2)

#Determinação das medidas de posição, dispersão e separação 

idade <- CVLI_2024_a_2025$`Idade da Vítima`

#Medidas de posição - onde os dados se concentram

#Média
mean(idade, na.rm = TRUE)

#Mediana
median(idade, na.rm = TRUE)

#Moda
moda <- function(x) {
  
  ux <- unique(x)  # valores únicos
  # ocorre com maior frequência no meu conjunto de dados
  
  ux[which.max(tabulate(match(x, ux)))]  # valor mais frequente
  
}
moda(idade)
