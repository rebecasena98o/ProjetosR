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


# ----------------------------------------------------------------


#Determinação das medidas de posição, dispersão e separação 

# IDADE DA VÍTIMA 

#Medidas de posição - onde os dados se concentram

#idade_numerica armazena todas as idades como numero e "não informado" vira NA
idade_numerica <- as.numeric(CVLI_2024_a_2025$`Idade da Vítima`)

#idade_valida armazena idades ignorando todos NA
idade_valida <- idade_numerica[!is.na(idade_numerica)]

#idade armazena media de idade das vitimas
idadeMedia <- mean(idade_valida)
idadeMedia

#Mediana
median(idadeMedia, na.rm = TRUE)

#Moda
moda <- function(x) {
  
  ux <- unique(x)  # valores únicos
  # ocorre com maior frequência no meu conjunto de dados
  
  ux[which.max(tabulate(match(x, ux)))]  # valor mais frequente
  
}

moda(idadeMedia)

# MEDIDAS DE DISPERSÃO
#IDADE VÍTIMA
#Amplitude

#amplitude_max <- as.numeric(max(CVLI_2024_a_2025$`Idade da Vítima`, na.rm = TRUE))
#amplitude_min <- as.numeric(min(CVLI_2024_a_2025$`Idade da Vítima`, na.rm = TRUE))
#amplitude_max_valida <- amplitude_max[!is.na(amplitude_max)]
#amplitude_min_valida <- amplitude_min[!is.na(amplitude_min)]
#amplitude_direta <- amplitude_max_valida - amplitude_min_valida
#amplitude_direta

#código limpo e de forma direta
idadesV <- as.numeric(CVLI_2024_a_2025$`Idade da Vítima`)
summary(idadesV) # verifica se o resultado está correto antes de rodar
amplitude_direta <- max(idadesV, na.rm = TRUE) - min(idadesV, na.rm = TRUE)
amplitude_direta

#Variância 

varianca <- as.numeric(CVLI_2024_a_2025$`Idade da Vítima`) #variância armazena 
#todas as idades como número e "não informado" vira NA

varianca_valida <- varianca[!is.na(varianca)] # armazena idades ignorando todos os NA

variancaOne <- var(varianca_valida) #armazena media de idade das vitimas
variancaOne

#Desvio Padrão

Desvio <- as.numeric(CVLI_2024_a_2025$`Idade da Vítima`) #Desvio armazena
#todas as idades como número e "não informado" vira NA

Desvio_valido <- Desvio[!is.na(Desvio)] # Armazena idades ignorando todos os NA

DesvioPadrao <- sd(Desvio_valido) #Armazena desvio padrão das idades das vítimas
DesvioPadrao # resultado

#coeficiente de variação
cv <- (DesvioPadrao/idadeMedia) * 100 
cv

#separatrizes - valores
#que dividem um conjunto de dados 
#(ordenado do menor para o maior) em partes iguais

# Quartis (Q): Dividem os dados em 4 partes (25% cada)
OneQ <- as.numeric(CVLI_2024_a_2025$`Idade da Vítima`)
OneQ_valido <- OneQ[!is.na(OneQ)]
Quartis <- quantile(OneQ_valido)
Quartis

#Bloxplot - é um gráfico usado para visualizar a distribuição e a 
#dispersão dos seus dados, destacando as separatrizes que você acabou de calcular
boxplot(idade_valida, 
        main = "Distribuição de Idade das Vítimas",
        ylab = "Idade",
        col = "lightblue",
        border = "darkblue")
