# gráfico de barra - pega a base - nomes de variáveis - vetor e frequência - cores - título do gráfico
#barplot(table(CVLI_2024_a_2025_1_$`5. Tipo de Residência`), xlab="Tipo residencia", ylab="Quantidade", ylim=c(0,80), col=c("green", "yellow","red"), main="Tipo de residencia")

#--------------------------------------------------------------------------------

# Frequência absoluta 

#Semana
freq_Semana <- table(CVLI_2024_a_2025_1_$`Dia da Semana`)
freq_Semana

#Escolaridade da Vítima
freq_Escolaridade <- table(CVLI_2024_a_2025_1_$`Escolaridade da Vítima`)
freq_Escolaridade

#Natureza do Crime
freq_Natureza <- table(CVLI_2024_a_2025_1_$Natureza)
freq_Natureza

#Raça das vítimas
freq_Raca <- table(CVLI_2024_a_2025_1_$`Raça da Vítima`)
freq_Raca

#--------------------------------------------------------------------------------

# gráfico de setor - tipo de residência 

pie(table(CVLI_2024_a_2025_1_$Gênero), col=c("pink", "blue"), main = "Gráfico de Gênero")

pie(table(CVLI_2024_a_2025_1_$`Meio Empregado`), col=c("orange", "purple"), main = "Gráfico de Meio Empregado")


#--------------------------------------------------------------------------------

#Gráfico de barras
#jeito simples e sem cor
barplot(table(CVLI_2024_a_2025_1_$`Dia da Semana`))
barplot(table(CVLI_2024_a_2025_1_$`Escolaridade da Vítima`))

#Gráfico de barras
#jeito complexo e com cores
freq_SEMANA_table <- table(CVLI_2024_a_2025_1_$`Dia da Semana`)

barplot(freq_SEMANA_table,
        xlab="Dia da Semana",
        ylab="Quantidade",
        main="Ocorrências por Dia da Semana",
        col=c("red","blue","green","yellow","purple","orange","pink"))



freq_ESCOLARIDADE_table <- table(CVLI_2024_a_2025_1_$`Escolaridade da Vítima`)

barplot(freq_ESCOLARIDADE_table,
        xlab="Escolaridade da Vítima",
        ylab="Quantidade",
        main="Ocorrências por Escolaridade",
        col=c("#FFB3BA","#BAFFC9","#BAE1FF","#FFFFBA","#E6CCFF","#FFD9B3","#C2F0FC"))



#--------------------------------------------------------------------------------

#Freq. relativa

#Semana
tabW=prop.table(table(CVLI_2024_a_2025_1_$`Dia da Semana`))
tabW

# Para visualizar em porcentagem (0% a 100%) e arredondar
#Semana
freq_Relativa_Semana <- round(tabW * 100, 2)
print(freq_Relativa_Semana)

#Escolaridade da Vítima
tabWE=prop.table(table(CVLI_2024_a_2025_1_$`Escolaridade da Vítima`))
tabW

# Para visualizar em porcentagem (0% a 100%) e arredondar
freq_Relativa_Escolaridade <- round(tabWE * 100, 2)
print(freq_relativa_escolaridade)

#Raça das Vítimas
tabWR = prop.table(table(CVLI_2024_a_2025_1_$`Raça da Vítima`))
tabWR
  
# Para visualizar em porcentagem (0% a 100%) e arredondar
freq_relativa_Raca <- round(tabWR * 100, 2)
print(freq_relativa_Raca)

#--------------------------------------------------------------------------------


#Determinação das medidas de posição, dispersão e separação 

# IDADE DA VÍTIMA 

#Medidas de posição - onde os dados se concentram

#idade_numerica armazena todas as idades como numero e "não informado" vira NA
#Idade da Vítima
idade_numerica <- as.numeric(CVLI_2024_a_2025_1_$`Idade da Vítima`)

#idade_valida armazena idades ignorando todos NA
idade_valida <- idade_numerica[!is.na(idade_numerica)]

#idade armazena media de idade das vitimas
idadeMedia <- mean(idade_valida)
idadeMedia

#--------------------------------------------------------------------------------

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

idade_numerica_bp <- as.numeric(CVLI_2024_a_2025_1_$`Idade da Vítima`)
#Bloxplot - é um gráfico usado para visualizar a distribuição e a 
#dispersão dos seus dados, destacando as separatrizes que você acabou de calcular
bp <- boxplot(idade_numerica_bp, 
        main = "Distribuição de Idade das Vítimas",
        ylab = "Idade (Anos)",
        col = "lightblue",
        border = "darkblue",
        outline = TRUE)

#Boxplot mostrando outliners:

outliers_valores <- bp$out

print("Valores considerados outliers:")
print(outliers_valores)


freq_ESCOLARIDADE_table <- table(CVLI_2024_a_2025_1_$`Escolaridade da Vítima`)

par(mar=c(11,4,4,2))

barplot(freq_ESCOLARIDADE_table,
        #xlab="Escolaridade da Vítima",
        ylab="Quantidade",
        main="Ocorrências por Escolaridade",
        col=c("#FFB3BA","#BAFFC9","#BAE1FF","#FFFFBA","#E6CCFF","#FFD9B3","#C2F0FC"),
        las=2,
        cex.names=0.8)
