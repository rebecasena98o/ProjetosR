data.frame(Base) # caso o nome seja diferente, troque o nome da variável

tab=table(Base$`5. Tipo de Residência`, Base$`6. Sexo`) #atribuição de valor - chamar variável - freq. simples

tab

# Freq. relativa 
tabW=prop.table(table(Base$`5. Tipo de Residência`))


tabW

# Freq. relativa - arredonda para 2 casas
round(prop.table(table(Base$`5. Tipo de Residência`)),2)


library(dplyr)


tabela_res <- Base %>%
  
  
  count(`5. Tipo de Residência`, name = "freq_simples") %>%
  
  mutate( #concatenar a freq. simples com a relativa
    
    freq_relativa = round(freq_simples / sum(freq_simples) * 100, 1)
    
  ) %>%
  
  arrange(desc(freq_simples)) %>% #arranjo
  
  select(`5. Tipo de Residência`, freq_simples, freq_relativa) # colocar cada um na sua coluna - nomes

tabela_res

# gráfico de barra - pega a base - nomes de variáveis - vetor e frequência - cores - título do gráfico
barplot(table(Base$`5. Tipo de Residência`), xlab="Tipo residencia", ylab="Quantidade", ylim=c(0,80), col=c("green", "yellow","red"), main="Tipo de residencia")


# gráfico de setor - tipo de residência 
pie(table(Base$`5. Tipo de Residência`), col=c("green", "yellow", "blue"), main = "Gráfico de setor Tipo de residência")



# Frequência absoluta da variável "Tipo de Residência"


freq <- table(Base_$`5. Tipo de Residência`)


# Calcular percentuais


pct <- round(freq / sum(freq) * 100, 1)


# Criar rótulos: nome da categoria + percentual


rotulos <- paste(names(freq), "-", pct, "%")




# Gráfico de setor


pie(freq,
    
    labels = rotulos,
    
    col = c("green", "yellow", "blue"),  # cores
    
    
    
    main = "Gráfico de Setor - Tipo de Residência")

# mediana
median(Base$`3. Tempo Mora na Localidade`)

# média
mediaOne = mean(Base$`3. Tempo Mora na Localidade`)
mediaOne

# sumário - temp min, quant - importante quartis (4 partes iguais - 25% dos dados - 1/4), 

summary(Base$`3. Tempo Mora na Localidade`)

#MODA:unique(x) → pega os valores únicos.


#match(x, ux) → encontra as posições de cada valor.


#tabulate() → conta quantas vezes cada valor apareceu.


#which.max() → retorna a posição do valor mais frequente.


# Criar função da moda


moda <- function(x) {
  
  
  ux <- unique(x)                     # valores únicos
  
# ocorre com maior frequência no meu conjunto de dados
  ux[which.max(tabulate(match(x, ux)))]  # valor mais frequente
  
}


# Usando na variável


moda(Base$`3. Tempo Mora na Localidade`) #maior frequência - medidas de dispersão


var(Base$`3. Tempo Mora na Localidade`) #variância

desvioPad= sd(Base$`3. Tempo Mora na Localidade`) #desvio padrão
desvioPad

cv= (desvioPad/mediaOne) * 100 #coeficiente de variação
cv

boxplot(Base$`3. Tempo Mora na Localidade`)


#Boxplot mostrando outliners:
  
  out <- bp$out


ind <- which(Base_$`3. Tempo Mora na Localidade` %out% out) #base de dados - pega - quem é

ind

# Criar o boxplot e guardar informações

bp <- boxplot(Base$`3. Tempo Mora na Localidade`,
              
              main = "Tempo que Mora na Localidade",
              
              ylab = "Anos",
              
              col = "lightblue")

# Pegar apenas os valores outliers


out <- bp$out

# Adicionar rótulos com os valores


text(x = rep(1, length(out)),
     
     y = out,
     
     labels = out,       # mostra só o valor do outlier
     
     pos = 4, cex = 0.8, col = "red")