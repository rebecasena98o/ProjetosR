#Código utilizado para fins estatísticos de Análise Assintótica
# dos seguintes algoritmos:
# HeapSort, QuickSort e RadixSort

# Criando o dataframe com seus resultados do QuickSort
dados <- data.frame(
  tamanho = c(10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 
              10000, 25000, 50000, 75000, 100000, 250000, 500000, 750000, 1000000),
  tempo = c(0.000001, 0.000003, 0.000007, 0.000010, 0.000011, 0.000036, 
            0.000067, 0.000111, 0.000148, 0.000429, 0.001140, 0.001765, 
            0.002252, 0.003547, 0.007272, 0.010528, 0.014312, 0.038709, 
            0.083823, 0.130222, 0.180532)
)

library(ggplot2)

ggplot(dados, aes(x = tamanho, y = tempo)) +
  geom_point(color = "blue") +      # Pontos dos dados reais
  geom_line(color = "darkblue") +   # Linha conectando as médias
  labs(
    title = "Desempenho Empírico: Quick Sort (Lomuto)",
    subtitle = "Média de 5 execuções por tamanho de entrada",
    x = "Tamanho do Vetor (N)",
    y = "Tempo Médio (segundos)"
  ) +
  theme_minimal()

ggplot(dados, aes(x = tamanho, y = tempo)) +
  geom_point() +
  geom_line() +
  scale_x_log10() + # Escala logarítmica no eixo X
  scale_y_log10() + # Escala logarítmica no eixo Y
  labs(title = "Análise em Escala Log", x = "N (log)", y = "Tempo (log)")

# Criando o dataframe com seus resultados da HeapSort
dados_heap <- data.frame(
  tamanho = c(10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 
              10000, 25000, 50000, 75000, 100000, 250000, 500000, 750000, 1000000),
  tempo = c(0.000002, 0.000005, 0.000009, 0.000014, 0.000019, 0.000071, 
            0.000144, 0.000213, 0.000269, 0.000708, 0.001665, 0.001702, 
            0.002155, 0.004831, 0.010109, 0.015882, 0.022631, 0.061808, 
            0.130227, 0.212983, 0.290692)
)

# Gerar o gráfico técnico para o relatório
ggplot(dados_heap, aes(x = tamanho, y = tempo)) +
  geom_point(color = "red", size = 2) +           # Pontos representando as médias
  geom_line(color = "black", linewidth = 1) +    # Linha de tendência empírica
  labs(
    title = "Análise de Desempenho: Heap Sort (Heap Máximo)",
    subtitle = "Média de 5 execuções (Complexidade Teórica O(n log n))",
    x = "Tamanho da Entrada (N)",
    y = "Tempo Médio de Execução (segundos)",
    caption = "Fonte: Dados experimentais obtidos via clock() no Ubuntu/WSL"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)     

ggplot(dados_heap, aes(x = tamanho, y = tempo)) +
  geom_point() +
  geom_line() +
  scale_x_log10() + # Escala logarítmica no eixo X
  scale_y_log10() + # Escala logarítmica no eixo Y
  labs(title = "Análise em Escala Log", x = "N (log)", y = "Tempo (log)")

#Criando o dataframe com seus resultados da RadixSort
dados_radix <- data.frame(
  tamanho = c(10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 
              10000, 25000, 50000, 75000, 100000, 250000, 500000, 750000, 1000000),
  tempo = c(0.000003, 0.000005, 0.000008, 0.000011, 0.000016, 0.000033, 
            0.000102, 0.000125, 0.000163, 0.000344, 0.000624, 0.001080, 
            0.000907, 0.002576, 0.003848, 0.006133, 0.008136, 0.019870, 
            0.040222, 0.064168, 0.290692)
)

ggplot(dados_radix, aes(x = tamanho, y = tempo)) +
  geom_point(color = "purple", size = 2) +          
  geom_line(color = "black", linewidth = 0.8) +      
  labs(
    title = "Análise de Desempenho: Radix Sort (LSD)",
    subtitle = "Média de 5 execuções (Complexidade Teórica Linear O(n))",
    x = "Tamanho da Entrada (N)",
    y = "Tempo Médio de Execução (segundos)",
    caption = "Fonte: Dados experimentais obtidos via clock() no Ubuntu/WSL"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)

# Comparando os três algoritmos
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
library(ggplot2)
library(tidyr)

tamanhos <- c(10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 
              10000, 25000, 50000, 75000, 100000, 250000, 500000, 750000, 1000000)

dados_comparativos <- data.frame(
  Tamanho = tamanhos,
  HeapSort = c(0.000002, 0.000005, 0.000009, 0.000014, 0.000019, 0.000071, 
               0.000144, 0.000213, 0.000269, 0.000708, 0.001665, 0.001702, 
               0.002155, 0.004831, 0.010109, 0.015882, 0.022631, 0.061808, 
               0.130227, 0.212983, 0.290692),
  QuickSort = c(0.000001, 0.000003, 0.000007, 0.000010, 0.000011, 0.000036, 
                0.000067, 0.000111, 0.000148, 0.000429, 0.001140, 0.001765, 
                0.002252, 0.003547, 0.007272, 0.010528, 0.014312, 0.038709, 
                0.083823, 0.130222, 0.180532),
  RadixSort =  c(0.000003, 0.000005, 0.000008, 0.000011, 0.000016, 0.000033, 
                 0.000102, 0.000125, 0.000163, 0.000344, 0.000624, 0.001080, 
                 0.000907, 0.002576, 0.003848, 0.006133, 0.008136, 0.019870, 
                 0.040222, 0.064168, 0.083253)
  
)

# Transformar os dados para o formato longo (ideal para o ggplot)
dados_longos <- pivot_longer(dados_comparativos, 
                             cols = c("HeapSort", "QuickSort", "RadixSort"), 
                             names_to = "Algoritmo", 
                             values_to = "Tempo")

ggplot(dados_longos, aes(x = Tamanho, y = Tempo, color = Algoritmo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("HeapSort" = "#E41A1C", "QuickSort" = "#377EB8", "RadixSort" = "#4DAF4A")) +
  labs(
    title = "Comparação de Desempenho: Heap vs Quick vs Radix",
    subtitle = "Média de 5 execuções por tamanho de entrada",
    x = "Tamanho do Vetor (N)",
    y = "Tempo Médio (segundos)",
    color = "Algoritmos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)