# Análise Assintótica de Algoritmos de Ordenação

Este projeto consiste na implementação e análise estatística de desempenho dos algoritmos **HeapSort**, **QuickSort** e **RadixSort**. O objetivo é comparar o tempo de execução empírico com a complexidade teórica de cada método.

## 📊 Algoritmos Analisados

1. **QuickSort (Lomuto)**: Complexidade média $O(n \log n)$.
2. **HeapSort (Heap Máximo)**: Complexidade $O(n \log n)$.
3. **RadixSort (LSD)**: Complexidade linear $O(n)$ em relação ao número de dígitos.

## 📈 Metodologia de Análise

Os dados foram coletados em ambiente **Ubuntu/WSL** utilizando a função `clock()` da linguagem C. 
- **Entradas**: Vetores de tamanho 10 até 1.000.000.
- **Execuções**: Média de 5 repetições para cada tamanho de entrada para garantir estabilidade estatística.

## 🛠️ Tecnologias Utilizadas

* **Linguagem C**: Implementação dos algoritmos e coleta de tempos.
* **R (RStudio)**: Processamento de dados e geração de gráficos.
* **Bibliotecas R**: `ggplot2`, `tidyr`, `scales`.

## 🖥️ Visualização dos Resultados

O script R incluído neste repositório gera:
- Gráficos de dispersão e linha para cada algoritmo individualmente.
- Gráficos em **Escala Logarítmica** para melhor visualização da tendência assintótica.
- Uma **Comparação Geral** sobrepondo os três algoritmos para identificar o ponto de maior eficiência de cada um.

---
*Relatório gerado como parte da atividade prática de Análise de Algoritmos.*