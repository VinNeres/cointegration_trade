# Análise e Backtesting de Estratégias de Pairs Trading

Este repositório contém um projeto em R para a identificação, validação estatística e backtesting de estratégias de *Pairs Trading* no mercado de ações. O objetivo é fornecer um framework claro e educacional para a implementação de uma das estratégias de arbitragem estatística mais clássicas.

## O que é Pairs Trading?

*Pairs Trading* é uma estratégia de investimento **neutra ao mercado** que busca lucrar com desequilíbrios temporários na relação de preços de dois ativos que são historicamente correlacionados.

A lógica é simples:
1.  **Identificar um Par:** Encontrar dois ativos cujos preços "dançam juntos" ao longo do tempo.
2.  **Monitorar o Desvio:** Quando os preços se afastam anormalmente um do outro, um ativo fica "caro" em relação ao seu par.
3.  **Apostar na Convergência:** O trader abre uma posição *short* (vendida) no ativo caro e uma posição *long* (comprada) no ativo barato.
4.  **Realizar o Lucro:** Quando a relação de preços retorna à sua média histórica, as posições são fechadas, gerando lucro independentemente da direção geral do mercado.


*Analogia do Bêbado e o Cachorro: os caminhos individuais são aleatórios (não-estacionários), mas a distância entre eles (a coleira, ou o "spread") é estável e reverte à média (estacionária).*

---

## A Base Estatística: Cointegração

Uma simples correlação não é suficiente para garantir que dois ativos formem um bom par. Preços de ações são tipicamente **não-estacionários** (seguem um "passeio aleatório"). O alicerce estatístico do Pairs Trading é o conceito de **cointegração**.

Duas séries não-estacionárias, $Y_t$ e $X_t$, são ditas cointegradas se existe uma combinação linear entre elas que é **estacionária**.

$$
Z_t = Y_t - \beta X_t
$$

Onde $Z_t$ é o **spread** (ou resíduo) e $\beta$ é o **hedge ratio**. Se $Z_t$ for estacionário, significa que existe uma relação de equilíbrio de longo prazo entre $Y_t$ e $X_t$, e qualquer desvio dessa relação tende a ser corrigido.

### Testes de Cointegração Utilizados

Para validar um par, este projeto utiliza dois testes principais:

1.  **Teste de Engle-Granger (Dois Passos):**
    -   **Passo 1:** Estima-se a relação de longo prazo através de uma regressão linear simples:

        $$
        Y_t = \alpha + \beta X_t + \epsilon_t
        $$

    -   **Passo 2:** Os resíduos da regressão, $\hat{\epsilon}_t$, são extraídos e testados para estacionariedade usando o **Teste Augmented Dickey-Fuller (ADF)**. Se os resíduos forem estacionários (p-valor < 0.05), os ativos são considerados cointegrados.

2.  **Teste de Johansen:**
    -   Uma abordagem mais robusta que modela os ativos como um sistema (Modelo Vetorial de Auto-Regressão - VAR) e determina o número de relações de cointegração existentes. É especialmente útil para análises com mais de dois ativos.

---

## Workflow da Estratégia

O fluxo de trabalho implementado neste código segue os seguintes passos:

1.  **Seleção de Pares:** Coleta de dados históricos de preços para ativos candidatos.
2.  **Validação Estatística:**
    -   Cálculo do hedge ratio ($\beta$) via regressão linear.
    -   Cálculo do spread (resíduos da regressão).
    -   Aplicação dos testes de cointegração (Engle-Granger e Johansen) para validar o par.
3.  **Geração de Sinais:**
    -   O spread é normalizado usando o **Z-score** para criar um indicador padronizado de desvio:

        $$
        \text{Z-score}_t = \frac{\text{Spread}_t - \text{MédiaMóvel}(\text{Spread})}{\text{DesvioPadrãoMóvel}(\text{Spread})}
        $$
        
    -   Sinais de entrada e saída são gerados com base em limiares (thresholds) de Z-score (ex: entrar em ±2.0, sair em 0).
4.  **Backtesting:**
    -   Simulação histórica da estratégia, aplicando os sinais de negociação aos dados de preços.
    -   Cálculo dos retornos diários e construção da curva de capital.
5.  **Análise de Performance:**
    -   Cálculo de métricas de desempenho chave:
        -   Retorno Anualizado
        -   Volatilidade Anualizada
        -   Índice de Sharpe
        -   Máximo Drawdown
        -   Taxa de Acerto (Win Rate)