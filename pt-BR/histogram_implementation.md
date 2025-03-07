# Implementação de Histogramas em COBOL

Este documento descreve como implementar histogramas para métricas em aplicações COBOL, seguindo as convenções do OpenTelemetry.

## Conceito

Em observabilidade, um histograma é uma métrica que amostra observações (geralmente coisas como durações de requisições ou tamanhos de resposta) e as conta em buckets configuráveis. Histogramas são usados para:
- Medir a distribuição de valores (como tempos de resposta)
- Calcular percentis (p50, p90, p99)
- Entender o comportamento estatístico de uma métrica ao longo do tempo

## Estrutura Básica em COBOL

Em COBOL, os histogramas podem ser implementados como uma série de contadores para diferentes faixas de valores (buckets), além de variáveis para controlar a soma total e a contagem de observações.

```cobol
       WORKING-STORAGE SECTION.
       * Histograma para tempo de resposta (ms)
       01 WS-HISTOGRAMA-TEMPO.
          05 WS-BUCKET-0-10              PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-11-50             PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-51-100            PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500           PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-PLUS          PIC 9(9) COMP VALUE ZEROS.
          05 WS-HIST-SOMA                PIC 9(9) COMP VALUE ZEROS.
          05 WS-HIST-CONTAGEM            PIC 9(9) COMP VALUE ZEROS.
```

## Histogramas para Métricas de Negócio Genéricas

Para implementar histogramas que medem aspectos de negócio independentes do setor:

```cobol
       WORKING-STORAGE SECTION.
       * Histograma para duração de transações de negócio
       01 WS-BUSINESS-DURATION-HIST.
          05 WS-BUCKET-0-100-MS         PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500-MS       PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-1000-MS      PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-1001-5000-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-5001-PLUS-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-SUM            PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-COUNT          PIC 9(9) COMP VALUE ZEROS.
          
       * Histograma para tamanho de entidades de negócio
       01 WS-BUSINESS-SIZE-HIST.
          05 WS-SIZE-0-1KB              PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-1-10KB             PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-10-100KB           PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-100KB-1MB          PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-1MB-PLUS           PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-SUM               PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-COUNT             PIC 9(9) COMP VALUE ZEROS.
          
       * Estrutura para métricas com atributos de contexto
       01 WS-BUSINESS-HIST-METRIC.
          05 WS-HIST-ID                 PIC 9(3).
          05 WS-BUCKET-ID               PIC 9(2).
          05 WS-BUCKET-VALOR            PIC 9(9).
          05 WS-HIST-CONTEXT.
             10 WS-BUSINESS-DOMAIN      PIC X(15).
             10 WS-TRANSACTION-TYPE     PIC X(15).
             10 WS-ENTITY-TYPE          PIC X(15).
             10 WS-PROCESS-NAME         PIC X(20).
             10 WS-PRIORITY-LEVEL       PIC X(6).
```

## Implementação Completa para Métricas de Negócio

Aqui está um exemplo completo de como implementar histogramas para métricas de negócio:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSNHIST.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Histograma para duração de transações de negócio
       01 WS-BUSINESS-DURATION-HIST.
          05 WS-BUCKET-0-100-MS         PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500-MS       PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-1000-MS      PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-1001-5000-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-5001-PLUS-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-SUM            PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-COUNT          PIC 9(9) COMP VALUE ZEROS.
       
       * Variáveis para medição de tempo
       01 WS-INICIO-TEMPO               PIC 9(18) COMP VALUE ZEROS.
       01 WS-FIM-TEMPO                  PIC 9(18) COMP VALUE ZEROS.
       01 WS-DURACAO                    PIC 9(9) COMP VALUE ZEROS.
       
       * Estrutura para métricas com atributos de contexto
       01 WS-BUSINESS-HIST-METRIC.
          05 WS-HIST-ID                 PIC 9(3).
          05 WS-BUCKET-ID               PIC 9(2).
          05 WS-BUCKET-VALOR            PIC 9(9).
          05 WS-HIST-CONTEXT.
             10 WS-BUSINESS-DOMAIN      PIC X(15).
             10 WS-TRANSACTION-TYPE     PIC X(15).
             10 WS-ENTITY-TYPE          PIC X(15).
             10 WS-PROCESS-NAME         PIC X(20).
             10 WS-PRIORITY-LEVEL       PIC X(6).
       
       * Variáveis para controle CICS
       01 WS-RESP                       PIC S9(8) COMP.
       01 WS-RESP2                      PIC S9(8) COMP.
       01 WS-CONTAINER-NAME             PIC X(16) VALUE 'HIST-CONTAINER'.
       01 WS-CHANNEL-NAME               PIC X(16) VALUE 'HIST-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Iniciar medição de tempo
           EXEC CICS ASKTIME ABSTIME(WS-INICIO-TEMPO) END-EXEC
           
           * Simular processamento de negócio
           PERFORM PROCESS-BUSINESS-TRANSACTION
           
           * Finalizar medição de tempo
           EXEC CICS ASKTIME ABSTIME(WS-FIM-TEMPO) END-EXEC
           
           * Calcular duração em milissegundos
           COMPUTE WS-DURACAO = 
                  (WS-FIM-TEMPO - WS-INICIO-TEMPO) / 1000
           
           * Registrar duração no histograma
           PERFORM RECORD-DURATION-HISTOGRAM
           
           * Exportar histogramas
           PERFORM EXPORT-BUSINESS-HISTOGRAMS
           
           EXEC CICS RETURN END-EXEC.
       
       PROCESS-BUSINESS-TRANSACTION.
           * Simulação de processamento de negócio
           * Este é apenas um exemplo para demonstrar o conceito
           EXEC CICS DELAY FOR SECONDS(1) END-EXEC.
       
       RECORD-DURATION-HISTOGRAM.
           ADD WS-DURACAO TO WS-DURATION-SUM
           ADD 1 TO WS-DURATION-COUNT
           
           * Incrementar o bucket apropriado
           EVALUATE TRUE
              WHEN WS-DURACAO <= 100
                 ADD 1 TO WS-BUCKET-0-100-MS
              WHEN WS-DURACAO <= 500
                 ADD 1 TO WS-BUCKET-101-500-MS
              WHEN WS-DURACAO <= 1000
                 ADD 1 TO WS-BUCKET-501-1000-MS
              WHEN WS-DURACAO <= 5000
                 ADD 1 TO WS-BUCKET-1001-5000-MS
              WHEN OTHER
                 ADD 1 TO WS-BUCKET-5001-PLUS-MS
           END-EVALUATE.
       
       EXPORT-BUSINESS-HISTOGRAMS.
           * Preparar contexto comum
           MOVE 'BANKING' TO WS-BUSINESS-DOMAIN
           MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
           MOVE 'ACCOUNT' TO WS-ENTITY-TYPE
           MOVE 'FUNDS-TRANSFER' TO WS-PROCESS-NAME
           MOVE 'HIGH' TO WS-PRIORITY-LEVEL
           
           * Exportar histograma de duração (ID: 610)
           MOVE 610 TO WS-HIST-ID
           
           * Exportar cada bucket
           MOVE 1 TO WS-BUCKET-ID
           MOVE WS-BUCKET-0-100-MS TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 2 TO WS-BUCKET-ID
           MOVE WS-BUCKET-101-500-MS TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 3 TO WS-BUCKET-ID
           MOVE WS-BUCKET-501-1000-MS TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 4 TO WS-BUCKET-ID
           MOVE WS-BUCKET-1001-5000-MS TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 5 TO WS-BUCKET-ID
           MOVE WS-BUCKET-5001-PLUS-MS TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET
           
           * Exportar soma e contagem para cálculo de média
           MOVE 6 TO WS-BUCKET-ID
           MOVE WS-DURATION-SUM TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 7 TO WS-BUCKET-ID
           MOVE WS-DURATION-COUNT TO WS-BUCKET-VALOR
           PERFORM SEND-HISTOGRAM-BUCKET.
       
       SEND-HISTOGRAM-BUCKET.
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-BUSINESS-HIST-METRIC)
              FLENGTH(LENGTH OF WS-BUSINESS-HIST-METRIC)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC
           
           IF WS-RESP = DFHRESP(NORMAL)
              EXEC CICS LINK PROGRAM('HISTCOLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
```

## Cálculo de Percentis

Para calcular percentis a partir dos buckets do histograma:

```cobol
       CALCULATE-PERCENTILES.
           * Calcular o total de observações
           COMPUTE WS-TOTAL-OBSERVATIONS = WS-DURATION-COUNT
           
           * Número de observações para cada percentil
           COMPUTE WS-P50-COUNT = WS-TOTAL-OBSERVATIONS * 0.5
           COMPUTE WS-P90-COUNT = WS-TOTAL-OBSERVATIONS * 0.9
           COMPUTE WS-P99-COUNT = WS-TOTAL-OBSERVATIONS * 0.99
           
           * Contador acumulado
           MOVE ZEROS TO WS-ACCUMULATED-COUNT
           
           * P50 (Mediana)
           ADD WS-BUCKET-0-100-MS TO WS-ACCUMULATED-COUNT
           IF WS-ACCUMULATED-COUNT >= WS-P50-COUNT
              MOVE 100 TO WS-P50-VALUE
           ELSE
              ADD WS-BUCKET-101-500-MS TO WS-ACCUMULATED-COUNT
              IF WS-ACCUMULATED-COUNT >= WS-P50-COUNT
                 MOVE 500 TO WS-P50-VALUE
              ELSE
                 * Continuar para os outros buckets
                 * ...
              END-IF
           END-IF.
```

## Implementação Alternativa: Histograma de Valores Brutos

Uma abordagem alternativa é coletar e enviar valores brutos de observação para um coletor de métricas externo para cálculo de buckets. Esta abordagem:

1. Reduz a complexidade do programa COBOL
2. Torna a configuração de buckets do histograma mais flexível (pode ser alterada sem modificar o código COBOL)
3. Diminui o tamanho dos registros de métricas
4. Permite análises mais avançadas nos dados brutos

Veja como implementar esta abordagem:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAWHIST.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Buffer para armazenar valores brutos de observação
       01 WS-VALORES-BRUTOS.
          05 WS-MAX-OBSERVACOES         PIC 9(4) COMP VALUE 100.
          05 WS-OBSERVACOES-COUNT       PIC 9(4) COMP VALUE ZEROS.
          05 WS-OBSERVACOES OCCURS 100 TIMES.
             10 WS-OBSERVACAO-VALOR     PIC 9(9)V99 COMP-3.
             10 WS-OBSERVACAO-TEMPO     PIC 9(18) COMP.
       
       * Estrutura para enviar valores brutos
       01 WS-METRICA-BRUTA.
          05 WS-METRICA-ID              PIC 9(3).
          05 WS-OBSERVACAO-VALOR        PIC 9(9)V99 COMP-3.
          05 WS-METRICA-ATRIBUTOS.
             10 WS-BUSINESS-DOMAIN      PIC X(15).
             10 WS-TRANSACTION-TYPE     PIC X(15).
             10 WS-ENTITY-TYPE          PIC X(15).
             10 WS-OBSERVACAO-TEMPO     PIC 9(18) COMP.
       
       * Variáveis para medição de tempo
       01 WS-INICIO-TEMPO               PIC 9(18) COMP VALUE ZEROS.
       01 WS-FIM-TEMPO                  PIC 9(18) COMP VALUE ZEROS.
       01 WS-DURACAO                    PIC 9(9)V99 COMP-3 VALUE ZEROS.
       
       * Variáveis para controle CICS
       01 WS-RESP                       PIC S9(8) COMP.
       01 WS-RESP2                      PIC S9(8) COMP.
       01 WS-CONTAINER-NAME             PIC X(16) VALUE 'RAW-CONTAINER'.
       01 WS-CHANNEL-NAME               PIC X(16) VALUE 'RAW-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Limpar o buffer de observações
           PERFORM INICIALIZAR-OBSERVACOES
           
           * Simular coleta de múltiplas observações
           PERFORM VARYING WS-IDX FROM 1 BY 1 
              UNTIL WS-IDX > 10
              
              * Iniciar medição de tempo
              EXEC CICS ASKTIME ABSTIME(WS-INICIO-TEMPO) END-EXEC
              
              * Simular algum processamento
              PERFORM PROCESS-BUSINESS-TRANSACTION
              
              * Finalizar medição de tempo
              EXEC CICS ASKTIME ABSTIME(WS-FIM-TEMPO) END-EXEC
              
              * Calcular e registrar duração
              COMPUTE WS-DURACAO = 
                     (WS-FIM-TEMPO - WS-INICIO-TEMPO) / 1000
              
              * Armazenar a observação bruta
              PERFORM ARMAZENAR-OBSERVACAO
           END-PERFORM
           
           * Exportar as observações brutas
           PERFORM EXPORTAR-OBSERVACOES-BRUTAS
           
           EXEC CICS RETURN END-EXEC.
       
       INICIALIZAR-OBSERVACOES.
           MOVE ZEROS TO WS-OBSERVACOES-COUNT.
       
       PROCESS-BUSINESS-TRANSACTION.
           * Simular algum processamento de negócio com duração variável
           EXEC CICS DELAY FOR MICROSECONDS(50000 + 
                FUNCTION RANDOM * 100000)
           END-EXEC.
       
       ARMAZENAR-OBSERVACAO.
           * Verificar se o buffer está cheio
           IF WS-OBSERVACOES-COUNT < WS-MAX-OBSERVACOES
              * Incrementar contador
              ADD 1 TO WS-OBSERVACOES-COUNT
              
              * Armazenar o valor e timestamp
              MOVE WS-DURACAO TO 
                 WS-OBSERVACAO-VALOR(WS-OBSERVACOES-COUNT)
              MOVE WS-FIM-TEMPO TO 
                 WS-OBSERVACAO-TEMPO(WS-OBSERVACOES-COUNT)
           END-IF.
       
       EXPORTAR-OBSERVACOES-BRUTAS.
           * ID da métrica para duração (610)
           MOVE 610 TO WS-METRICA-ID
           
           * Definir atributos comuns
           MOVE 'BANKING' TO WS-BUSINESS-DOMAIN
           MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
           MOVE 'ACCOUNT' TO WS-ENTITY-TYPE
           
           * Enviar cada observação individualmente
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-OBSERVACOES-COUNT
              
              * Definir o valor e timestamp para esta observação
              MOVE WS-OBSERVACAO-VALOR(WS-IDX) TO WS-OBSERVACAO-VALOR
              MOVE WS-OBSERVACAO-TEMPO(WS-IDX) TO WS-OBSERVACAO-TEMPO
              
              * Enviar a observação
              PERFORM ENVIAR-OBSERVACAO-BRUTA
           END-PERFORM.
       
       ENVIAR-OBSERVACAO-BRUTA.
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-METRICA-BRUTA)
              FLENGTH(LENGTH OF WS-METRICA-BRUTA)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC
           
           IF WS-RESP = DFHRESP(NORMAL)
              EXEC CICS LINK PROGRAM('RAWCOLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
```

### Alternativa para Processamento em Lotes

Para programas batch ou quando muitas observações precisam ser enviadas de uma vez, uma abordagem mais eficiente é usar armazenamento temporário ou arquivos:

```cobol
       GRAVAR-OBSERVACOES-BRUTAS-EM-TS.
           * Criar um nome de fila único (por exemplo, baseado em timestamp)
           MOVE CURRENT-TIMESTAMP TO WS-QUEUE-SUFFIX
           STRING 'RAWHIST-' WS-QUEUE-SUFFIX 
              DELIMITED BY SIZE INTO WS-QUEUE-NAME
           
           * Gravar registro de metadados
           MOVE 610 TO WS-METRICA-ID
           MOVE 'BANKING' TO WS-BUSINESS-DOMAIN
           MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
           MOVE 'ACCOUNT' TO WS-ENTITY-TYPE
           MOVE WS-OBSERVACOES-COUNT TO WS-RECORD-COUNT
           
           EXEC CICS WRITEQ TS
              QUEUE(WS-QUEUE-NAME)
              FROM(WS-METADATA-RECORD)
              LENGTH(LENGTH OF WS-METADATA-RECORD)
              ITEM(1)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC
           
           * Gravar todas as observações em um único registro
           EXEC CICS WRITEQ TS
              QUEUE(WS-QUEUE-NAME)
              FROM(WS-VALORES-BRUTOS)
              LENGTH(LENGTH OF WS-VALORES-BRUTOS)
              ITEM(2)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
```

## Processamento do Coletor Externo

O coletor externo que recebe as observações brutas é responsável por:

1. Agregar observações de múltiplos programas COBOL
2. Calcular buckets de histograma baseados em limiares configuráveis
3. Computar percentis e outras estatísticas
4. Exportar as métricas finais no formato necessário para sistemas como Prometheus

Isso fornece a flexibilidade para:

- Alterar limites de bucket sem modificar o código COBOL
- Aplicar diferentes estratégias de bucketing para diferentes tipos de análise
- Armazenar dados brutos para análises mais avançadas quando necessário
- Otimizar o formato das métricas para o sistema de observabilidade alvo

## Integração com Prometheus

Para integração com o Prometheus, o coletor externo transforma os dados brutos em buckets de histograma:

```
# HELP cobol_business_transaction_duration_seconds Duração das transações de negócio
# TYPE cobol_business_transaction_duration_seconds histogram
cobol_business_transaction_duration_seconds_bucket{le="0.1",business_domain="BANKING",transaction_type="PAYMENT"} 10
cobol_business_transaction_duration_seconds_bucket{le="0.5",business_domain="BANKING",transaction_type="PAYMENT"} 25
cobol_business_transaction_duration_seconds_bucket{le="1",business_domain="BANKING",transaction_type="PAYMENT"} 35
cobol_business_transaction_duration_seconds_bucket{le="5",business_domain="BANKING",transaction_type="PAYMENT"} 45
cobol_business_transaction_duration_seconds_bucket{le="+Inf",business_domain="BANKING",transaction_type="PAYMENT"} 50
cobol_business_transaction_duration_seconds_sum{business_domain="BANKING",transaction_type="PAYMENT"} 75.3
cobol_business_transaction_duration_seconds_count{business_domain="BANKING",transaction_type="PAYMENT"} 50
```

## Considerações de Performance

Ao implementar histogramas para métricas de negócio em COBOL, considere:

1. **Escolha dos buckets**: Defina faixas de valores que façam sentido para seu domínio de negócio.

2. **Precisão vs. Performance**: Balance o número de buckets com o overhead de processamento.

3. **Armazenamento eficiente**: Use PIC 9(9) COMP para valores inteiros e PIC 9(9)V99 COMP-3 para valores decimais.

4. **Frequência de exportação**: Considere o impacto de exportar grandes volumes de dados de histogramas.

## Comparação das Abordagens

| Aspecto | Buckets em COBOL | Buckets Externos |
|--------|-----------------|------------------|
| Complexidade do Código COBOL | Maior | Menor |
| Transmissão em Rede | Menos dados | Mais dados brutos |
| Flexibilidade de Buckets | Fixo a menos que o código seja alterado | Pode ser alterado sem atualizar o código |
| Requisitos de Armazenamento | Pré-agregado, menor | Dados brutos, maior |
| Sobrecarga de Processamento | Mais no programa COBOL | Mais no coletor externo |
| Flexibilidade de Análise | Limitada aos buckets pré-definidos | Acesso completo aos dados brutos |

## Vantagens dos Histogramas para Métricas de Negócio

1. **Análise detalhada**: Permite entender a distribuição dos tempos de processamento de transações de negócio.

2. **Identificação de gargalos**: Ajuda a identificar problemas de performance em processos específicos.

3. **SLO/SLA**: Facilita o monitoramento de acordos de nível de serviço baseados em percentis.

4. **Otimização de processos**: Fornece insights para melhorar a eficiência dos processos de negócio.

5. **Comparação entre domínios**: Permite comparar performance entre diferentes tipos de transações ou domínios de negócio. 