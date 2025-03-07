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

## Integração com Prometheus

Para integrar com Prometheus, o formato de saída para histogramas de negócio seria:

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

3. **Armazenamento eficiente**: Use PIC 9(9) COMP para contadores e PIC 9(9)V99 COMP-3 para valores decimais.

4. **Frequência de exportação**: Considere o impacto de exportar grandes volumes de dados de histogramas.

## Vantagens dos Histogramas para Métricas de Negócio

1. **Análise detalhada**: Permite entender a distribuição dos tempos de processamento de transações de negócio.

2. **Identificação de gargalos**: Ajuda a identificar problemas de performance em processos específicos.

3. **SLO/SLA**: Facilita o monitoramento de acordos de nível de serviço baseados em percentis.

4. **Otimização de processos**: Fornece insights para melhorar a eficiência dos processos de negócio.

5. **Comparação entre domínios**: Permite comparar performance entre diferentes tipos de transações ou domínios de negócio. 