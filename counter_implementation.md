# Implementação de Contadores (Counters) em COBOL

Este documento descreve como implementar contadores para métricas em aplicações COBOL, seguindo as convenções do OpenTelemetry.

## Conceito

Em observabilidade, um contador (counter) é uma métrica cumulativa que representa um valor único que só pode aumentar ou ser redefinido para zero. Contadores são usados para medir eventos discretos ao longo do tempo, como:
- Número de transações processadas
- Quantidade de erros encontrados
- Total de registros lidos ou escritos

## Estrutura Básica em COBOL

Em COBOL, os contadores podem ser implementados como variáveis numéricas que são incrementadas quando ocorre um evento específico.

```cobol
       WORKING-STORAGE SECTION.
       01 WS-CONTADORES.
          05 WS-CONTADOR-LEITURAS        PIC 9(9) COMP VALUE ZEROS.
          05 WS-CONTADOR-ESCRITAS        PIC 9(9) COMP VALUE ZEROS.
          05 WS-CONTADOR-ERROS           PIC 9(9) COMP VALUE ZEROS.
```

## Contadores para Métricas de Negócio Genéricas

Para implementar contadores de métricas de negócio que sejam aplicáveis a qualquer domínio, podemos usar a seguinte estrutura:

```cobol
       WORKING-STORAGE SECTION.
       * Contadores genéricos de negócio
       01 WS-BUSINESS-CONTADORES.
          05 WS-TRANS-STARTED          PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-COMPLETED        PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-FAILED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-CANCELED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-PERFORMED        PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-PASSED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-FAILED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-CREATED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-UPDATED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-DELETED         PIC 9(9) COMP VALUE ZEROS.
          
       * Estrutura para métricas com atributos de contexto
       01 WS-BUSINESS-METRICA.
          05 WS-METRICA-ID             PIC 9(3).
          05 WS-METRICA-VALOR          PIC 9(9).
          05 WS-METRICA-CONTEXT.
             10 WS-BUSINESS-DOMAIN     PIC X(15).
             10 WS-TRANSACTION-TYPE    PIC X(15).
             10 WS-ENTITY-TYPE         PIC X(15).
             10 WS-PROCESS-NAME        PIC X(20).
             10 WS-PRIORITY-LEVEL      PIC X(6).
```

Estas estruturas permitem capturar eventos de negócio em qualquer domínio, enquanto os atributos contextuais fornecem a diferenciação específica do setor.

## Implementação Completa

Abaixo está um exemplo completo de como implementar contadores em um programa COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTER-EXAMPLE.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Definição dos contadores
       01 WS-CONTADORES.
          05 WS-CONTADOR-LEITURAS        PIC 9(9) COMP VALUE ZEROS.
          05 WS-CONTADOR-ESCRITAS        PIC 9(9) COMP VALUE ZEROS.
          05 WS-CONTADOR-ERROS           PIC 9(9) COMP VALUE ZEROS.
          05 WS-CONTADOR-VALIDACOES      PIC 9(9) COMP VALUE ZEROS.
          05 WS-CONTADOR-TRANSACOES      PIC 9(9) COMP VALUE ZEROS.
       
       * Definição da estrutura para exportar métricas
       01 WS-METRICA.
          05 WS-METRICA-ID               PIC 9(3).
          05 WS-METRICA-VALOR            PIC 9(9).
          05 WS-METRICA-ATRIBUTOS.
             10 WS-PROGRAM-ID            PIC X(8).
             10 WS-TRANSACTION-ID        PIC X(4).
             10 WS-FILE-NAME             PIC X(8).
       
       * Variáveis para controle de erros CICS
       01 WS-RESP                        PIC S9(8) COMP.
       01 WS-RESP2                       PIC S9(8) COMP.
       
       * Nome do container e canal para CICS
       01 WS-CONTAINER-NAME              PIC X(16) VALUE 'METRIC-CONTAINER'.
       01 WS-CHANNEL-NAME                PIC X(16) VALUE 'METRIC-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Iniciar processamento principal
           PERFORM PROCESS-BUSINESS-LOGIC.
           
           * Exportar as métricas coletadas
           PERFORM EXPORT-METRICS.
           
           EXEC CICS RETURN END-EXEC.
           
       PROCESS-BUSINESS-LOGIC.
           * Simular operações de negócio que incrementam contadores
           
           * Simulação: Leitura de registro
           ADD 1 TO WS-CONTADOR-LEITURAS.
           
           * Simulação: Processamento com validação
           ADD 1 TO WS-CONTADOR-VALIDACOES.
           
           * Simulação: Escrita de registro (algumas vezes)
           IF WS-CONTADOR-LEITURAS > 0
              ADD 1 TO WS-CONTADOR-ESCRITAS
           END-IF.
           
           * Simulação: Ocorrência de erro (ocasional)
           IF WS-CONTADOR-LEITURAS > 5
              ADD 1 TO WS-CONTADOR-ERROS
           END-IF.
           
           * Incrementar contador de transações
           ADD 1 TO WS-CONTADOR-TRANSACOES.
       
       EXPORT-METRICS.
           * Exportar cada contador como uma métrica separada
           
           * Contador de leituras (ID: 101)
           MOVE 101 TO WS-METRICA-ID.
           MOVE WS-CONTADOR-LEITURAS TO WS-METRICA-VALOR.
           MOVE 'CNTREXMP' TO WS-PROGRAM-ID.
           MOVE 'METR' TO WS-TRANSACTION-ID.
           MOVE 'DATAFILE' TO WS-FILE-NAME.
           PERFORM SEND-METRIC.
           
           * Contador de escritas (ID: 102)
           MOVE 102 TO WS-METRICA-ID.
           MOVE WS-CONTADOR-ESCRITAS TO WS-METRICA-VALOR.
           PERFORM SEND-METRIC.
           
           * Contador de erros (ID: 105)
           MOVE 105 TO WS-METRICA-ID.
           MOVE WS-CONTADOR-ERROS TO WS-METRICA-VALOR.
           PERFORM SEND-METRIC.
           
           * Contador de validações (ID: 601)
           MOVE 601 TO WS-METRICA-ID.
           MOVE WS-CONTADOR-VALIDACOES TO WS-METRICA-VALOR.
           PERFORM SEND-METRIC.
           
           * Contador de transações (ID: 401)
           MOVE 401 TO WS-METRICA-ID.
           MOVE WS-CONTADOR-TRANSACOES TO WS-METRICA-VALOR.
           PERFORM SEND-METRIC.
           
       SEND-METRIC.
           * Criar container CICS para a métrica
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-METRICA)
              FLENGTH(LENGTH OF WS-METRICA)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
           
           * Verificar se a operação foi bem-sucedida
           IF WS-RESP = DFHRESP(NORMAL)
              * Chamar programa coletor de métricas
              EXEC CICS LINK PROGRAM('METCOLLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
```

## Implementação para Métricas Genéricas de Negócio

Aqui está um exemplo específico para implementação de contadores de negócio genéricos:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSNMETRIC.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Contadores de transações de negócio
       01 WS-BUSINESS-CONTADORES.
          05 WS-TRANS-STARTED          PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-COMPLETED        PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-FAILED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-CANCELED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-PERFORMED        PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-PASSED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-FAILED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-CREATED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-UPDATED         PIC 9(9) COMP VALUE ZEROS.
       
       * Estrutura para métricas com atributos de contexto
       01 WS-BUSINESS-METRICA.
          05 WS-METRICA-ID             PIC 9(3).
          05 WS-METRICA-VALOR          PIC 9(9).
          05 WS-METRICA-CONTEXT.
             10 WS-BUSINESS-DOMAIN     PIC X(15).
             10 WS-TRANSACTION-TYPE    PIC X(15).
             10 WS-ENTITY-TYPE         PIC X(15).
             10 WS-PROCESS-NAME        PIC X(20).
             10 WS-PRIORITY-LEVEL      PIC X(6).
       
       * Variáveis para controle CICS
       01 WS-RESP                      PIC S9(8) COMP.
       01 WS-RESP2                     PIC S9(8) COMP.
       01 WS-CONTAINER-NAME            PIC X(16) VALUE 'BSN-CONTAINER'.
       01 WS-CHANNEL-NAME              PIC X(16) VALUE 'BSN-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Iniciar processamento de transação de negócio
           PERFORM PROCESS-BUSINESS-TRANSACTION.
           
           * Exportar métricas coletadas
           PERFORM EXPORT-BUSINESS-METRICS.
           
           EXEC CICS RETURN END-EXEC.
       
       PROCESS-BUSINESS-TRANSACTION.
           * Incrementar contador de transações iniciadas
           ADD 1 TO WS-TRANS-STARTED
           
           * Simulação: Realizar validações de negócio
           PERFORM VALIDATE-BUSINESS-RULES
           
           * Simulação: Processar entidade (criação ou atualização)
           PERFORM PROCESS-BUSINESS-ENTITY
           
           * Incrementar contador de transações completadas
           ADD 1 TO WS-TRANS-COMPLETED.
       
       VALIDATE-BUSINESS-RULES.
           * Incrementar contador de validações realizadas
           ADD 1 TO WS-VALID-PERFORMED
           
           * Simulação: Validação bem-sucedida (95% do tempo)
           IF FUNCTION RANDOM() > 0.05
              ADD 1 TO WS-VALID-PASSED
           ELSE
              ADD 1 TO WS-VALID-FAILED
           END-IF.
       
       PROCESS-BUSINESS-ENTITY.
           * Simulação: Criar nova entidade (30% do tempo)
           IF FUNCTION RANDOM() < 0.3
              ADD 1 TO WS-ENTITY-CREATED
           ELSE
              ADD 1 TO WS-ENTITY-UPDATED
           END-IF.
       
       EXPORT-BUSINESS-METRICS.
           * Preparar contexto comum para todas as métricas
           MOVE 'BANKING' TO WS-BUSINESS-DOMAIN
           MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
           MOVE 'ACCOUNT' TO WS-ENTITY-TYPE
           MOVE 'FUNDS-TRANSFER' TO WS-PROCESS-NAME
           MOVE 'HIGH' TO WS-PRIORITY-LEVEL
           
           * Exportar métricas de contador
           MOVE 601 TO WS-METRICA-ID
           MOVE WS-TRANS-STARTED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 602 TO WS-METRICA-ID
           MOVE WS-TRANS-COMPLETED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 651 TO WS-METRICA-ID
           MOVE WS-VALID-PERFORMED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 652 TO WS-METRICA-ID
           MOVE WS-VALID-PASSED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 653 TO WS-METRICA-ID
           MOVE WS-VALID-FAILED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 671 TO WS-METRICA-ID
           MOVE WS-ENTITY-CREATED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 672 TO WS-METRICA-ID
           MOVE WS-ENTITY-UPDATED TO WS-METRICA-VALOR
           PERFORM SEND-BUSINESS-METRIC.
       
       SEND-BUSINESS-METRIC.
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-BUSINESS-METRICA)
              FLENGTH(LENGTH OF WS-BUSINESS-METRICA)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC
           
           IF WS-RESP = DFHRESP(NORMAL)
              EXEC CICS LINK PROGRAM('BSNCOLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
```

## Implementação Alternativa com TSQ (Temporary Storage Queue)

Outra abordagem comum é usar TSQs do CICS para armazenar métricas:

```cobol
       EXPORT-METRIC-TSQ.
           * Preparar dados da métrica
           MOVE 101 TO WS-METRICA-ID.
           MOVE WS-CONTADOR-LEITURAS TO WS-METRICA-VALOR.
           
           * Gravar métrica em uma TSQ para coleta posterior
           EXEC CICS WRITEQ TS
              QUEUE('METRICSQ')
              FROM(WS-METRICA)
              LENGTH(LENGTH OF WS-METRICA)
              ITEM(1)
              REWRITE
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
```

## Implementação com Shared Memory (COMMAREA em CICS)

Para métricas que precisam ser compartilhadas entre transações:

```cobol
       * Em um programa de inicialização ou controle
       INITIALIZE-SHARED-COUNTERS.
           EXEC CICS GET CONTAINER('SHARED-COUNTERS')
              CHANNEL('GLOBAL-METRICS')
              INTO(WS-CONTADORES)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
           
           IF WS-RESP NOT = DFHRESP(NORMAL)
              INITIALIZE WS-CONTADORES
              EXEC CICS PUT CONTAINER('SHARED-COUNTERS')
                 CHANNEL('GLOBAL-METRICS')
                 FROM(WS-CONTADORES)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
           
       * Em qualquer programa que precise atualizar os contadores
       UPDATE-SHARED-COUNTERS.
           EXEC CICS GET CONTAINER('SHARED-COUNTERS')
              CHANNEL('GLOBAL-METRICS')
              INTO(WS-CONTADORES)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
           
           ADD 1 TO WS-CONTADOR-LEITURAS.
           
           EXEC CICS PUT CONTAINER('SHARED-COUNTERS')
              CHANNEL('GLOBAL-METRICS')
              FROM(WS-CONTADORES)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
```

## Considerações de Performance

Ao implementar contadores em COBOL, é importante considerar:

1. **Uso de instruções COMP (COMPUTATIONAL)**: Armazene contadores como PIC 9(9) COMP para melhor performance.

2. **Frequência de atualização**: Em loops de alto volume, considere incrementar contadores em lotes (por exemplo, a cada 100 processamentos) para reduzir o overhead.

3. **Atomicidade**: Em ambientes multithread ou sistemas compartilhados, garanta a atomicidade das operações de incremento.

4. **Limites de tamanho**: Certifique-se de que seus contadores sejam grandes o suficiente para não sofrer overflow durante o ciclo de vida esperado.

## Integração com Prometheus

Para integrar com Prometheus, o formato de saída para métricas de negócio seria algo como:

```
# HELP cobol_business_transaction_started_total Total de transações de negócio iniciadas
# TYPE cobol_business_transaction_started_total counter
cobol_business_transaction_started_total{business_domain="BANKING",transaction_type="PAYMENT",entity_type="ACCOUNT",process_name="FUNDS-TRANSFER",priority="HIGH"} 1500
```

O programa COBOL pode gerar este formato em um arquivo ou disponibilizá-lo por meio de uma API para que um exportador do Prometheus possa coletá-lo.

## Vantagens das Métricas Genéricas de Negócio

A abordagem de métricas genéricas de negócio com atributos contextuais oferece várias vantagens:

1. **Reusabilidade do código**: O mesmo código de instrumentação pode ser reutilizado em diferentes programas COBOL, independentemente do domínio de negócio.

2. **Flexibilidade analítica**: Os atributos permitem que analistas de dados filtrem e agrupem métricas de acordo com diferentes dimensões de negócio.

3. **Rastreabilidade end-to-end**: Ao manter consistência nos atributos entre diferentes serviços, é possível correlacionar métricas entre diferentes sistemas.

4. **Evolução independente**: É possível adicionar novos atributos contextuais sem alterar a estrutura básica das métricas.

5. **Integração simplificada**: Sistemas modernos de observabilidade como Prometheus podem facilmente consumir esse formato padronizado. 