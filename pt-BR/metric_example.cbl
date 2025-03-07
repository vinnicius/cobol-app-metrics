       IDENTIFICATION DIVISION.
       PROGRAM-ID. METRICEX.
       AUTHOR. COBOL-METRICS-TEAM.
      
      *----------------------------------------------------------------*
      * Exemplo de programa COBOL com instrumentação de métricas       *
      * utilizando identificadores de atributos otimizados             *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *----------------------------------------------------------------*
      * Definição de estrutura para métricas com atributos             *
      *----------------------------------------------------------------*
       01 WS-METRIC.
          05 WS-METRIC-ID                PIC 9(3).
          05 WS-METRIC-VALUE             PIC 9(9) COMP.
          05 WS-ATTR-COUNT               PIC 9(2) VALUE ZEROS.
          05 WS-ATTRIBUTES OCCURS 10 TIMES.
             10 WS-ATTR-CODE             PIC 9(2).
             10 WS-ATTR-VALUE            PIC X(16).
       
      *----------------------------------------------------------------*
      * Contadores para instrumentação                                 *
      *----------------------------------------------------------------*
       01 WS-COUNTERS.
          05 WS-FILE-READS               PIC 9(9) COMP VALUE ZEROS.
          05 WS-DB-SELECTS               PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-STARTED            PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-COMPLETED          PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-ERRORS             PIC 9(9) COMP VALUE ZEROS.
       
      *----------------------------------------------------------------*
      * Variáveis para medição de tempo (histogramas)                  *
      *----------------------------------------------------------------*
       01 WS-TIMERS.
          05 WS-TRANS-START-TIME         PIC 9(18) COMP VALUE ZEROS.
          05 WS-TRANS-END-TIME           PIC 9(18) COMP VALUE ZEROS.
          05 WS-TRANS-DURATION-MS        PIC 9(9) COMP VALUE ZEROS.
       
      *----------------------------------------------------------------*
      * Histograma para duração de transação                           *
      *----------------------------------------------------------------*
       01 WS-TRANSACTION-HISTOGRAM.
          05 WS-BUCKET-0-100-MS          PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500-MS        PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-1000-MS       PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-1001-5000-MS      PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-5001-PLUS-MS      PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-SUM             PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-COUNT           PIC 9(9) COMP VALUE ZEROS.
       
      *----------------------------------------------------------------*
      * Variáveis para controle CICS                                   *
      *----------------------------------------------------------------*
       01 WS-CICS-CONTROL.
          05 WS-RESP                     PIC S9(8) COMP.
          05 WS-RESP2                    PIC S9(8) COMP.
          05 WS-CONTAINER-NAME           PIC X(16) VALUE 'METRIC-CONTAIN'.
          05 WS-CHANNEL-NAME             PIC X(16) VALUE 'METRIC-CHANNEL'.
       
      *----------------------------------------------------------------*
      * Variáveis para simulação de processamento                      *
      *----------------------------------------------------------------*
       01 WS-PROCESS-VARS.
          05 WS-CUSTOMER-ID              PIC X(8).
          05 WS-ACCOUNT-NUMBER           PIC X(12).
          05 WS-TRANSACTION-AMOUNT       PIC 9(9)V99 COMP-3.
          05 WS-ERROR-CODE               PIC X(4).
          05 WS-ERROR-FLAG               PIC X VALUE 'N'.
             88 WS-ERROR-OCCURRED        VALUE 'Y'.
             88 WS-NO-ERROR              VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *----------------------------------------------------------------*
      * Iniciar processamento principal                                *
      *----------------------------------------------------------------*
      
           * Iniciar timer para medição da transação
           EXEC CICS ASKTIME ABSTIME(WS-TRANS-START-TIME)
           END-EXEC
           
           * Incrementar contador de transações iniciadas
           ADD 1 TO WS-TRANS-STARTED
           
           * Executar processamento simulado
           PERFORM PROCESS-BUSINESS-TRANSACTION
           
           * Finalizar timer e calcular duração
           EXEC CICS ASKTIME ABSTIME(WS-TRANS-END-TIME)
           END-EXEC
           
           COMPUTE WS-TRANS-DURATION-MS = 
              (WS-TRANS-END-TIME - WS-TRANS-START-TIME) / 1000
           
           * Registrar duração no histograma
           PERFORM RECORD-TRANSACTION-DURATION
           
           * Verificar resultado e registrar contadores
           IF WS-ERROR-OCCURRED
              ADD 1 TO WS-TRANS-ERRORS
           ELSE
              ADD 1 TO WS-TRANS-COMPLETED
           END-IF
           
           * Exportar as métricas coletadas
           PERFORM EXPORT-METRICS
           
           EXEC CICS RETURN
           END-EXEC
           .
       
       PROCESS-BUSINESS-TRANSACTION.
      *----------------------------------------------------------------*
      * Simulação de processamento de negócio                          *
      *----------------------------------------------------------------*
      
           * Simulação: Ler arquivo de cliente
           MOVE 'C1234567' TO WS-CUSTOMER-ID
           ADD 1 TO WS-FILE-READS
           
           * Simulação: Consultar dados em banco
           MOVE '000123456789' TO WS-ACCOUNT-NUMBER
           ADD 1 TO WS-DB-SELECTS
           
           * Simulação de processamento (com possibilidade de erro)
           MOVE 1250.75 TO WS-TRANSACTION-AMOUNT
           
           * Simular erro em 10% dos casos
           IF FUNCTION RANDOM < 0.1
              MOVE 'Y' TO WS-ERROR-FLAG
              MOVE 'E123' TO WS-ERROR-CODE
           ELSE
              MOVE 'N' TO WS-ERROR-FLAG
           END-IF
           .
       
       RECORD-TRANSACTION-DURATION.
      *----------------------------------------------------------------*
      * Registrar a duração da transação no histograma apropriado      *
      *----------------------------------------------------------------*
      
           * Adicionar valor à soma e incrementar contagem
           ADD WS-TRANS-DURATION-MS TO WS-DURATION-SUM
           ADD 1 TO WS-DURATION-COUNT
           
           * Registrar no bucket apropriado
           EVALUATE TRUE
              WHEN WS-TRANS-DURATION-MS <= 100
                 ADD 1 TO WS-BUCKET-0-100-MS
              WHEN WS-TRANS-DURATION-MS <= 500
                 ADD 1 TO WS-BUCKET-101-500-MS
              WHEN WS-TRANS-DURATION-MS <= 1000
                 ADD 1 TO WS-BUCKET-501-1000-MS
              WHEN WS-TRANS-DURATION-MS <= 5000
                 ADD 1 TO WS-BUCKET-1001-5000-MS
              WHEN OTHER
                 ADD 1 TO WS-BUCKET-5001-PLUS-MS
           END-EVALUATE
           .
       
       EXPORT-METRICS.
      *----------------------------------------------------------------*
      * Exportar métricas coletadas usando atributos otimizados        *
      *----------------------------------------------------------------*
      
           * Exportar contador de leituras de arquivo (101)
           MOVE 101 TO WS-METRIC-ID
           MOVE WS-FILE-READS TO WS-METRIC-VALUE
           PERFORM SETUP-FILE-READ-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Exportar contador de seleções DB (201)
           MOVE 201 TO WS-METRIC-ID
           MOVE WS-DB-SELECTS TO WS-METRIC-VALUE
           PERFORM SETUP-DB-SELECT-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Exportar contador de transações iniciadas (601)
           MOVE 601 TO WS-METRIC-ID
           MOVE WS-TRANS-STARTED TO WS-METRIC-VALUE
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Exportar contador de transações completadas (602)
           MOVE 602 TO WS-METRIC-ID
           MOVE WS-TRANS-COMPLETED TO WS-METRIC-VALUE
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Exportar contador de erros (se houver)
           IF WS-TRANS-ERRORS > 0
              MOVE 603 TO WS-METRIC-ID
              MOVE WS-TRANS-ERRORS TO WS-METRIC-VALUE
              PERFORM SETUP-ERROR-ATTRIBUTES
              PERFORM SEND-METRIC
           END-IF
           
           * Exportar histograma de duração (somente se houve transação)
           IF WS-DURATION-COUNT > 0
              PERFORM EXPORT-DURATION-HISTOGRAM
           END-IF
           .
       
       SETUP-FILE-READ-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configurar atributos para métricas de leitura de arquivo       *
      *----------------------------------------------------------------*
      
           * Limpar contador de atributos
           MOVE ZEROS TO WS-ATTR-COUNT
           
           * Atributo 1: program_id (01)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 1 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'METRICEX        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 2: business_domain (20)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 20 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'BANKING         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 3: file_name (40)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 40 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'CUSTOMER        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       SETUP-DB-SELECT-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configurar atributos para métricas de operações DB             *
      *----------------------------------------------------------------*
      
           * Limpar contador de atributos
           MOVE ZEROS TO WS-ATTR-COUNT
           
           * Atributo 1: program_id (01)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 1 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'METRICEX        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 2: business_domain (20)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 20 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'BANKING         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 3: table_name (50)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 50 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'ACCOUNT         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       SETUP-TRANSACTION-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configurar atributos para métricas de transação                *
      *----------------------------------------------------------------*
      
           * Limpar contador de atributos
           MOVE ZEROS TO WS-ATTR-COUNT
           
           * Atributo 1: program_id (01)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 1 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'METRICEX        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 2: transaction_id (02)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 2 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'TRNX            ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 3: business_domain (20)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 20 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'BANKING         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Atributo 4: transaction_type (22)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 22 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'ACCOUNT_INQUIRY ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       SETUP-ERROR-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configurar atributos para métricas de erro                     *
      *----------------------------------------------------------------*
      
           * Configurar atributos base igual à transação
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           
           * Atributo adicional: error_code (60)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 60 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE WS-ERROR-CODE TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       EXPORT-DURATION-HISTOGRAM.
      *----------------------------------------------------------------*
      * Exportar histograma de duração                                 *
      *----------------------------------------------------------------*
      
           * ID da métrica para histograma de duração (610)
           MOVE 610 TO WS-METRIC-ID
           
           * Configurar atributos comuns para todos os buckets
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           
           * Bucket 1: 0-100ms
           MOVE WS-BUCKET-0-100-MS TO WS-METRIC-VALUE
           
           * Adicionar atributo de bucket (específico para histograma)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE '0.1             ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Bucket 2: 101-500ms
           MOVE WS-BUCKET-101-500-MS TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE '0.5             ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Bucket 3: 501-1000ms
           MOVE WS-BUCKET-501-1000-MS TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE '1.0             ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Bucket 4: 1001-5000ms
           MOVE WS-BUCKET-1001-5000-MS TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE '5.0             ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Bucket 5: 5001+ms
           MOVE WS-BUCKET-5001-PLUS-MS TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE '+Inf            ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Soma para cálculo da média
           MOVE WS-DURATION-SUM TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'sum             ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Contagem total
           MOVE WS-DURATION-COUNT TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'count           ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           .
       
       SEND-METRIC.
      *----------------------------------------------------------------*
      * Enviar métrica via CICS                                        *
      *----------------------------------------------------------------*
      
           * Criar container para a métrica
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-METRIC)
              FLENGTH(LENGTH OF WS-METRIC)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC
           
           * Verificar se operação foi bem-sucedida
           IF WS-RESP = DFHRESP(NORMAL)
              * Chamar programa coletor de métricas
              EXEC CICS LINK PROGRAM('METCOLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF
           . 