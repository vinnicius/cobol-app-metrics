       IDENTIFICATION DIVISION.
       PROGRAM-ID. METRICEX.
       AUTHOR. COBOL-METRICS-TEAM.
      
      *----------------------------------------------------------------*
      * Example of a COBOL program with metrics instrumentation        *
      * using optimized attribute identifiers                          *
      *----------------------------------------------------------------*
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *----------------------------------------------------------------*
      * Definition of structure for metrics with attributes            *
      *----------------------------------------------------------------*
       01 WS-METRIC.
          05 WS-METRIC-ID                PIC 9(3).
          05 WS-METRIC-VALUE             PIC 9(9) COMP.
          05 WS-ATTR-COUNT               PIC 9(2) VALUE ZEROS.
          05 WS-ATTRIBUTES OCCURS 10 TIMES.
             10 WS-ATTR-CODE             PIC 9(2).
             10 WS-ATTR-VALUE            PIC X(16).
       
      *----------------------------------------------------------------*
      * Counters for instrumentation                                   *
      *----------------------------------------------------------------*
       01 WS-COUNTERS.
          05 WS-FILE-READS               PIC 9(9) COMP VALUE ZEROS.
          05 WS-DB-SELECTS               PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-STARTED            PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-COMPLETED          PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-ERRORS             PIC 9(9) COMP VALUE ZEROS.
       
      *----------------------------------------------------------------*
      * Variables for time measurement (histograms)                    *
      *----------------------------------------------------------------*
       01 WS-TIMERS.
          05 WS-TRANS-START-TIME         PIC 9(18) COMP VALUE ZEROS.
          05 WS-TRANS-END-TIME           PIC 9(18) COMP VALUE ZEROS.
          05 WS-TRANS-DURATION-MS        PIC 9(9) COMP VALUE ZEROS.
       
      *----------------------------------------------------------------*
      * Histogram for transaction duration                             *
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
      * Variables for CICS control                                     *
      *----------------------------------------------------------------*
       01 WS-CICS-CONTROL.
          05 WS-RESP                     PIC S9(8) COMP.
          05 WS-RESP2                    PIC S9(8) COMP.
          05 WS-CONTAINER-NAME           PIC X(16) VALUE 'METRIC-CONTAIN'.
          05 WS-CHANNEL-NAME             PIC X(16) VALUE 'METRIC-CHANNEL'.
       
      *----------------------------------------------------------------*
      * Variables for processing simulation                            *
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
      * Start main processing                                          *
      *----------------------------------------------------------------*
      
           * Start timer for transaction measurement
           EXEC CICS ASKTIME ABSTIME(WS-TRANS-START-TIME)
           END-EXEC
           
           * Increment counter for started transactions
           ADD 1 TO WS-TRANS-STARTED
           
           * Execute simulated processing
           PERFORM PROCESS-BUSINESS-TRANSACTION
           
           * End timer and calculate duration
           EXEC CICS ASKTIME ABSTIME(WS-TRANS-END-TIME)
           END-EXEC
           
           COMPUTE WS-TRANS-DURATION-MS = 
              (WS-TRANS-END-TIME - WS-TRANS-START-TIME) / 1000
           
           * Record duration in histogram
           PERFORM RECORD-TRANSACTION-DURATION
           
           * Check result and record counters
           IF WS-ERROR-OCCURRED
              ADD 1 TO WS-TRANS-ERRORS
           ELSE
              ADD 1 TO WS-TRANS-COMPLETED
           END-IF
           
           * Export collected metrics
           PERFORM EXPORT-METRICS
           
           EXEC CICS RETURN
           END-EXEC
           .
       
       PROCESS-BUSINESS-TRANSACTION.
      *----------------------------------------------------------------*
      * Simulation of business processing                              *
      *----------------------------------------------------------------*
      
           * Simulation: Read customer file
           MOVE 'C1234567' TO WS-CUSTOMER-ID
           ADD 1 TO WS-FILE-READS
           
           * Simulation: Query database
           MOVE '000123456789' TO WS-ACCOUNT-NUMBER
           ADD 1 TO WS-DB-SELECTS
           
           * Simulation of processing (with error possibility)
           MOVE 1250.75 TO WS-TRANSACTION-AMOUNT
           
           * Simulate error in 10% of cases
           IF FUNCTION RANDOM < 0.1
              MOVE 'Y' TO WS-ERROR-FLAG
              MOVE 'E123' TO WS-ERROR-CODE
           ELSE
              MOVE 'N' TO WS-ERROR-FLAG
           END-IF
           .
       
       RECORD-TRANSACTION-DURATION.
      *----------------------------------------------------------------*
      * Record the transaction duration in the appropriate histogram   *
      *----------------------------------------------------------------*
      
           * Add value to sum and increment count
           ADD WS-TRANS-DURATION-MS TO WS-DURATION-SUM
           ADD 1 TO WS-DURATION-COUNT
           
           * Record in the appropriate bucket
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
      * Export collected metrics using optimized attributes            *
      *----------------------------------------------------------------*
      
           * Export file read counter (101)
           MOVE 101 TO WS-METRIC-ID
           MOVE WS-FILE-READS TO WS-METRIC-VALUE
           PERFORM SETUP-FILE-READ-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Export DB SELECT counter (201)
           MOVE 201 TO WS-METRIC-ID
           MOVE WS-DB-SELECTS TO WS-METRIC-VALUE
           PERFORM SETUP-DB-SELECT-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Export started transactions counter (601)
           MOVE 601 TO WS-METRIC-ID
           MOVE WS-TRANS-STARTED TO WS-METRIC-VALUE
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Export completed transactions counter (602)
           MOVE 602 TO WS-METRIC-ID
           MOVE WS-TRANS-COMPLETED TO WS-METRIC-VALUE
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           PERFORM SEND-METRIC
           
           * Export error counter (if any)
           IF WS-TRANS-ERRORS > 0
              MOVE 603 TO WS-METRIC-ID
              MOVE WS-TRANS-ERRORS TO WS-METRIC-VALUE
              PERFORM SETUP-ERROR-ATTRIBUTES
              PERFORM SEND-METRIC
           END-IF
           
           * Export duration histogram (only if transactions occurred)
           IF WS-DURATION-COUNT > 0
              PERFORM EXPORT-DURATION-HISTOGRAM
           END-IF
           .
       
       SETUP-FILE-READ-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configure attributes for file read metrics                     *
      *----------------------------------------------------------------*
      
           * Clear attribute counter
           MOVE ZEROS TO WS-ATTR-COUNT
           
           * Attribute 1: program_id (01)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 1 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'METRICEX        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 2: business_domain (20)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 20 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'BANKING         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 3: file_name (40)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 40 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'CUSTOMER        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       SETUP-DB-SELECT-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configure attributes for DB operation metrics                  *
      *----------------------------------------------------------------*
      
           * Clear attribute counter
           MOVE ZEROS TO WS-ATTR-COUNT
           
           * Attribute 1: program_id (01)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 1 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'METRICEX        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 2: business_domain (20)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 20 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'BANKING         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 3: table_name (50)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 50 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'ACCOUNT         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       SETUP-TRANSACTION-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configure attributes for transaction metrics                   *
      *----------------------------------------------------------------*
      
           * Clear attribute counter
           MOVE ZEROS TO WS-ATTR-COUNT
           
           * Attribute 1: program_id (01)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 1 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'METRICEX        ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 2: transaction_id (02)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 2 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'TRNX            ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 3: business_domain (20)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 20 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'BANKING         ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           
           * Attribute 4: transaction_type (22)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 22 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'ACCOUNT_INQUIRY ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       SETUP-ERROR-ATTRIBUTES.
      *----------------------------------------------------------------*
      * Configure attributes for error metrics                         *
      *----------------------------------------------------------------*
      
           * Configure base attributes same as transaction
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           
           * Additional attribute: error_code (60)
           ADD 1 TO WS-ATTR-COUNT
           MOVE 60 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE WS-ERROR-CODE TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           .
       
       EXPORT-DURATION-HISTOGRAM.
      *----------------------------------------------------------------*
      * Export duration histogram                                      *
      *----------------------------------------------------------------*
      
           * Metric ID for duration histogram (610)
           MOVE 610 TO WS-METRIC-ID
           
           * Configure common attributes for all buckets
           PERFORM SETUP-TRANSACTION-ATTRIBUTES
           
           * Bucket 1: 0-100ms
           MOVE WS-BUCKET-0-100-MS TO WS-METRIC-VALUE
           
           * Add bucket attribute (specific for histogram)
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
           
           * Sum for average calculation
           MOVE WS-DURATION-SUM TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'sum             ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           
           * Total count
           MOVE WS-DURATION-COUNT TO WS-METRIC-VALUE
           MOVE 99 TO WS-ATTR-CODE(WS-ATTR-COUNT)
           MOVE 'count           ' TO WS-ATTR-VALUE(WS-ATTR-COUNT)
           PERFORM SEND-METRIC
           .
       
       SEND-METRIC.
      *----------------------------------------------------------------*
      * Send metric via CICS                                           *
      *----------------------------------------------------------------*
      
           * Create container for the metric
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-METRIC)
              FLENGTH(LENGTH OF WS-METRIC)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC
           
           * Check if operation was successful
           IF WS-RESP = DFHRESP(NORMAL)
              * Call metric collector program
              EXEC CICS LINK PROGRAM('METCOLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF
           . 