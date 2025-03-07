# Implementation of Counters in COBOL

This document describes how to implement counters for metrics in COBOL applications, following the OpenTelemetry conventions.

## Concept

In observability, a counter is a cumulative metric that represents a single value that can only increase or be reset to zero. Counters are used to measure discrete events over time, such as:
- Number of transactions processed
- Number of errors encountered
- Total records read or written

## Basic Structure in COBOL

In COBOL, counters can be implemented as numeric variables that are incremented when a specific event occurs.

```cobol
       WORKING-STORAGE SECTION.
       01 WS-COUNTERS.
          05 WS-COUNTER-READS          PIC 9(9) COMP VALUE ZEROS.
          05 WS-COUNTER-WRITES         PIC 9(9) COMP VALUE ZEROS.
          05 WS-COUNTER-ERRORS         PIC 9(9) COMP VALUE ZEROS.
```

## Counters for Generic Business Metrics

To implement counters for business metrics that are applicable to any domain, we can use the following structure:

```cobol
       WORKING-STORAGE SECTION.
       * Generic business counters
       01 WS-BUSINESS-COUNTERS.
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
          
       * Structure for metrics with contextual attributes
       01 WS-BUSINESS-METRIC.
          05 WS-METRIC-ID             PIC 9(3).
          05 WS-METRIC-VALUE          PIC 9(9).
          05 WS-METRIC-CONTEXT.
             10 WS-BUSINESS-DOMAIN     PIC X(15).
             10 WS-TRANSACTION-TYPE    PIC X(15).
             10 WS-ENTITY-TYPE         PIC X(15).
             10 WS-PROCESS-NAME        PIC X(20).
             10 WS-PRIORITY-LEVEL      PIC X(6).
```

These structures allow capturing business events in any domain, while contextual attributes provide the sector-specific differentiation.

## Complete Implementation

Below is a complete example of how to implement counters in a COBOL program:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTER-EXAMPLE.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Counter definitions
       01 WS-COUNTERS.
          05 WS-COUNTER-READS          PIC 9(9) COMP VALUE ZEROS.
          05 WS-COUNTER-WRITES         PIC 9(9) COMP VALUE ZEROS.
          05 WS-COUNTER-ERRORS         PIC 9(9) COMP VALUE ZEROS.
          05 WS-COUNTER-VALIDATIONS    PIC 9(9) COMP VALUE ZEROS.
          05 WS-COUNTER-TRANSACTIONS   PIC 9(9) COMP VALUE ZEROS.
       
       * Definition of structure to export metrics
       01 WS-METRIC.
          05 WS-METRIC-ID               PIC 9(3).
          05 WS-METRIC-VALUE            PIC 9(9).
          05 WS-METRIC-ATTRIBUTES.
             10 WS-PROGRAM-ID            PIC X(8).
             10 WS-TRANSACTION-ID        PIC X(4).
             10 WS-FILE-NAME             PIC X(8).
       
       * Variables for CICS error control
       01 WS-RESP                        PIC S9(8) COMP.
       01 WS-RESP2                       PIC S9(8) COMP.
       
       * Container and channel name for CICS
       01 WS-CONTAINER-NAME              PIC X(16) VALUE 'METRIC-CONTAINER'.
       01 WS-CHANNEL-NAME                PIC X(16) VALUE 'METRIC-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Start main processing
           PERFORM PROCESS-BUSINESS-LOGIC.
           
           * Export collected metrics
           PERFORM EXPORT-METRICS.
           
           EXEC CICS RETURN END-EXEC.
           
       PROCESS-BUSINESS-LOGIC.
           * Simulate business operations that increment counters
           
           * Simulation: Read record
           ADD 1 TO WS-COUNTER-READS.
           
           * Simulation: Processing with validation
           ADD 1 TO WS-COUNTER-VALIDATIONS.
           
           * Simulation: Write record (sometimes)
           IF WS-COUNTER-READS > 0
              ADD 1 TO WS-COUNTER-WRITES
           END-IF.
           
           * Simulation: Error occurrence (occasional)
           IF WS-COUNTER-READS > 5
              ADD 1 TO WS-COUNTER-ERRORS
           END-IF.
           
           * Increment transaction counter
           ADD 1 TO WS-COUNTER-TRANSACTIONS.
       
       EXPORT-METRICS.
           * Export each counter as a separate metric
           
           * Read counter (ID: 101)
           MOVE 101 TO WS-METRIC-ID.
           MOVE WS-COUNTER-READS TO WS-METRIC-VALUE.
           MOVE 'CNTREXMP' TO WS-PROGRAM-ID.
           MOVE 'METR' TO WS-TRANSACTION-ID.
           MOVE 'DATAFILE' TO WS-FILE-NAME.
           PERFORM SEND-METRIC.
           
           * Write counter (ID: 102)
           MOVE 102 TO WS-METRIC-ID.
           MOVE WS-COUNTER-WRITES TO WS-METRIC-VALUE.
           PERFORM SEND-METRIC.
           
           * Error counter (ID: 105)
           MOVE 105 TO WS-METRIC-ID.
           MOVE WS-COUNTER-ERRORS TO WS-METRIC-VALUE.
           PERFORM SEND-METRIC.
           
           * Validation counter (ID: 601)
           MOVE 601 TO WS-METRIC-ID.
           MOVE WS-COUNTER-VALIDATIONS TO WS-METRIC-VALUE.
           PERFORM SEND-METRIC.
           
           * Transaction counter (ID: 401)
           MOVE 401 TO WS-METRIC-ID.
           MOVE WS-COUNTER-TRANSACTIONS TO WS-METRIC-VALUE.
           PERFORM SEND-METRIC.
           
       SEND-METRIC.
           * Create CICS container for the metric
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-METRIC)
              FLENGTH(LENGTH OF WS-METRIC)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
           
           * Check if the operation was successful
           IF WS-RESP = DFHRESP(NORMAL)
              * Call metric collector program
              EXEC CICS LINK PROGRAM('METCOLLECT')
                 CHANNEL(WS-CHANNEL-NAME)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
```

## Implementation for Generic Business Metrics

Here is a specific example for implementing generic business counters:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSNMETRIC.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Business transaction counters
       01 WS-BUSINESS-COUNTERS.
          05 WS-TRANS-STARTED          PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-COMPLETED        PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-FAILED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-TRANS-CANCELED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-PERFORMED        PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-PASSED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-VALID-FAILED           PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-CREATED         PIC 9(9) COMP VALUE ZEROS.
          05 WS-ENTITY-UPDATED         PIC 9(9) COMP VALUE ZEROS.
       
       * Structure for metrics with contextual attributes
       01 WS-BUSINESS-METRIC.
          05 WS-METRIC-ID             PIC 9(3).
          05 WS-METRIC-VALUE          PIC 9(9).
          05 WS-METRIC-CONTEXT.
             10 WS-BUSINESS-DOMAIN     PIC X(15).
             10 WS-TRANSACTION-TYPE    PIC X(15).
             10 WS-ENTITY-TYPE         PIC X(15).
             10 WS-PROCESS-NAME        PIC X(20).
             10 WS-PRIORITY-LEVEL      PIC X(6).
       
       * Variables for CICS control
       01 WS-RESP                      PIC S9(8) COMP.
       01 WS-RESP2                     PIC S9(8) COMP.
       01 WS-CONTAINER-NAME            PIC X(16) VALUE 'BSN-CONTAINER'.
       01 WS-CHANNEL-NAME              PIC X(16) VALUE 'BSN-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Start business transaction processing
           PERFORM PROCESS-BUSINESS-TRANSACTION.
           
           * Export collected metrics
           PERFORM EXPORT-BUSINESS-METRICS.
           
           EXEC CICS RETURN END-EXEC.
       
       PROCESS-BUSINESS-TRANSACTION.
           * Increment counter for started transactions
           ADD 1 TO WS-TRANS-STARTED
           
           * Simulation: Perform business validations
           PERFORM VALIDATE-BUSINESS-RULES
           
           * Simulation: Process entity (creation or update)
           PERFORM PROCESS-BUSINESS-ENTITY
           
           * Increment counter for completed transactions
           ADD 1 TO WS-TRANS-COMPLETED.
       
       VALIDATE-BUSINESS-RULES.
           * Increment counter for performed validations
           ADD 1 TO WS-VALID-PERFORMED
           
           * Simulation: Successful validation (95% of the time)
           IF FUNCTION RANDOM() > 0.05
              ADD 1 TO WS-VALID-PASSED
           ELSE
              ADD 1 TO WS-VALID-FAILED
           END-IF.
       
       PROCESS-BUSINESS-ENTITY.
           * Simulation: Create new entity (30% of the time)
           IF FUNCTION RANDOM() < 0.3
              ADD 1 TO WS-ENTITY-CREATED
           ELSE
              ADD 1 TO WS-ENTITY-UPDATED
           END-IF.
       
       EXPORT-BUSINESS-METRICS.
           * Prepare common context for all metrics
           MOVE 'BANKING' TO WS-BUSINESS-DOMAIN
           MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
           MOVE 'ACCOUNT' TO WS-ENTITY-TYPE
           MOVE 'FUNDS-TRANSFER' TO WS-PROCESS-NAME
           MOVE 'HIGH' TO WS-PRIORITY-LEVEL
           
           * Export counter metrics
           MOVE 601 TO WS-METRIC-ID
           MOVE WS-TRANS-STARTED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 602 TO WS-METRIC-ID
           MOVE WS-TRANS-COMPLETED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 651 TO WS-METRIC-ID
           MOVE WS-VALID-PERFORMED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 652 TO WS-METRIC-ID
           MOVE WS-VALID-PASSED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 653 TO WS-METRIC-ID
           MOVE WS-VALID-FAILED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 671 TO WS-METRIC-ID
           MOVE WS-ENTITY-CREATED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC
           
           MOVE 672 TO WS-METRIC-ID
           MOVE WS-ENTITY-UPDATED TO WS-METRIC-VALUE
           PERFORM SEND-BUSINESS-METRIC.
       
       SEND-BUSINESS-METRIC.
           EXEC CICS CREATE CONTAINER(WS-CONTAINER-NAME)
              CHANNEL(WS-CHANNEL-NAME)
              FROM(WS-BUSINESS-METRIC)
              FLENGTH(LENGTH OF WS-BUSINESS-METRIC)
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

## Alternative Implementation with TSQ (Temporary Storage Queue)

Another common approach is to use CICS TSQs to store metrics:

```cobol
       EXPORT-METRIC-TSQ.
           * Prepare metric data
           MOVE 101 TO WS-METRIC-ID.
           MOVE WS-COUNTER-READS TO WS-METRIC-VALUE.
           
           * Write metric to a TSQ for later collection
           EXEC CICS WRITEQ TS
              QUEUE('METRICSQ')
              FROM(WS-METRIC)
              LENGTH(LENGTH OF WS-METRIC)
              ITEM(1)
              REWRITE
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
```

## Implementation with Shared Memory (COMMAREA in CICS)

For metrics that need to be shared between transactions:

```cobol
       * In an initialization or control program
       INITIALIZE-SHARED-COUNTERS.
           EXEC CICS GET CONTAINER('SHARED-COUNTERS')
              CHANNEL('GLOBAL-METRICS')
              INTO(WS-COUNTERS)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
           
           IF WS-RESP NOT = DFHRESP(NORMAL)
              INITIALIZE WS-COUNTERS
              EXEC CICS PUT CONTAINER('SHARED-COUNTERS')
                 CHANNEL('GLOBAL-METRICS')
                 FROM(WS-COUNTERS)
                 RESP(WS-RESP)
                 RESP2(WS-RESP2)
              END-EXEC
           END-IF.
           
       * In any program that needs to update the counters
       UPDATE-SHARED-COUNTERS.
           EXEC CICS GET CONTAINER('SHARED-COUNTERS')
              CHANNEL('GLOBAL-METRICS')
              INTO(WS-COUNTERS)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
           
           ADD 1 TO WS-COUNTER-READS.
           
           EXEC CICS PUT CONTAINER('SHARED-COUNTERS')
              CHANNEL('GLOBAL-METRICS')
              FROM(WS-COUNTERS)
              RESP(WS-RESP)
              RESP2(WS-RESP2)
           END-EXEC.
```

## Performance Considerations

When implementing counters in COBOL, it's important to consider:

1. **Use of COMP (COMPUTATIONAL) instructions**: Store counters as PIC 9(9) COMP for better performance.

2. **Update frequency**: In high-volume loops, consider incrementing counters in batches (for example, every 100 processes) to reduce overhead.

3. **Atomicity**: In multi-threaded environments or shared systems, ensure atomicity of increment operations.

4. **Size limits**: Make sure your counters are large enough to not overflow during the expected lifecycle.

## Integration with Prometheus

For integration with Prometheus, the output format for business metrics would be something like:

```
# HELP cobol_business_transaction_started_total Total business transactions started
# TYPE cobol_business_transaction_started_total counter
cobol_business_transaction_started_total{business_domain="BANKING",transaction_type="PAYMENT",entity_type="ACCOUNT",process_name="FUNDS-TRANSFER",priority="HIGH"} 1500
```

The COBOL program can generate this format in a file or make it available through an API for a Prometheus exporter to collect it.

## Advantages of Generic Business Metrics

The approach of generic business metrics with contextual attributes offers several advantages:

1. **Code reusability**: The same instrumentation code can be reused in different COBOL programs, regardless of the business domain.

2. **Analytical flexibility**: Attributes allow data analysts to filter and group metrics according to different business dimensions.

3. **End-to-end traceability**: By maintaining consistency in attributes across different services, it's possible to correlate metrics between different systems.

4. **Independent evolution**: It's possible to add new contextual attributes without changing the basic structure of metrics.

5. **Simplified integration**: Modern observability systems like Prometheus can easily consume this standardized format. 