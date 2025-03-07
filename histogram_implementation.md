# Implementation of Histograms in COBOL

This document describes how to implement histograms for metrics in COBOL applications, following the OpenTelemetry conventions.

## Concept

In observability, a histogram is a metric that samples observations (usually things like request durations or response sizes) and counts them in configurable buckets. Histograms are used to:
- Measure the distribution of values (such as response times)
- Calculate percentiles (p50, p90, p99)
- Understand the statistical behavior of a metric over time

## Basic Structure in COBOL

In COBOL, histograms can be implemented as a series of counters for different value ranges (buckets), plus variables to track the total sum and count of observations.

```cobol
       WORKING-STORAGE SECTION.
       * Histogram for response time (ms)
       01 WS-RESPONSE-TIME-HISTOGRAM.
          05 WS-BUCKET-0-10              PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-11-50             PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-51-100            PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500           PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-PLUS          PIC 9(9) COMP VALUE ZEROS.
          05 WS-HIST-SUM                 PIC 9(9) COMP VALUE ZEROS.
          05 WS-HIST-COUNT               PIC 9(9) COMP VALUE ZEROS.
```

## Histograms for Generic Business Metrics

To implement histograms that measure business aspects independent of the industry:

```cobol
       WORKING-STORAGE SECTION.
       * Histogram for business transaction duration
       01 WS-BUSINESS-DURATION-HIST.
          05 WS-BUCKET-0-100-MS         PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500-MS       PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-1000-MS      PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-1001-5000-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-5001-PLUS-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-SUM            PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-COUNT          PIC 9(9) COMP VALUE ZEROS.
          
       * Histogram for business entity size
       01 WS-BUSINESS-SIZE-HIST.
          05 WS-SIZE-0-1KB              PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-1-10KB             PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-10-100KB           PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-100KB-1MB          PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-1MB-PLUS           PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-SUM               PIC 9(9) COMP VALUE ZEROS.
          05 WS-SIZE-COUNT             PIC 9(9) COMP VALUE ZEROS.
          
       * Structure for metrics with contextual attributes
       01 WS-BUSINESS-HIST-METRIC.
          05 WS-HIST-ID                 PIC 9(3).
          05 WS-BUCKET-ID               PIC 9(2).
          05 WS-BUCKET-VALUE            PIC 9(9).
          05 WS-HIST-CONTEXT.
             10 WS-BUSINESS-DOMAIN      PIC X(15).
             10 WS-TRANSACTION-TYPE     PIC X(15).
             10 WS-ENTITY-TYPE          PIC X(15).
             10 WS-PROCESS-NAME         PIC X(20).
             10 WS-PRIORITY-LEVEL       PIC X(6).
```

## Complete Implementation for Business Metrics

Here is a complete example of how to implement histograms for business metrics:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSNHIST.
       AUTHOR. METRICS-TEAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       * Histogram for business transaction duration
       01 WS-BUSINESS-DURATION-HIST.
          05 WS-BUCKET-0-100-MS         PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-101-500-MS       PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-501-1000-MS      PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-1001-5000-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-BUCKET-5001-PLUS-MS     PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-SUM            PIC 9(9) COMP VALUE ZEROS.
          05 WS-DURATION-COUNT          PIC 9(9) COMP VALUE ZEROS.
       
       * Variables for time measurement
       01 WS-START-TIME                PIC 9(18) COMP VALUE ZEROS.
       01 WS-END-TIME                  PIC 9(18) COMP VALUE ZEROS.
       01 WS-DURATION                  PIC 9(9) COMP VALUE ZEROS.
       
       * Structure for metrics with contextual attributes
       01 WS-BUSINESS-HIST-METRIC.
          05 WS-HIST-ID                 PIC 9(3).
          05 WS-BUCKET-ID               PIC 9(2).
          05 WS-BUCKET-VALUE            PIC 9(9).
          05 WS-HIST-CONTEXT.
             10 WS-BUSINESS-DOMAIN      PIC X(15).
             10 WS-TRANSACTION-TYPE     PIC X(15).
             10 WS-ENTITY-TYPE          PIC X(15).
             10 WS-PROCESS-NAME         PIC X(20).
             10 WS-PRIORITY-LEVEL       PIC X(6).
       
       * Variables for CICS control
       01 WS-RESP                       PIC S9(8) COMP.
       01 WS-RESP2                      PIC S9(8) COMP.
       01 WS-CONTAINER-NAME             PIC X(16) VALUE 'HIST-CONTAINER'.
       01 WS-CHANNEL-NAME               PIC X(16) VALUE 'HIST-CHANNEL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           * Start time measurement
           EXEC CICS ASKTIME ABSTIME(WS-START-TIME) END-EXEC
           
           * Simulate business processing
           PERFORM PROCESS-BUSINESS-TRANSACTION
           
           * End time measurement
           EXEC CICS ASKTIME ABSTIME(WS-END-TIME) END-EXEC
           
           * Calculate duration in milliseconds
           COMPUTE WS-DURATION = 
                  (WS-END-TIME - WS-START-TIME) / 1000
           
           * Record duration in histogram
           PERFORM RECORD-DURATION-HISTOGRAM
           
           * Export histograms
           PERFORM EXPORT-BUSINESS-HISTOGRAMS
           
           EXEC CICS RETURN END-EXEC.
       
       PROCESS-BUSINESS-TRANSACTION.
           * Simulation of business processing
           * This is just an example to demonstrate the concept
           EXEC CICS DELAY FOR SECONDS(1) END-EXEC.
       
       RECORD-DURATION-HISTOGRAM.
           ADD WS-DURATION TO WS-DURATION-SUM
           ADD 1 TO WS-DURATION-COUNT
           
           * Increment the appropriate bucket
           EVALUATE TRUE
              WHEN WS-DURATION <= 100
                 ADD 1 TO WS-BUCKET-0-100-MS
              WHEN WS-DURATION <= 500
                 ADD 1 TO WS-BUCKET-101-500-MS
              WHEN WS-DURATION <= 1000
                 ADD 1 TO WS-BUCKET-501-1000-MS
              WHEN WS-DURATION <= 5000
                 ADD 1 TO WS-BUCKET-1001-5000-MS
              WHEN OTHER
                 ADD 1 TO WS-BUCKET-5001-PLUS-MS
           END-EVALUATE.
       
       EXPORT-BUSINESS-HISTOGRAMS.
           * Prepare common context
           MOVE 'BANKING' TO WS-BUSINESS-DOMAIN
           MOVE 'PAYMENT' TO WS-TRANSACTION-TYPE
           MOVE 'ACCOUNT' TO WS-ENTITY-TYPE
           MOVE 'FUNDS-TRANSFER' TO WS-PROCESS-NAME
           MOVE 'HIGH' TO WS-PRIORITY-LEVEL
           
           * Export duration histogram (ID: 610)
           MOVE 610 TO WS-HIST-ID
           
           * Export each bucket
           MOVE 1 TO WS-BUCKET-ID
           MOVE WS-BUCKET-0-100-MS TO WS-BUCKET-VALUE
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 2 TO WS-BUCKET-ID
           MOVE WS-BUCKET-101-500-MS TO WS-BUCKET-VALUE
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 3 TO WS-BUCKET-ID
           MOVE WS-BUCKET-501-1000-MS TO WS-BUCKET-VALUE
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 4 TO WS-BUCKET-ID
           MOVE WS-BUCKET-1001-5000-MS TO WS-BUCKET-VALUE
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 5 TO WS-BUCKET-ID
           MOVE WS-BUCKET-5001-PLUS-MS TO WS-BUCKET-VALUE
           PERFORM SEND-HISTOGRAM-BUCKET
           
           * Export sum and count for average calculation
           MOVE 6 TO WS-BUCKET-ID
           MOVE WS-DURATION-SUM TO WS-BUCKET-VALUE
           PERFORM SEND-HISTOGRAM-BUCKET
           
           MOVE 7 TO WS-BUCKET-ID
           MOVE WS-DURATION-COUNT TO WS-BUCKET-VALUE
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

## Calculating Percentiles

To calculate percentiles from the histogram buckets:

```cobol
       CALCULATE-PERCENTILES.
           * Calculate total observations
           COMPUTE WS-TOTAL-OBSERVATIONS = WS-DURATION-COUNT
           
           * Number of observations for each percentile
           COMPUTE WS-P50-COUNT = WS-TOTAL-OBSERVATIONS * 0.5
           COMPUTE WS-P90-COUNT = WS-TOTAL-OBSERVATIONS * 0.9
           COMPUTE WS-P99-COUNT = WS-TOTAL-OBSERVATIONS * 0.99
           
           * Accumulated counter
           MOVE ZEROS TO WS-ACCUMULATED-COUNT
           
           * P50 (Median)
           ADD WS-BUCKET-0-100-MS TO WS-ACCUMULATED-COUNT
           IF WS-ACCUMULATED-COUNT >= WS-P50-COUNT
              MOVE 100 TO WS-P50-VALUE
           ELSE
              ADD WS-BUCKET-101-500-MS TO WS-ACCUMULATED-COUNT
              IF WS-ACCUMULATED-COUNT >= WS-P50-COUNT
                 MOVE 500 TO WS-P50-VALUE
              ELSE
                 * Continue for other buckets
                 * ...
              END-IF
           END-IF.
```

## Integration with Prometheus

For integration with Prometheus, the output format for business histograms would be:

```
# HELP cobol_business_transaction_duration_seconds Duration of business transactions
# TYPE cobol_business_transaction_duration_seconds histogram
cobol_business_transaction_duration_seconds_bucket{le="0.1",business_domain="BANKING",transaction_type="PAYMENT"} 10
cobol_business_transaction_duration_seconds_bucket{le="0.5",business_domain="BANKING",transaction_type="PAYMENT"} 25
cobol_business_transaction_duration_seconds_bucket{le="1",business_domain="BANKING",transaction_type="PAYMENT"} 35
cobol_business_transaction_duration_seconds_bucket{le="5",business_domain="BANKING",transaction_type="PAYMENT"} 45
cobol_business_transaction_duration_seconds_bucket{le="+Inf",business_domain="BANKING",transaction_type="PAYMENT"} 50
cobol_business_transaction_duration_seconds_sum{business_domain="BANKING",transaction_type="PAYMENT"} 75.3
cobol_business_transaction_duration_seconds_count{business_domain="BANKING",transaction_type="PAYMENT"} 50
```

## Performance Considerations

When implementing histograms for business metrics in COBOL, consider:

1. **Bucket selection**: Define value ranges that make sense for your business domain.

2. **Precision vs. Performance**: Balance the number of buckets with processing overhead.

3. **Efficient storage**: Use PIC 9(9) COMP for integer values and PIC 9(9)V99 COMP-3 for decimal values.

4. **Export frequency**: Consider the impact of exporting large volumes of histogram data.

## Advantages of Histograms for Business Metrics

1. **Detailed analysis**: Allows understanding the distribution of business transaction processing times.

2. **Bottleneck identification**: Helps identify performance issues in specific processes.

3. **SLO/SLA**: Facilitates monitoring of service level agreements based on percentiles.

4. **Process optimization**: Provides insights to improve business process efficiency.

5. **Cross-domain comparison**: Allows comparing performance between different transaction types or business domains. 