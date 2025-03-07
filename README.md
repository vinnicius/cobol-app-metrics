# COBOL Application Metrics (cobol-app-metrics)

A proposal for implementing metrics in COBOL applications following the semantic conventions of the OpenTelemetry project, enabling modern monitoring of legacy systems.

## Overview

This project presents a proposal for instrumenting metrics in COBOL applications, aiming at integration with modern observability systems such as Prometheus. The implementation allows collecting metrics from COBOL programs through a dedicated CICS transaction, providing valuable insights into the behavior and performance of legacy applications.

## Metrics Catalog

The project defines a comprehensive catalog of metrics organized into functional groups:

### 1. File Operations Metrics (100-199)

#### Counters
- **101**: `cobol.file.records.read_total` - Total records read (per file)
- **102**: `cobol.file.records.written_total` - Total records written (per file)
- **103**: `cobol.file.records.updated_total` - Total records updated (per file)
- **104**: `cobol.file.records.deleted_total` - Total records deleted (per file)
- **105**: `cobol.file.operations.errors_total` - Total errors in file operations (by error type)

#### Histograms
- **110**: `cobol.file.operation.duration_seconds` - Time spent on file operations
- **111**: `cobol.file.record.size_bytes` - Size of processed records

### 2. Database Metrics (200-299)

#### Counters
- **201**: `cobol.db.operations.select_total` - Total SELECT operations
- **202**: `cobol.db.operations.insert_total` - Total INSERT operations
- **203**: `cobol.db.operations.update_total` - Total UPDATE operations
- **204**: `cobol.db.operations.delete_total` - Total DELETE operations
- **205**: `cobol.db.operations.errors_total` - Total errors in database operations (by type)
- **206**: `cobol.db.records.processed_total` - Total records processed in database operations

#### Histograms
- **210**: `cobol.db.operation.duration_seconds` - Execution time of database operations
- **211**: `cobol.db.result.size_records` - Size of query results (in records)

### 3. CICS Transaction Metrics (300-399)

#### Counters
- **301**: `cobol.cics.transaction.started_total` - Total CICS transactions started (by type)
- **302**: `cobol.cics.transaction.completed_total` - Total CICS transactions completed
- **303**: `cobol.cics.transaction.aborted_total` - Total CICS transactions aborted
- **304**: `cobol.cics.commarea.calls_total` - Total calls using COMMAREA
- **305**: `cobol.cics.container.calls_total` - Total calls using containers

#### Histograms
- **310**: `cobol.cics.transaction.duration_seconds` - Execution time of CICS transactions
- **311**: `cobol.cics.commarea.size_bytes` - Size of COMMAREA used

### 4. Data Processing Metrics (400-499)

#### Counters
- **401**: `cobol.processing.records.successful_total` - Total records processed successfully
- **402**: `cobol.processing.records.failed_total` - Total records with processing failure
- **403**: `cobol.processing.batch.completed_total` - Total batches processed

#### Gauges
- **410**: `cobol.processing.records.current` - Current number of records being processed
- **411**: `cobol.processing.batch.progress_percent` - Percentage progress of the current batch

#### Histograms
- **420**: `cobol.processing.record.duration_seconds` - Processing time per record
- **421**: `cobol.processing.batch.size_records` - Size of processed batches (in records)

### 5. Inter-System Communication Metrics (500-599)

#### Counters
- **501**: `cobol.calls.program_total` - Total calls to other COBOL programs
- **502**: `cobol.calls.external_total` - Total calls to external systems
- **503**: `cobol.mq.messages.sent_total` - Total messages sent to MQ queues
- **504**: `cobol.mq.messages.received_total` - Total messages received from MQ queues

#### Histograms
- **510**: `cobol.calls.duration_seconds` - Execution time of external calls
- **511**: `cobol.mq.message.size_bytes` - Size of processed messages

### 6. Business Logic Metrics (600-699)

#### Counters
- **601**: `cobol.business.transaction.started_total` - Total business transactions started
- **602**: `cobol.business.transaction.completed_total` - Total business transactions completed successfully
- **603**: `cobol.business.transaction.failed_total` - Total business transactions failed
- **604**: `cobol.business.transaction.canceled_total` - Total business transactions canceled
- **605**: `cobol.business.transaction.retried_total` - Total business transactions that needed to be retried

#### Histograms
- **610**: `cobol.business.transaction.duration_seconds` - Execution time of business transactions
- **611**: `cobol.business.transaction.size_bytes` - Size of data processed in transactions

## Optimized Contextual Attributes

To enrich metrics and save space, we use a coded system of contextual attributes, where each attribute is represented by a numeric code (1-99) and a fixed-size value (16 characters). The attribute codes are defined in [attribute_codes.md](attribute_codes.md).

### Basic Structure

```
ATTRIBUTE-CODE: ATTRIBUTE-VALUE
```

For example:
- `01: PGMABCD         ` (program_id)
- `20: BANKING         ` (business_domain)
- `40: CUSTFILE        ` (file_name)

### Main Attribute Categories

- **General Attributes (1-19)**: program_id, transaction_id, user_id, environment, etc.
- **Business Attributes (20-39)**: business_domain, business_unit, transaction_type, etc.
- **File Attributes (40-49)**: file_name, file_type, record_type, etc.
- **Database Attributes (50-59)**: table_name, database_name, sql_operation, etc.
- **Error Attributes (60-69)**: error_code, error_type, error_severity, etc.
- **Communication Attributes (70-79)**: program_called, queue_name, message_type, etc.

## Implementation

### Communication via CICS

Metrics can be triggered through a dedicated CICS transaction, using the unique identifier of the metric as a key. For example:

```
KEY: 101
VALUE: 1500
ATTRIBUTES: [01: PGMABCD, 40: CUSTFILE]
```

This would indicate that the metric `cobol.file.records.read_total` has the value 1500, meaning 1500 records were read by the PGMABCD program in the CUSTFILE file.

### COBOL Data Structure

```cobol
* Definition for metrics
01 WS-METRIC.
   05 WS-METRIC-ID                 PIC 9(3).
   05 WS-METRIC-VALUE              PIC 9(9) COMP.
   05 WS-ATTR-COUNT                PIC 9(2).
   05 WS-ATTRIBUTES OCCURS 10 TIMES.
      10 WS-ATTR-CODE              PIC 9(2).
      10 WS-ATTR-VALUE             PIC X(16).
```

### Additional Documentation

For additional implementation details, see:

- [counter_implementation.md](counter_implementation.md): Implementation of counters
- [histogram_implementation.md](histogram_implementation.md): Implementation of histograms
- [attribute_codes.md](attribute_codes.md): Contextual attribute codes

## Benefits

- **Modern observability for legacy systems**: Allows integrating COBOL systems with modern monitoring platforms.
- **Efficient diagnostics**: Facilitates quick identification of problems in critical applications.
- **Capacity planning**: Provides data for informed decisions about resources and infrastructure.
- **Trend analysis**: Enables detection of patterns and anomalies in application behavior.
- **Performance optimization**: Helps identify bottlenecks and opportunities for improvement.

## Integration with Prometheus

The collected metrics can be exported to Prometheus in the format:

```
# HELP cobol_business_transaction_duration_seconds Duration of business transactions
# TYPE cobol_business_transaction_duration_seconds histogram
cobol_business_transaction_duration_seconds_bucket{le="0.1",business_domain="BANKING",transaction_type="PAYMENT"} 10
cobol_business_transaction_duration_seconds_bucket{le="0.5",business_domain="BANKING",transaction_type="PAYMENT"} 25
...
```

## Contributions

Contributions are welcome! Feel free to suggest new metrics, implementation improvements, or additional optimizations.

## Portuguese Version

A Portuguese (Brazil) version of this documentation is available in the [pt-BR](pt-BR/) folder.
