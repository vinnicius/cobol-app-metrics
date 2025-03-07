# Contextual Attribute Identifiers for COBOL Metrics

This document defines the unique identifiers for contextual attributes used in COBOL application metrics. Each attribute has a numeric code to optimize communication between systems.

## Standard Attribute Structure

To save memory space and standardize communication, all attributes follow the format:

```
ATTRIBUTE-CODE: ATTRIBUTE-VALUE
```

Where:
- `ATTRIBUTE-CODE`: Integer between 1-99 that identifies the attribute type
- `ATTRIBUTE-VALUE`: Alphanumeric string with a fixed size of 16 characters (right-padded with spaces when necessary)

## Attribute Codes

### General Attributes (1-19)

- **01**: `program_id` - COBOL program identifier
- **02**: `transaction_id` - CICS transaction identifier
- **03**: `user_id` - User identifier
- **04**: `environment` - Environment (DEV, TEST, PROD)
- **05**: `timestamp` - Collection timestamp (YYYYMMDDHHMMSS format)
- **06**: `system_id` - System identifier
- **07**: `subsystem_id` - Subsystem identifier
- **08**: `region_id` - Region identifier
- **09**: `job_id` - Job identifier

### Business Domain Attributes (20-39)

- **20**: `business_domain` - Business domain (e.g., BANKING, INSURANCE)
- **21**: `business_unit` - Business unit
- **22**: `transaction_type` - Business transaction type
- **23**: `entity_type` - Business entity type
- **24**: `process_name` - Business process name
- **25**: `priority_level` - Priority level
- **26**: `channel` - Source channel
- **27**: `customer_segment` - Customer segment
- **28**: `product_id` - Product identifier
- **29**: `service_id` - Service identifier

### File Operation Attributes (40-49)

- **40**: `file_name` - File name
- **41**: `file_type` - File type
- **42**: `record_type` - Record type
- **43**: `operation_type` - File operation type
- **44**: `access_method` - Access method

### Database Attributes (50-59)

- **50**: `table_name` - Table name
- **51**: `database_name` - Database name
- **52**: `sql_operation` - SQL operation type
- **53**: `cursor_name` - Cursor name
- **54**: `schema_name` - Schema name

### Error Attributes (60-69)

- **60**: `error_code` - Error code
- **61**: `error_type` - Error type
- **62**: `error_severity` - Error severity
- **63**: `error_source` - Error source
- **64**: `error_message` - Summarized error message
- **65**: `retry_count` - Retry counter

### Communication Attributes (70-79)

- **70**: `program_called` - Called program
- **71**: `queue_name` - Queue name
- **72**: `message_type` - Message type
- **73**: `protocol` - Communication protocol
- **74**: `endpoint` - Communication endpoint

## COBOL Data Structure for Attributes

```cobol
       * Attribute definition with code and value
       01 WS-ATTRIBUTE.
          05 WS-ATTR-CODE              PIC 9(2).
          05 WS-ATTR-VALUE             PIC X(16).
          
       * Array of attributes for a metric (up to 10 attributes)
       01 WS-METRIC-ATTRIBUTES.
          05 WS-ATTR-COUNT             PIC 9(2).
          05 WS-ATTRIBUTES OCCURS 10 TIMES.
             10 WS-ATTR-CODE           PIC 9(2).
             10 WS-ATTR-VALUE          PIC X(16).
```

## Usage Example

To record a metric with contextual attributes:

```cobol
       * Prepare attributes for the metric
       MOVE 5 TO WS-ATTR-COUNT.
       
       * Attribute 1: program_id
       MOVE 1 TO WS-ATTRIBUTES(1).
       MOVE 'PGMABCD         ' TO WS-ATTR-VALUE(1).
       
       * Attribute 2: transaction_id
       MOVE 2 TO WS-ATTRIBUTES(2).
       MOVE 'TRX1            ' TO WS-ATTR-VALUE(2).
       
       * Attribute 3: environment
       MOVE 4 TO WS-ATTRIBUTES(3).
       MOVE 'PROD            ' TO WS-ATTR-VALUE(3).
       
       * Attribute 4: file_name
       MOVE 40 TO WS-ATTRIBUTES(4).
       MOVE 'CUSTFILE        ' TO WS-ATTR-VALUE(4).
       
       * Attribute 5: business_domain
       MOVE 20 TO WS-ATTRIBUTES(5).
       MOVE 'BANKING         ' TO WS-ATTR-VALUE(5).
```

This optimized format significantly reduces the space needed for transmitting metrics between systems, while maintaining the ability to properly contextualize the collected metrics. 