# Proposta de Métricas para Aplicações COBOL

Este documento apresenta uma proposta de métricas para instrumentação em aplicações COBOL, seguindo as convenções semânticas do projeto OpenTelemetry. Cada métrica possui um identificador único que pode ser utilizado para acionamento via transação CICS.

## 1. Métricas de Operações de Arquivo (100-199)

### Counters
- **101**: `cobol.file.records.read_total` - Total de registros lidos (por arquivo)
- **102**: `cobol.file.records.written_total` - Total de registros escritos (por arquivo)
- **103**: `cobol.file.records.updated_total` - Total de registros atualizados (por arquivo)
- **104**: `cobol.file.records.deleted_total` - Total de registros excluídos (por arquivo)
- **105**: `cobol.file.operations.errors_total` - Total de erros em operações de arquivo (por tipo de erro)

### Histograms
- **110**: `cobol.file.operation.duration_seconds` - Tempo gasto em operações de arquivo
- **111**: `cobol.file.record.size_bytes` - Tamanho dos registros processados

## 2. Métricas de Banco de Dados (200-299)

### Counters
- **201**: `cobol.db.operations.select_total` - Total de operações SELECT
- **202**: `cobol.db.operations.insert_total` - Total de operações INSERT
- **203**: `cobol.db.operations.update_total` - Total de operações UPDATE
- **204**: `cobol.db.operations.delete_total` - Total de operações DELETE
- **205**: `cobol.db.operations.errors_total` - Total de erros em operações de banco (por tipo)
- **206**: `cobol.db.records.processed_total` - Total de registros processados em operações de banco

### Histograms
- **210**: `cobol.db.operation.duration_seconds` - Tempo de execução de operações de banco de dados
- **211**: `cobol.db.result.size_records` - Tamanho dos resultados de consultas (em registros)

## 3. Métricas de Transações CICS (300-399)

### Counters
- **301**: `cobol.cics.transaction.started_total` - Total de transações CICS iniciadas (por tipo)
- **302**: `cobol.cics.transaction.completed_total` - Total de transações CICS completadas
- **303**: `cobol.cics.transaction.aborted_total` - Total de transações CICS abortadas
- **304**: `cobol.cics.commarea.calls_total` - Total de chamadas usando COMMAREA
- **305**: `cobol.cics.container.calls_total` - Total de chamadas usando containers

### Histograms
- **310**: `cobol.cics.transaction.duration_seconds` - Tempo de execução de transações CICS
- **311**: `cobol.cics.commarea.size_bytes` - Tamanho de COMMAREA utilizada

## 4. Métricas de Processamento de Dados (400-499)

### Counters
- **401**: `cobol.processing.records.successful_total` - Total de registros processados com sucesso
- **402**: `cobol.processing.records.failed_total` - Total de registros com falha no processamento
- **403**: `cobol.processing.batch.completed_total` - Total de lotes (batches) processados

### Gauges
- **410**: `cobol.processing.records.current` - Número atual de registros em processamento
- **411**: `cobol.processing.batch.progress_percent` - Progresso percentual do lote atual

### Histograms
- **420**: `cobol.processing.record.duration_seconds` - Tempo de processamento por registro
- **421**: `cobol.processing.batch.size_records` - Tamanho dos lotes processados (em registros)

## 5. Métricas de Comunicação Entre Sistemas (500-599)

### Counters
- **501**: `cobol.calls.program_total` - Total de chamadas a outros programas COBOL
- **502**: `cobol.calls.external_total` - Total de chamadas a sistemas externos
- **503**: `cobol.mq.messages.sent_total` - Total de mensagens enviadas para filas MQ
- **504**: `cobol.mq.messages.received_total` - Total de mensagens recebidas de filas MQ

### Histograms
- **510**: `cobol.calls.duration_seconds` - Tempo de execução de chamadas externas
- **511**: `cobol.mq.message.size_bytes` - Tamanho das mensagens processadas

## 6. Métricas de Lógica de Negócios (600-699)

### Counters
- **601**: `cobol.business.validations.success_total` - Total de validações de negócio bem-sucedidas
- **602**: `cobol.business.validations.failure_total` - Total de validações de negócio com falha
- **603**: `cobol.business.operations.type_total` - Total de operações de negócio por tipo
- **604**: `cobol.business.exceptions.total` - Total de exceções de negócio (por tipo)

### Histograms
- **610**: `cobol.business.rule.execution_seconds` - Tempo de execução de regras de negócio específicas

## 7. Atributos Contextuais (Labels/Tags)

Para enriquecer todas as métricas acima, sugere-se incluir os seguintes atributos contextuais:

- `program_id`: Identificador do programa COBOL
- `transaction_id`: Identificador da transação (quando aplicável)
- `user_id`: Identificador do usuário (quando aplicável)
- `file_name`: Nome do arquivo sendo processado
- `table_name`: Nome da tabela de banco de dados
- `operation_type`: Tipo de operação
- `error_code`: Código de erro (quando aplicável)
- `business_unit`: Unidade de negócio responsável
- `environment`: Ambiente (DEV, TEST, PROD)

## Implementação

Para acionar uma métrica via transação CICS, poderia ser utilizado o identificador único como chave. Por exemplo:

```
CHAVE: 101
VALOR: 1500
```

Isso indicaria que a métrica `cobol.file.records.read_total` possui o valor 1500, ou seja, 1500 registros foram lidos. 