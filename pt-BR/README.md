# COBOL Application Metrics (cobol-app-metrics)

Proposta de implementação de métricas para aplicações COBOL seguindo as convenções semânticas do projeto OpenTelemetry, permitindo monitoramento moderno de sistemas legados.

## Visão Geral

Este projeto apresenta uma proposta para instrumentação de métricas em aplicações COBOL, visando integração com sistemas modernos de observabilidade como Prometheus. A implementação permite coletar métricas a partir de programas COBOL através de uma transação CICS dedicada, fornecendo insights valiosos sobre o comportamento e performance das aplicações legadas.

## Catálogo de Métricas

O projeto define um catálogo abrangente de métricas organizadas em grupos funcionais:

### 1. Métricas de Operações de Arquivo (100-199)

#### Counters
- **101**: `cobol.file.records.read_total` - Total de registros lidos (por arquivo)
- **102**: `cobol.file.records.written_total` - Total de registros escritos (por arquivo)
- **103**: `cobol.file.records.updated_total` - Total de registros atualizados (por arquivo)
- **104**: `cobol.file.records.deleted_total` - Total de registros excluídos (por arquivo)
- **105**: `cobol.file.operations.errors_total` - Total de erros em operações de arquivo (por tipo de erro)

#### Histograms
- **110**: `cobol.file.operation.duration_seconds` - Tempo gasto em operações de arquivo
- **111**: `cobol.file.record.size_bytes` - Tamanho dos registros processados

### 2. Métricas de Banco de Dados (200-299)

#### Counters
- **201**: `cobol.db.operations.select_total` - Total de operações SELECT
- **202**: `cobol.db.operations.insert_total` - Total de operações INSERT
- **203**: `cobol.db.operations.update_total` - Total de operações UPDATE
- **204**: `cobol.db.operations.delete_total` - Total de operações DELETE
- **205**: `cobol.db.operations.errors_total` - Total de erros em operações de banco (por tipo)
- **206**: `cobol.db.records.processed_total` - Total de registros processados em operações de banco

#### Histograms
- **210**: `cobol.db.operation.duration_seconds` - Tempo de execução de operações de banco de dados
- **211**: `cobol.db.result.size_records` - Tamanho dos resultados de consultas (em registros)

### 3. Métricas de Transações CICS (300-399)

#### Counters
- **301**: `cobol.cics.transaction.started_total` - Total de transações CICS iniciadas (por tipo)
- **302**: `cobol.cics.transaction.completed_total` - Total de transações CICS completadas
- **303**: `cobol.cics.transaction.aborted_total` - Total de transações CICS abortadas
- **304**: `cobol.cics.commarea.calls_total` - Total de chamadas usando COMMAREA
- **305**: `cobol.cics.container.calls_total` - Total de chamadas usando containers

#### Histograms
- **310**: `cobol.cics.transaction.duration_seconds` - Tempo de execução de transações CICS
- **311**: `cobol.cics.commarea.size_bytes` - Tamanho de COMMAREA utilizada

### 4. Métricas de Processamento de Dados (400-499)

#### Counters
- **401**: `cobol.processing.records.successful_total` - Total de registros processados com sucesso
- **402**: `cobol.processing.records.failed_total` - Total de registros com falha no processamento
- **403**: `cobol.processing.batch.completed_total` - Total de lotes (batches) processados

#### Gauges
- **410**: `cobol.processing.records.current` - Número atual de registros em processamento
- **411**: `cobol.processing.batch.progress_percent` - Progresso percentual do lote atual

#### Histograms
- **420**: `cobol.processing.record.duration_seconds` - Tempo de processamento por registro
- **421**: `cobol.processing.batch.size_records` - Tamanho dos lotes processados (em registros)

### 5. Métricas de Comunicação Entre Sistemas (500-599)

#### Counters
- **501**: `cobol.calls.program_total` - Total de chamadas a outros programas COBOL
- **502**: `cobol.calls.external_total` - Total de chamadas a sistemas externos
- **503**: `cobol.mq.messages.sent_total` - Total de mensagens enviadas para filas MQ
- **504**: `cobol.mq.messages.received_total` - Total de mensagens recebidas de filas MQ

#### Histograms
- **510**: `cobol.calls.duration_seconds` - Tempo de execução de chamadas externas
- **511**: `cobol.mq.message.size_bytes` - Tamanho das mensagens processadas

### 6. Métricas de Lógica de Negócios (600-699)

#### Counters
- **601**: `cobol.business.transaction.started_total` - Total de transações de negócio iniciadas
- **602**: `cobol.business.transaction.completed_total` - Total de transações de negócio completadas com sucesso
- **603**: `cobol.business.transaction.failed_total` - Total de transações de negócio com falha
- **604**: `cobol.business.transaction.canceled_total` - Total de transações de negócio canceladas
- **605**: `cobol.business.transaction.retried_total` - Total de transações de negócio que precisaram ser repetidas

#### Histograms
- **610**: `cobol.business.transaction.duration_seconds` - Tempo de execução de transações de negócio
- **611**: `cobol.business.transaction.size_bytes` - Tamanho de dados processados em transações

## Atributos Contextuais Otimizados

Para enriquecer as métricas e economizar espaço, utilizamos um sistema codificado de atributos contextuais, onde cada atributo é representado por um código numérico (1-99) e um valor de tamanho fixo (16 caracteres). Os códigos dos atributos são definidos em [attribute_codes.md](attribute_codes.md).

### Estrutura Básica

```
CÓDIGO-ATRIBUTO: VALOR-ATRIBUTO
```

Por exemplo:
- `01: PGMABCD         ` (program_id)
- `20: BANKING         ` (business_domain)
- `40: CUSTFILE        ` (file_name)

### Principais Categorias de Atributos

- **Atributos Gerais (1-19)**: program_id, transaction_id, user_id, environment, etc.
- **Atributos de Negócio (20-39)**: business_domain, business_unit, transaction_type, etc.
- **Atributos de Arquivo (40-49)**: file_name, file_type, record_type, etc.
- **Atributos de Banco de Dados (50-59)**: table_name, database_name, sql_operation, etc.
- **Atributos de Erro (60-69)**: error_code, error_type, error_severity, etc.
- **Atributos de Comunicação (70-79)**: program_called, queue_name, message_type, etc.

## Implementação

### Comunicação via CICS

As métricas podem ser acionadas através de uma transação CICS dedicada, utilizando o identificador único da métrica como chave. Por exemplo:

```
CHAVE: 101
VALOR: 1500
ATRIBUTOS: [01: PGMABCD, 40: CUSTFILE]
```

Isso indicaria que a métrica `cobol.file.records.read_total` possui o valor 1500, ou seja, 1500 registros foram lidos pelo programa PGMABCD no arquivo CUSTFILE.

### Estrutura de Dados COBOL

```cobol
* Definição para métricas
01 WS-METRIC.
   05 WS-METRIC-ID                 PIC 9(3).
   05 WS-METRIC-VALUE              PIC 9(9) COMP.
   05 WS-ATTR-COUNT                PIC 9(2).
   05 WS-ATTRIBUTES OCCURS 10 TIMES.
      10 WS-ATTR-CODE              PIC 9(2).
      10 WS-ATTR-VALUE             PIC X(16).
```

### Documentação Adicional

Para detalhes adicionais sobre a implementação, consulte:

- [counter_implementation.md](counter_implementation.md): Implementação de contadores (counters)
- [histogram_implementation.md](histogram_implementation.md): Implementação de histogramas
- [attribute_codes.md](attribute_codes.md): Códigos dos atributos contextuais

## Benefícios

- **Observabilidade moderna para sistemas legados**: Permite integrar sistemas COBOL com plataformas modernas de monitoramento.
- **Diagnóstico eficiente**: Facilita a identificação rápida de problemas em aplicações críticas.
- **Planejamento de capacidade**: Fornece dados para decisões informadas sobre recursos e infraestrutura.
- **Análise de tendências**: Possibilita detectar padrões e anomalias no comportamento das aplicações.
- **Otimização de desempenho**: Ajuda a identificar gargalos e oportunidades de melhoria.

## Integração com Prometheus

As métricas coletadas podem ser exportadas para o Prometheus no formato:

```
# HELP cobol_business_transaction_duration_seconds Duração das transações de negócio
# TYPE cobol_business_transaction_duration_seconds histogram
cobol_business_transaction_duration_seconds_bucket{le="0.1",business_domain="BANKING",transaction_type="PAYMENT"} 10
cobol_business_transaction_duration_seconds_bucket{le="0.5",business_domain="BANKING",transaction_type="PAYMENT"} 25
...
```

## Contribuições

Contribuições são bem-vindas! Sinta-se à vontade para sugerir novas métricas, melhorias na implementação ou otimizações adicionais.
