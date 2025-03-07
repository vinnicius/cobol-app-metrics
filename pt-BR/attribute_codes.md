# Identificadores de Atributos Contextuais para Métricas COBOL

Este documento define os identificadores únicos para os atributos contextuais utilizados nas métricas de aplicações COBOL. Cada atributo possui um código numérico para otimizar a comunicação entre sistemas.

## Estrutura Padrão de Atributos

Para economizar espaço de memória e padronizar a comunicação, todos os atributos seguem o formato:

```
CÓDIGO-ATRIBUTO: VALOR-ATRIBUTO
```

Onde:
- `CÓDIGO-ATRIBUTO`: Número inteiro entre 1-99 que identifica o tipo de atributo
- `VALOR-ATRIBUTO`: String alfanumérica com tamanho fixo de 16 caracteres (preenchido com espaços em branco à direita quando necessário)

## Códigos de Atributos

### Atributos Gerais (1-19)

- **01**: `program_id` - Identificador do programa COBOL
- **02**: `transaction_id` - Identificador da transação CICS
- **03**: `user_id` - Identificador do usuário
- **04**: `environment` - Ambiente (DEV, TEST, PROD)
- **05**: `timestamp` - Timestamp da coleta (formato YYYYMMDDHHMMSS)
- **06**: `system_id` - Identificador do sistema
- **07**: `subsystem_id` - Identificador do subsistema
- **08**: `region_id` - Identificador da região
- **09**: `job_id` - Identificador do job

### Atributos de Domínio de Negócio (20-39)

- **20**: `business_domain` - Domínio de negócio (ex: BANKING, INSURANCE)
- **21**: `business_unit` - Unidade de negócio
- **22**: `transaction_type` - Tipo de transação de negócio
- **23**: `entity_type` - Tipo de entidade de negócio
- **24**: `process_name` - Nome do processo de negócio
- **25**: `priority_level` - Nível de prioridade
- **26**: `channel` - Canal de origem
- **27**: `customer_segment` - Segmento de cliente
- **28**: `product_id` - Identificador do produto
- **29**: `service_id` - Identificador do serviço

### Atributos de Operações de Arquivo (40-49)

- **40**: `file_name` - Nome do arquivo
- **41**: `file_type` - Tipo de arquivo
- **42**: `record_type` - Tipo de registro
- **43**: `operation_type` - Tipo de operação de arquivo
- **44**: `access_method` - Método de acesso

### Atributos de Banco de Dados (50-59)

- **50**: `table_name` - Nome da tabela
- **51**: `database_name` - Nome do banco de dados
- **52**: `sql_operation` - Tipo de operação SQL
- **53**: `cursor_name` - Nome do cursor
- **54**: `schema_name` - Nome do schema

### Atributos de Erro (60-69)

- **60**: `error_code` - Código de erro
- **61**: `error_type` - Tipo de erro
- **62**: `error_severity` - Severidade do erro
- **63**: `error_source` - Fonte do erro
- **64**: `error_message` - Mensagem de erro resumida
- **65**: `retry_count` - Contador de tentativas

### Atributos de Comunicação (70-79)

- **70**: `program_called` - Programa chamado
- **71**: `queue_name` - Nome da fila
- **72**: `message_type` - Tipo de mensagem
- **73**: `protocol` - Protocolo de comunicação
- **74**: `endpoint` - Endpoint de comunicação

## Estrutura de Dados COBOL para Atributos

```cobol
       * Definição de atributo com código e valor
       01 WS-ATTRIBUTE.
          05 WS-ATTR-CODE              PIC 9(2).
          05 WS-ATTR-VALUE             PIC X(16).
          
       * Array de atributos para uma métrica (até 10 atributos)
       01 WS-METRIC-ATTRIBUTES.
          05 WS-ATTR-COUNT             PIC 9(2).
          05 WS-ATTRIBUTES OCCURS 10 TIMES.
             10 WS-ATTR-CODE           PIC 9(2).
             10 WS-ATTR-VALUE          PIC X(16).
```

## Exemplo de Uso

Para registrar uma métrica com atributos contextuais:

```cobol
       * Preparar os atributos para a métrica
       MOVE 5 TO WS-ATTR-COUNT.
       
       * Atributo 1: program_id
       MOVE 1 TO WS-ATTRIBUTES(1).
       MOVE 'PGMABCD         ' TO WS-ATTR-VALUE(1).
       
       * Atributo 2: transaction_id
       MOVE 2 TO WS-ATTRIBUTES(2).
       MOVE 'TRX1            ' TO WS-ATTR-VALUE(2).
       
       * Atributo 3: environment
       MOVE 4 TO WS-ATTRIBUTES(3).
       MOVE 'PROD            ' TO WS-ATTR-VALUE(3).
       
       * Atributo 4: file_name
       MOVE 40 TO WS-ATTRIBUTES(4).
       MOVE 'CUSTFILE        ' TO WS-ATTR-VALUE(4).
       
       * Atributo 5: business_domain
       MOVE 20 TO WS-ATTRIBUTES(5).
       MOVE 'BANKING         ' TO WS-ATTR-VALUE(5).
```

Este formato otimizado reduz significativamente o espaço necessário para transmissão das métricas entre sistemas, ao mesmo tempo que mantém a capacidade de contextualizar adequadamente as métricas coletadas. 