{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2003, The Mercury Team          }
{              Contact: henrique@meira.net                   }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}

// Transtalte do Portuguese
// CARLOS MAÇÃO - cmacao@suprasoft.pt

// Versão Brasileira
// SILVIO DELGADO - silviodelphi@gmail.com - www.clubedosoftware.com.br


{ @abstract(Translate to Portuguese)
  @author(CARLOS MAÇÃO [cmacao@suprasoft.pt])
  @created(30 Aug 2004) }

unit MDOConst;

interface

uses MDOUtils;

resourcestring
{ generic strings used in code }
  SMDODatabaseEditor = 'Editor de &Base de Dados...';
  SMDOTransactionEditor = 'Editor de &Transação...';
 SDatabaseFilter = 'Arquivos BD Firebird (*.fdb;*.gdb;*.fb)|*.fdb;*.gdb;*.fb|Todos os Arquivos (*.*)|*.*';
  SDisconnectDatabase = 'A Base de Dados encontra-se desconectada. Deseja conectá-la e continuar?';
  SCommitTransaction = 'A Transação encontra-se ativa. Pretende cancelar a Transação e continuar?';
  SExecute = 'E&xecutar';
  SNoDataSet = 'Não existe associação ao Dataset';
  SSQLGenSelect = 'É necessário selecionar pelo menos um campo chave e um campo a modificar';
  SSQLNotGenerated = 'Não foram geradas quaisquer sentenças de modificação SQL, deseja sair mesmo assim?';
  SMDOUpdateSQLEditor = 'Editor &UpdateSQL...';
  SMDODataSetEditor = 'Editor do &Dataset...';
  SSQLDataSetOpen = 'Impossível determinar nomes de campos para %s';
  SDefaultTransaction = '%s, Por Omissão';

{ strings used in error messages}
  SUnknownError = 'Erro desconhecido';
  SFirebirdMissing = 'A biblioteca cliente do Firebird não foi encontrada. Por favor, instale o Firebird para poder usar esta funcionalidade';
  SFirebirdInstallMissing = 'A biblioteca de Instalação do Firebird ibinstall.dll não foi encontrada. Por favor, instale o Firebird para poder usar esta funcionalidade';
  SIB60feature = '%s é uma função do InterBase 6. Por favor, faça a atualização para o Firebird para poder usar esta funcionalidade';
  SNotSupported = 'Funcionalidade não suportada';
  SNotPermitted = 'Não permitido';
  SFileAccessError = 'Erro no acesso ao arquivo temporário';
  SConnectionTimeout = 'Foi excedido o tempo máximo para conexão à Base de Dados';
  SCannotSetDatabase = 'Não é possível atribuir a base de dados';
  SCannotSetTransaction = 'Não é possível atribuir a transação';
  SOperationCancelled = 'Operação cancelada a pedido do usuário';
  SDPBConstantNotSupported = 'A constante DPB (isc_dpb_%s) não é suportada';
  SDPBConstantUnknown = 'A constante DPB (%d) é desconhecida';
  STPBConstantNotSupported = 'A constante TPB (isc_tpb_%s) não é suportada';
  STPBConstantUnknown = 'A constante TPB (%d) é desconhecida';
  SDatabaseClosed = 'Não é possível executar a operação - Base de dados não está conectada';
  SDatabaseOpen = 'Não é possível executar a operação -- Base de dados está conectada';
  SDatabaseNameMissing = 'Falta o nome de Base de Dados';
  SNotInTransaction = 'A Transação não está ativa';
  SInTransaction = 'A Transação está ativa';
  STimeoutNegative = 'Valor de Tempo Limite não pode ser negativo';
  SNoDatabasesInTransaction = 'Não existem Bases de Dados atribuídas ao componente Transaction';
  SUpdateWrongDB = 'Erro ao atualizar a Base de Dados';
  SUpdateWrongTR = 'Erro ao atualizar a Transação. Esperada uma transação única';
  SDatabaseNotAssigned = 'Bade de Dados não atribuída';
  STransactionNotAssigned = 'Transação não atribuída';
  SXSQLDAIndexOutOfRange = 'Índice do XSQLDA fora dos limites';
  SXSQLDANameDoesNotExist = 'Nome de XSQLDA não existe (%s)';
  SEOF = 'Fim do arquivo';
  SBOF = 'Ínicio do arquivo';
  SInvalidStatementHandle = 'Manipulador de sentença inválido';
  SSQLOpen = 'MDOSQL Aberto';
  SSQLClosed = 'MDOSQL Fechado';
  SDatasetOpen = 'Dataset Aberto';
  SDatasetClosed = 'Dataset Fechado';
  SUnknownSQLDataType = 'Tipo de Dado SQL desconhecido (%d)';
  SInvalidColumnIndex = 'Índice de coluna inválido (o índice excede os limites permitidos)';
  SInvalidParamColumnIndex = 'Índice de parâmetro inválido (o índice excede os limites permitidos)';
  SInvalidDataConversion = 'Conversão de dados inválida';
  SColumnIsNotNullable = 'A Coluna não pode ser nula (%s)';
  SBlobCannotBeRead = 'A cadeia Blob não pôde ser lida';
  SBlobCannotBeWritten = 'A cadeia Blob não pôde ser escrita';
  SEmptyQuery = 'Sentença SQL vazia';
  SCannotOpenNonSQLSelect = 'Não é possível executar o "Open" numa sentença não-select. Use ExecQuery';
  SNoFieldAccess = 'Sem acesso ao campo "%s"';
  SFieldReadOnly = 'O Campo "%s" é somente-leitura';
  SFieldNotFound = 'O Campo "%s" não foi encontrado';
  SNotEditing = 'Não está em modo de edição';
  SCannotInsert = 'Não foi possível inserir no Dataset. (Não existe sentença de inserção SQL)';
  SCannotPost = 'Não foi possível gravar no Dataset. (Não existe sentença de alteração/inserção SQL)';
  SCannotUpdate = 'Não foi possível alterar. (Não existe sentença de alteração SQL)';
  SCannotDelete = 'Não foi possível excluir no dataset. (Não existe sentença de exclusão SQL)';
  SCannotRefresh = 'Não foi possível atualizar a linha. (Não existe sentença de atualização SQL)';
  SBufferNotSet = 'Buffer não definido';
  SCircularReference = 'Não é permitida referência circular';
  SSQLParseError = 'Erro no Parser SQL:' + CRLF + CRLF + '%s';
  SUserAbort = 'Operação abortada pelo usuário';
  SDataSetUniDirectional = 'O Dataset é uni-direcional';
  SCannotCreateSharedResource = 'Não foi possível criar o recurso partilhado. (Erro do Windows %d)';
  SWindowsAPIError = 'Erro API do Windows. (Erro do Windows %d [$%.8x])';
  SColumnListsDontMatch = 'Listas das Colunas não conferem';
  SColumnTypesDontMatch = 'Tipos de Dados das Colunas não conferem. (do índice: %d; ao índice: %d)';
  SCantEndSharedTransaction = 'Não é possível terminar uma transação partilhada senão de modo forçado e similar à ação TimeoutAction da transação';
  SFieldUnsupportedType = 'Tipo de dado do campo não suportado';
  SCircularDataLink = 'Referência circular no DataLink';
  SEmptySQLStatement = 'Sentença SQL vazia';
  SIsASelectStatement = 'use Open para um Procedimento de Seleção';
  SRequiredParamNotSet = 'Valor de parâmetro requerido não atribuído';
  SNoStoredProcName = 'Nenhum nome de Procedimento associado';
  SIsAExecuteProcedure = 'use ExecProc para procedimentos; use TQuery para procedimentos de seleção';
  SUpdateFailed = 'Falha na alteração';
  SNotCachedUpdates = 'CachedUpdates não ativado';
  SNotLiveRequest = 'Requisição não é modicável';
  SNoProvider = 'Sem Provider';
  SNoRecordsAffected = 'Nenhum registo afetado';
  SNoTableName = 'Sem nome de tabela atribuído';
  SCannotCreatePrimaryIndex = 'Não foi possível criar um Índice Primário; estes são criados automaticamente';
  SCannotDropSystemIndex = 'Não foi possível excluir o índice de sistema';
  STableNameMismatch = 'O nome da tabela não confere';
  SIndexFieldMissing = 'Campo indexado em falta';
  SInvalidCancellation = 'Não é possível cancelar eventos durante o processamento';
  SInvalidEvent = 'Evento inválido';
  SMaximumEvents = 'Numero máximo de eventos atingido';
  SNoEventsRegistered = 'Nenhum evento registrado';
  SInvalidQueueing = 'Fila de espera inválida';
  SInvalidRegistration = 'Registro inválido';
  SInvalidBatchMove = 'Batch Move inválido';
  SSQLDialectInvalid = 'Dialeto SQL inválido';
  SSPBConstantNotSupported = 'Constante SPB não suportada';
  SSPBConstantUnknown = 'Constante SPB desconhecida';
  SServiceActive = 'Não é possível executar a operação -- o serviço não está ativo';
  SServiceInActive = 'Não é possível executar a operação  -- o serviço está ativo';
  SServerNameMissing = 'Falta nome do servidor';
  SQueryParamsError = 'Parâmetros da consulta SQL incorretos ou faltantes';
  SStartParamsError = 'Parâmetros incorretos ou faltantes';
  SOutputParsingError = 'Valor do Buffer de Saída inesperado';
  SUseSpecificProcedures = 'Início de serviço genérico não aplicável: Use procedimentos específicos para alterar os parâmetros de configuração';
  SSQLMonitorAlreadyPresent = 'Já existe uma instância ativa do Monitor SQL';
  SCantPrintValue = 'Não é possível imprimir o valor';
  SEOFReached = 'EOF alcançado';
  SEOFInComment = 'Detectado EOF no comentário';
  SEOFInString = 'Detectado EOF na cadeia de texto';
  SParamNameExpected = 'Nome de parâmetro esperado';
  SSuccess = 'Execução concluída com sucesso';
  SDelphiException = 'Exceção Delphi %s';
  SNoOptionsSet = 'Não foram selecionadas opções de instalação';
  SNoDestinationDirectory = 'Diretório destino não está definido';
  SNosourceDirectory = 'Diretório de origem não está definido';
  SNoUninstallFile = 'Não foi definido o nome do arquivo de desinstalação';
  SOptionNeedsClient = 'O componente %s requer um Cliente para funcionar adequadamente';
  SOptionNeedsServer = 'O componente %s requer um Servidor para funcionar adequadamente';
  SInvalidOption = 'Foi especificada uma opção inválida';
  SInvalidOnErrorResult = 'Valor inesperado retornado por onError';
  SInvalidOnStatusResult = 'Valor inesperado retornado por onStatus';
  SGeneratorNotDefined = 'O gerador %s não está definido';

  SCSVExportNoDataset = 'Não é possível exportar os dados sem um DataSet disponível.';
  SCSVExportNoFileName = 'Não é possível exportar os dados sem um arquivo definido.';

  SMercuryVersion = 'Mercury Database Objects RC3';
  SEditSQL = 'Editar SQL';
  SDPBConstantUnknownEx = 'A constante DPB (%s) é desconhecida';
  STPBConstantUnknownEx = 'A constante TPB (%s) é desconhecida';

implementation

end.
