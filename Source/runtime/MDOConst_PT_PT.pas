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

{ @abstract(Translate to Portuguese)
  @author(CARLOS MAÇÃO [cmacao@suprasoft.pt])
  @created(30 Aug 2004) }

unit MDOConst;

interface

uses MDOUtils;

resourcestring
{ generic strings used in code }
  SMDODatabaseEditor = 'Editor de &Base de Dados...';
  SMDOTransactionEditor = 'Editor de &Transacção...';
  SDatabaseFilter = 'Ficheiros BD Firebird (*.fdb;*.gdb;*.fb)|*.fdb;*.gdb;*.fb|Todos os Ficheiros (*.*)|*.*';
  SDisconnectDatabase = 'A Base de Dados encontra-se desligada. Pretende ligá-la e continuar?';
  SCommitTransaction = 'A Transacção encontra-se activa. Pretende Anular a Transacção e continuar?';
  SExecute = 'E&xecutar';
  SNoDataSet = 'Não existe associação ao dataset';
  SSQLGenSelect = 'Tem que seleccionar pelo menos um campo chave e um campo a modificar';
  SSQLNotGenerated = 'Não foram geradas quaisquer sentenças de modificação SQL, pretende sair mesmo assim?';
  SMDOUpdateSQLEditor = 'Editor &UpdateSQL...';
  SMDODataSetEditor = 'Editor do &Dataset...';
  SSQLDataSetOpen = 'Impossível determinar nomes de campos para %s';
  SDefaultTransaction = '%s, Por Omissão';

{ strings used in error messages}
  SUnknownError = 'Erro desconhecido';
  SFirebirdMissing = 'A biblioteca cliente do Firebird não foi encontrada. Por favor instale o Firebird para poder usar esta funcionalidade';
  SFirebirdInstallMissing = 'A biblioteca de Instalação do Firebird ibinstall.dll não foi encontrada. Por favor instale o Firebird 6 para poder usar esta funcionalidade';
  SIB60feature = '%s é uma função do InterBase 6. Por favor faça a actualização para o Firebird 6 para poder usar esta funcionalidade';
  SNotSupported = 'Funcionalidade não suportada';
  SNotPermitted = 'Não permitido';
  SFileAccessError = 'Erro no acesso ao ficheiro Temporário';
  SConnectionTimeout = 'Foi excedido o tempo máximo para ligação a Base de Dados';
  SCannotSetDatabase = 'Não é possível atribuir a base de dados';
  SCannotSetTransaction = 'Não é possível atribuir a transacção';
  SOperationCancelled = 'Operação Cancelada a pedido do utilizador';
  SDPBConstantNotSupported = 'A Constante DPB (isc_dpb_%s) não é suportada';
  SDPBConstantUnknown = 'A Constante DPB (%d) é desconhecida';
  STPBConstantNotSupported = 'A Constante TPB (isc_tpb_%s) não é suportada';
  STPBConstantUnknown = 'A Constante TPB (%d) é desconhecida';
  SDatabaseClosed = 'Não é possível executar a operação -- A BD não se encontra ligada';
  SDatabaseOpen = 'Não é possível executar a operação -- A BD encontra-se ligada';
  SDatabaseNameMissing = 'Nome de Base de Dados em falta';
  SNotInTransaction = 'A Transacção não está activa';
  SInTransaction = 'A Transacção está activa';
  STimeoutNegative = 'Valor de Tempo Limite não podem ser negativos';
  SNoDatabasesInTransaction = 'Não existem Bases de Dados atribuídas ao componente transacção';
  SUpdateWrongDB = 'Modificando a Base de Dados incorrecta';
  SUpdateWrongTR = 'Modificando a transacção incorrecta. Esperada uma transacção Única';
  SDatabaseNotAssigned = 'Bade de Dados não atribuída';
  STransactionNotAssigned = 'Transacção não atribuída';
  SXSQLDAIndexOutOfRange = 'Índice do XSQLDA fora dos limites';
  SXSQLDANameDoesNotExist = 'Nome de XSQLDA não existe (%s)';
  SEOF = 'Fim do Ficheiro';
  SBOF = 'Ínicio do Ficheiro';
  SInvalidStatementHandle = 'Descritor de sentença Inválido';
  SSQLOpen = 'MDOSQL Aberto';
  SSQLClosed = 'MDOSQL Fechado';
  SDatasetOpen = 'Dataset Aberto';
  SDatasetClosed = 'Dataset Fechado';
  SUnknownSQLDataType = 'Tipo de Dados SQL desconhecido (%d)';
  SInvalidColumnIndex = 'Índice de coluna inválido (o índice excede os limites permitidos)';
  SInvalidParamColumnIndex = 'Índice de parâmetro inválido (o índice excede os limites permitidos)';
  SInvalidDataConversion = 'Conversão de dados inválida';
  SColumnIsNotNullable = 'A Coluna não pode ser nula (%s)';
  SBlobCannotBeRead = 'A cadeia Blob não pôde ser lida';
  SBlobCannotBeWritten = 'A cadeia Blob não pôde ser escrita';
  SEmptyQuery = 'Sentença SQL vazia';
  SCannotOpenNonSQLSelect = 'Não é possível "open" uma sentença não-select. Use ExecQuery';
  SNoFieldAccess = 'Sem acesso ao campo "%s"';
  SFieldReadOnly = 'O Campo "%s" é apenas-de-leitura';
  SFieldNotFound = 'O Campo "%s" não foi encontrado';
  SNotEditing = 'Não está em modo de edição';
  SCannotInsert = 'Não foi possível inserir no dataset. (Não existe sentença de inserção SQL)';
  SCannotPost = 'Não foi possível gravar no dataset. (Não existe sentença de modificação/inserção SQL)';
  SCannotUpdate = 'Não foi possível modificar. (Não existe sentença de modificação SQL)';
  SCannotDelete = 'Não foi possível suprimir no dataset. (Não existe sentença de supressão SQL)';
  SCannotRefresh = 'Não foi possível actualizar a linha. (Não existe sentença de actualização SQL)';
  SBufferNotSet = 'Buffer não definido';
  SCircularReference = 'Não é permitida referência circular';
  SSQLParseError = 'Erro no Parser SQL:' + CRLF + CRLF + '%s';
  SUserAbort = 'Operação abortada pelo utilizador';
  SDataSetUniDirectional = 'O Dataset é uni-direccional';
  SCannotCreateSharedResource = 'Não foi possível criar o recurso partilhado. (Erro do Windows %d)';
  SWindowsAPIError = 'Erro API do Windows. (Erro do Windows %d [$%.8x])';
  SColumnListsDontMatch = 'Listas das Colunas não conferem';
  SColumnTypesDontMatch = 'Tipos de Dados das Colunas não conferem. (Do índice: %d; Ao índice: %d)';
  SCantEndSharedTransaction = 'Não é possível terminar uma transacção partilhada senão de modo forçado e similar à acção TimeoutAction da transacção';
  SFieldUnsupportedType = 'Tipo de Dados do Campo não suportado';
  SCircularDataLink = 'Referência Circular no DataLink';
  SEmptySQLStatement = 'Sentença SQL Vazia';
  SIsASelectStatement = 'use Open para um Procedimento de Selecção';
  SRequiredParamNotSet = 'Valor de Parâmetro obrigatório não atribuído';
  SNoStoredProcName = 'Nenhum Nome de Procedimento associado';
  SIsAExecuteProcedure = 'use ExecProc para Procedimentos; use TQuery para Procedimentos de Selecção';
  SUpdateFailed = 'Modificação fracassada';
  SNotCachedUpdates = 'CachedUpdates não activados';
  SNotLiveRequest = 'O Pedido não é modicável';
  SNoProvider = 'Sem Provider';
  SNoRecordsAffected = 'Nenhum registo Afectado';
  SNoTableName = 'Não existe Nome de Tabela atribuído';
  SCannotCreatePrimaryIndex = 'Não foi possível criar um Índice Primário; estes são criados automaticamente';
  SCannotDropSystemIndex = 'Não foi possível Suprimir o Índice de Sistema';
  STableNameMismatch = 'O Nome da Tabela não confere';
  SIndexFieldMissing = 'Campo indexado em falta';
  SInvalidCancellation = 'Não é possível cancelar eventos durante o processamento';
  SInvalidEvent = 'Evento Inválido';
  SMaximumEvents = 'Numero Máximo de Eventos atingido';
  SNoEventsRegistered = 'Nenhum Evento Registado';
  SInvalidQueueing = 'Fila de Espera Inválida';
  SInvalidRegistration = 'Registo Inválido';
  SInvalidBatchMove = 'Batch Inválido';
  SSQLDialectInvalid = 'Dialecto SQL Inválido';
  SSPBConstantNotSupported = 'Constante SPB Não Suportada';
  SSPBConstantUnknown = 'Constante SPB Desconhecida';
  SServiceActive = 'Não é possível executar a operação -- o serviço não está ligado';
  SServiceInActive = 'Não é possível executar a operação  -- o serviço está ligado';
  SServerNameMissing = 'Nome do Servidor em falta';
  SQueryParamsError = 'Parâmetros da Consulta SQL incorrectos ou em falta';
  SStartParamsError = 'Parâmetros incorrectos ou em falta';
  SOutputParsingError = 'Valor do Buffer de Saída inesperado';
  SUseSpecificProcedures = 'Inicio de Serviço Genérico não aplicável: Use Procedimentos Específicos para modificar os parâmetros de configuração';
  SSQLMonitorAlreadyPresent = 'Já existe uma instância activa do Monitor SQL';
  SCantPrintValue = 'Não é possível imprimir o valor';
  SEOFReached = 'SEOFReached';
  SEOFInComment = 'Detectado EOF no comentário';
  SEOFInString = 'Detectado EOF na cadeia de texto';
  SParamNameExpected = 'Esperado nome de Parâmetro';
  SSuccess = 'Execução terminada com Sucesso';
  SDelphiException = 'ExcepçãoDelphi %s';
  SNoOptionsSet = 'Não foram seleccionadas opções de Instalação';
  SNoDestinationDirectory = 'Não está definida a DirectoriaDestino';
  SNosourceDirectory = 'Não está definida a DirectoriaOrigem';
  SNoUninstallFile = 'Não foi definido o Nome do Ficheiro de Desinstalação';
  SOptionNeedsClient = 'O componente %s requer um Cliente para funcionar convenientemente';
  SOptionNeedsServer = 'O componente %s requer um Servidor para funcionar convenientemente';
  SInvalidOption = 'Foi especificada uma Opção Inválida';
  SInvalidOnErrorResult = 'Valor inesperado retornado por onError';
  SInvalidOnStatusResult = 'Valor inesperado retornado por onStatus';
  SGeneratorNotDefined = 'O Gerador %s não está definido';

  SCSVExportNoDataset = 'Não é possível exportar os dados sem um DataSet disponível.';
  SCSVExportNoFileName = 'Não é possível exportar os dados sem um arquivo definido.';
  
  SMercuryVersion = 'Mercury Database Objects RC1';
  SEditSQL = 'Edita SQL';
  SDPBConstantUnknownEx = 'A constante DPB (%s) é desconhecida';
  STPBConstantUnknownEx = 'A constante TPB (%s) é desconhecida';

implementation

end.
