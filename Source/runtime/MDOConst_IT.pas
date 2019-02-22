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
// Transtalte do Italy  / Translated to Italy
// FORNERO MONIA Marco - studiogfm@libero.it

unit MDOConst_ita;

interface

uses MDOUtils;
          
resourcestring
{ generic strings used in code }
  SMDODatabaseEditor = 'Editor di Da&tabase...';
  SMDOTransactionEditor = 'Editor di &Transazione...';
  SDatabaseFilter = 'Files di Database Firebird (*.gdb;*.fdb;*.fb)|*.gdb;*.fdb;*.fb|All files (*.*)|*.*';
  SDisconnectDatabase = 'Il Database e'' attualmente connesso. Disconnettere e continuare?';
  SCommitTransaction = 'La Transazione e'' attualmente attiva. Rollback e continuare?';
  SExecute = 'E&segui';
  SNoDataSet = 'Nessun dataset associato';
  SSQLGenSelect = 'Bisogna selezionare almeno un campo chiave ed un campo da aggiornare';
  SSQLNotGenerated = 'Non e'' stato generato il codice di UPDATE SQL , uscire lo stesso?';
  SMDOUpdateSQLEditor = 'Modifica il codice di &UPDATE SQL...';
  SMDODataSetEditor = 'Editor di &Dataset...';
  SSQLDataSetOpen = 'Impossibile determinare il nome del campo per %s';
  SDefaultTransaction = '%s, Default';

{ strings used in error messages}
  SUnknownError = 'Errore Sconosciuto';
  SFirebirdMissing = 'Impossibile trovare nel percorso la libreria del Client di Firebird. Installare Firebird per utilizzare questa funzione';
  SFirebirdInstallMissing = 'La libreria DLL Firebird di installazione(ibinstall.dll) non si trova nel percorso. Installare Firebird per utilizzare questa funzione';
  SIB60feature = '%s e'' una funzione di InterBase 6. Aggiornare ad Interbase 6 per utilizzare questa funzione';
  SNotSupported = 'Funzione non supportata';
  SNotPermitted = 'Non Permesso';
  SFileAccessError = 'Errore di Accesso al file temporaneo';
  SConnectionTimeout = 'Connessione al Database troncata per time out';
  SCannotSetDatabase = 'Impossibile assegnare il database';
  SCannotSetTransaction = 'Impossibile assegnare la transazione';
  SOperationCancelled = 'Operazione cancellata per richiesta utente';
  SDPBConstantNotSupported = 'La costante DPB (isc_dpb_%s) non e'' supportata';
  SDPBConstantUnknown = 'La costante DPB (%d) e'' sconosciuta';
  STPBConstantNotSupported = 'La costante TPB (isc_tpb_%s) non e'' supportata';
  STPBConstantUnknown = 'La costante TPB (%d) e'' sconosciuta';
  SDatabaseClosed = 'Impossibile eseguire l''operazione -- il DB non e'' aperto';
  SDatabaseOpen = 'Impossibile eseguire l''operazione -- il DB e'' ancora aperto';
  SDatabaseNameMissing = 'Manca il nome del Database';
  SNotInTransaction = 'Transazione non attiva';
  SInTransaction = 'Transazione attiva';
  STimeoutNegative = 'I Valori di Timeout non possono essere negativi';
  SNoDatabasesInTransaction = 'Nessun databse elencato nel componente della transazione';
  SUpdateWrongDB = 'Aggiornamento Database errato';
  SUpdateWrongTR = 'Aggiornamento Transazione errato. Il set si attende una transazione unica';
  SDatabaseNotAssigned = 'Database non assegnato';
  STransactionNotAssigned = 'Transazione non assegnata';
  SXSQLDAIndexOutOfRange = 'XSQLDA Indice fuori dal limite';
  SXSQLDANameDoesNotExist = 'XSQLDA nome inesistente (%s)';
  SEOF = 'Fine del File';
  SBOF = 'Inizio del file';
  SInvalidStatementHandle = 'Puntatore istruzione non valido';
  SSQLOpen = 'MDOSQL Aperto';
  SSQLClosed = 'MDOSQL Chiuso';
  SDatasetOpen = 'Dataset open';
  SDatasetClosed = 'Dataset Chiuso';
  SUnknownSQLDataType = 'Tipo di dati SQL sconosciuto (%d)';
  SInvalidColumnIndex = 'Indice di colonna non valido (l''indice eccede i limiti consentiti)';
  SInvalidParamColumnIndex = 'Parametro per l''indice non valido (l''indice eccede i limiti consentiti)';
  SInvalidDataConversion = 'Conversione di dati non valida';
  SColumnIsNotNullable = 'La colonna non puo'' essere impostata a null (%s)';
  SBlobCannotBeRead = 'Lo stream per il Blob non puo'' essere letto';
  SBlobCannotBeWritten = 'Lo stream per il Blob non puo'' essere scritto';
  SEmptyQuery = 'Query vuota';
  SCannotOpenNonSQLSelect = 'Impossibile fare una "open" per una richiesta non di tipo SELECT. Usare ExecQuery';
  SNoFieldAccess = 'Nessun accesso al campo "%s"';
  SFieldReadOnly = 'il Campo "%s" e'' di sola lettura';
  SFieldNotFound = 'Impossibile trovare il Campo "%s"';
  SNotEditing = 'Non e'' in modalita'' modifica';
  SCannotInsert = 'Impossibile inserire nel dataset. (Nessuna query di inserimento)';
  SCannotPost = 'Impossibile fare il Post. (Nessuna query di Inserimento/modifica)';
  SCannotUpdate = 'Impossibile la modifica. (Nessuna query di modifica)';
  SCannotDelete = 'Impossibile cancellare dal dataset. (Nessuna query di cancellazione)';
  SCannotRefresh = 'Impossibile rinfrescare la riga. (Nessuna query di rinfresco)';
  SBufferNotSet = 'Buffer non impostato';
  SCircularReference = 'Referenze circolari non permesse';
  SSQLParseError = 'Errore interpretazione SQL:' + CRLF + CRLF + '%s';
  SUserAbort = 'Abortito dall'utente';
  SDataSetUniDirectional = 'Il Data set e'' uni-direzionale';
  SCannotCreateSharedResource = 'Impossibile creare la risorsa condivisa. (Errore di Windows %d)';
  SWindowsAPIError = 'Errore delle API di Windows. (Errore di Windows %d [$%.8x])';
  SColumnListsDontMatch = 'La lista delle colonne non coincide';
  SColumnTypesDontMatch = 'I Tipi di colonna non coincidono. (Dall''indice: %d; All''indice: %d)';
  SCantEndSharedTransaction = 'Impossibile terminare una trasazione condivisa a meno che non sia forzata uguale all''azione di timeout della transazione';
  SFieldUnsupportedType = 'Tipo di dato non supportato';
  SCircularDataLink = 'Referenza circolare del DataLink';
  SEmptySQLStatement = 'Richiesta SQL Vuota';
  SIsASelectStatement = 'Usa OPEN per una richiesta di SELECT';
  SRequiredParamNotSet = 'Un parametro richiesto non e'' valorizzato';
  SNoStoredProcName = 'Nessun nome assegnato alla Stored Procedure';
  SIsAExecuteProcedure = 'Usa ExecProc per una Procedura normale; usa una TQuery per una procedura di SELECT';
  SUpdateFailed = 'Aggiornamento Fallito';
  SNotCachedUpdates = 'CachedUpdates non abilitato';
  SNotLiveRequest = 'La richiesta non e'' LIVE - Impossibile modificare';
  SNoProvider = 'Nessun Provider';
  SNoRecordsAffected = 'Nessun record segnalato';
  SNoTableName = 'Nessun nome di tabella assegnato';
  SCannotCreatePrimaryIndex = 'Impossibile creare indici primari; vengono creati automaticamente';
  SCannotDropSystemIndex = 'Impossibile eliminare un indice di sistema';
  STableNameMismatch = 'Nome tabella sconosciuto';
  SIndexFieldMissing = 'Campo dell''indice mancante';
  SInvalidCancellation = 'Impossibile cancellare mentre si sta processando';
  SInvalidEvent = 'Evento non valido';
  SMaximumEvents = 'Il massimo limite di eventi e'' stato superato';
  SNoEventsRegistered = 'Nessun evento registrato';
  SInvalidQueueing = 'Accodamento non valido';
  SInvalidRegistration = 'Registrazione non valida';
  SInvalidBatchMove = 'Movimento BATCH Non valido';
  SSQLDialectInvalid = 'Dialetto SQL non valido';
  SSPBConstantNotSupported = 'Costante SPB non supportata';
  SSPBConstantUnknown = 'Costante SPB sconosciuta';
  SServiceActive = 'Impossibile fare l''operazione -- Il servizio non e'' attivo';
  SServiceInActive = 'Impossibile fare l''operazione -- Il servizio e'' attivo';
  SServerNameMissing = 'Manca il nome del Server';
  SQueryParamsError = 'Parametro della Query mancante o non corretto';
  SStartParamsError = 'Parametro iniziale mancante o non corretto';
  SOutputParsingError = 'Valore inaspettato per il buffer di output';
  SUseSpecificProcedures = 'Generico ServiceStart non applicabile: Usa una Specifica Procedura per impostare i parametri di configurazione';
  SSQLMonitorAlreadyPresent = 'E'' gia'' presente un''istanza di SQL Monitor';
  SCantPrintValue = 'Impossibile stampare il valore';
  SEOFReached = 'SEOFRichiesta';
  SEOFInComment = 'EOF trovato in commento';
  SEOFInString = 'EOF trovato nella stringa';
  SParamNameExpected = 'Aspetto un nome di parametro';
  SSuccess = 'Esecuzione terminata con successo';
  SDelphiException = 'Eccezione Delphi %s';
  SNoOptionsSet = 'Nessuna opzione di installazione selezionata';
  SNoDestinationDirectory = 'Percorso di destinazione non specificato';
  SNosourceDirectory = 'Percorso di partenza non specificato';
  SNoUninstallFile = 'Nome del file di deinstallazione non specificato';
  SOptionNeedsClient = 'il componente %s richiede il Client per funzionare correttamente';
  SOptionNeedsServer = 'il componente %s richiede il Server per funzionare correttamente';
  SInvalidOption = 'Specificata una ozione non valida';
  SInvalidOnErrorResult = 'Il valore di ritorno di onError e'' inaspettato';
  SInvalidOnStatusResult = 'Il valore di ritorno di onStatus e'' inaspettato';

  SCSVExportNoDataset =  'It is not possible to export the data without a dataset.';
  SCSVExportNoFileName = 'It is not possible to export the data without a filename.';
  
  SMercuryVersion = 'Mercury Database Objects RC1';
  SEditSQL = 'Edit SQL';
  SDPBConstantUnknownEx = 'La costante DPB (%s) e'' sconosciuta';
  STPBConstantUnknownEx = 'La costante TPB (%s) e'' sconosciuta';

implementation

end.