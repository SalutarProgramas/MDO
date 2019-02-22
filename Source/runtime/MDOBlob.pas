{************************************************************}
{                                                            }
{                  Mercury Database Objects                  }
{                                                            }
{          Copyright(c) 2002-2005, The Mercury Team          }
{                  Contact: info@mdolib.org                  }
{                                                            }
{           Based on the FreeIBComponents written            }
{          by  Gregory H. Deatz - gdeatz@hlmdd.com           }
{           and InterBase Express 4.3 created by             }
{                    Inprise Corporation.                    }
{                                                            }
{************************************************************}

unit MDOBlob;

interface

uses
  SysUtils, Classes, MDOHeader, MDOErrorCodes, MDOExternals, DB, MDO,
  MDODatabase, MDOUtils;


const
  DefaultBlobSegmentSize = 16 * 1024;

type
  { TMDOBlobStream }
  TMDOBlobStream = class (TStream)
  private
    FBase: TMDOBase;
    FBlobID: TISC_QUAD;
    FBlobInitialized: Boolean;
    FBlobMaxSegmentSize: Long;
    FBlobNumSegments: Long;
    FBlobSize: Long;
    FBlobType: Short;
    FBuffer: PChar;
    FHandle: TISC_BLOB_HANDLE;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FPosition: Long;
  protected
    procedure CloseBlob;
    procedure CreateBlob;
    procedure EnsureBlobInitialized;
    procedure GetBlobInfo;
    function GetDatabase: TMDODataBase;
    function GetDBHandle: PISC_DB_HANDLE;
    function GetTransaction: TMDOTransaction;
    function GetTRHandle: PISC_TR_HANDLE;
    procedure OpenBlob;
    procedure SetBlobID(Value: TISC_QUAD);
    procedure SetDatabase(Value: TMDODataBase);
    procedure SetMode(Value: TBlobStreamMode);
    procedure SetTransaction(Value: TMDOTransaction);
  public
    constructor Create;
    destructor Destroy; override;
    function Call(ErrCode: ISC_STATUS; RaiseError: Boolean): ISC_STATUS;
    procedure CheckReadable;
    procedure CheckWritable;
    procedure Finalize;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
    function Read(var Buffer; Count: Longint): LongInt; override;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(Stream: TStream);
    function Seek(Offset: Longint; Origin: Word): LongInt; override;
    procedure SetSize(NewSize: Long); override;
    procedure Truncate;
    function Write(const Buffer; Count: Longint): LongInt; override;
    property BlobID: TISC_QUAD read FBlobID write SetBlobID;
    property BlobMaxSegmentSize: Long read FBlobMaxSegmentSize;
    property BlobNumSegments: Long read FBlobNumSegments;
    property BlobSize: Long read FBlobSize;
    property BlobType: Short read FBlobType;
    property Database: TMDODataBase read GetDatabase write SetDatabase;
    property DBHandle: PISC_DB_HANDLE read GetDBHandle;
    property Handle: TISC_BLOB_HANDLE read FHandle;
    property Mode: TBlobStreamMode read FMode write SetMode;
    property Modified: Boolean read FModified;
    property Transaction: TMDOTransaction read GetTransaction write 
            SetTransaction;
    property TRHandle: PISC_TR_HANDLE read GetTRHandle;
  end;
  
  procedure GetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; var NumSegments, MaxSegmentSize,
                       TotalSize: Long; var BlobType: Short);
  procedure ReadBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar; BlobSize: Long);
  procedure WriteBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar; BlobSize: Long);

implementation

uses MDOIntf, MDOCustomDataSet;

procedure GetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; var NumSegments, MaxSegmentSize,
                      TotalSize: Long; var BlobType: Short);
var
  items: array[0..3] of Char;
  results: array[0..99] of Char;
  i, item_length: Integer;
  item: Integer;
begin
  items[0] := Char(isc_info_blob_num_segments);
  items[1] := Char(isc_info_blob_max_segment);
  items[2] := Char(isc_info_blob_total_length);
  items[3] := Char(isc_info_blob_type);

  if isc_blob_info(StatusVector, hBlobHandle, 4, @items[0], SizeOf(results),
                    @results[0]) > 0 then
    MDODatabaseError;

  i := 0;
  while (i < SizeOf(results)) and (results[i] <> Char(isc_info_end)) do
  begin
    item := Integer(results[i]); Inc(i);
    item_length := isc_vax_integer(@results[i], 2); Inc(i, 2);
    case item of
      isc_info_blob_num_segments:
        NumSegments := isc_vax_integer(@results[i], item_length);
      isc_info_blob_max_segment:
        MaxSegmentSize := isc_vax_integer(@results[i], item_length);
      isc_info_blob_total_length:
        TotalSize := isc_vax_integer(@results[i], item_length);
      isc_info_blob_type:
        BlobType := isc_vax_integer(@results[i], item_length);
    end;
    Inc(i, item_length);
  end;
end;

procedure ReadBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar; BlobSize: Long);
var
  CurPos: Long;
  BytesRead, SegLen: UShort;
  LocalBuffer: PChar;
begin
  CurPos := 0;
  LocalBuffer := Buffer;
  SegLen := UShort(DefaultBlobSegmentSize);
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if not ((isc_get_segment(StatusVector, hBlobHandle, @BytesRead, SegLen,
                             LocalBuffer) = 0) or
            (StatusVectorArray[1] = isc_segment)) then
      MDODatabaseError;
    Inc(LocalBuffer, BytesRead);
    Inc(CurPos, BytesRead);
    BytesRead := 0;
  end;
end;

procedure WriteBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar;
  BlobSize: Long);
var
  CurPos, SegLen: Long;
begin
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if isc_put_segment(StatusVector, hBlobHandle, SegLen,
         PChar(@Buffer[CurPos])) > 0 then
      MDODatabaseError;
    Inc(CurPos, SegLen);
  end;
end;


{ TMDOBlobStream }
{
******************************** TMDOBlobStream ********************************
}
constructor TMDOBlobStream.Create;
begin
  inherited Create;
  FBase := TMDOBase.Create(Self);
  FBuffer := nil;
  FBlobSize := 0;
end;

destructor TMDOBlobStream.Destroy;
begin
  if (FHandle <> nil) and
     (Call(isc_close_blob(StatusVector, @FHandle), False) > 0) then
    MDODatabaseError;
  FBase.Free;
  SetSize(0);
  inherited Destroy;
end;

function TMDOBlobStream.Call(ErrCode: ISC_STATUS; RaiseError: Boolean): 
        ISC_STATUS;
begin
  result := 0;
  if Transaction <> nil then
    result := Transaction.Call(ErrCode, RaiseError)
  else if RaiseError and (ErrCode > 0) then
    MDODatabaseError;
end;

procedure TMDOBlobStream.CheckReadable;
begin
  if FMode = bmWrite then MDOError(mdoeBlobCannotBeRead, [nil]);
end;

procedure TMDOBlobStream.CheckWritable;
begin
  if FMode = bmRead then MDOError(mdoeBlobCannotBeWritten, [nil]);
end;

procedure TMDOBlobStream.CloseBlob;
begin
  Finalize;
  if (FHandle <> nil) and
     (Call(isc_close_blob(StatusVector, @FHandle), False) > 0) then
    MDODatabaseError;
end;

procedure TMDOBlobStream.CreateBlob;
begin
  CheckWritable;
  FBlobID.gds_quad_high := 0;
  FBlobID.gds_quad_low := 0;
  Truncate;
end;

procedure TMDOBlobStream.EnsureBlobInitialized;
begin
  if not FBlobInitialized then
    case FMode of
      bmWrite:
        CreateBlob;
      bmReadWrite: begin
        if (FBlobID.gds_quad_high = 0) and
           (FBlobID.gds_quad_low = 0) then
          CreateBlob
        else
          OpenBlob;
      end;
      else
        OpenBlob;
    end;
  FBlobInitialized := True;
end;

procedure TMDOBlobStream.Finalize;
begin
  if (not FBlobInitialized) or (FMode = bmRead) or (not FModified) then
    exit;
  { need to start writing to a blob, create one }
  Call(isc_create_blob2(StatusVector, DBHandle, TRHandle, @FHandle, @FBlobID,
                       0, nil), True);
  MDOBlob.WriteBlob(@FHandle, FBuffer, FBlobSize);
  Call(isc_close_blob(StatusVector, @FHandle), True);
  FModified := False;
end;

procedure TMDOBlobStream.GetBlobInfo;
var
  iBlobSize: Long;
begin
  MDOBlob.GetBlobInfo(@FHandle, FBlobNumSegments, FBlobMaxSegmentSize,
    iBlobSize, FBlobType);
  SetSize(iBlobSize);
end;

function TMDOBlobStream.GetDatabase: TMDODataBase;
begin
  result := FBase.Database;
end;

function TMDOBlobStream.GetDBHandle: PISC_DB_HANDLE;
begin
  result := FBase.DBHandle;
end;

function TMDOBlobStream.GetTransaction: TMDOTransaction;
begin
  result := FBase.Transaction;
end;

function TMDOBlobStream.GetTRHandle: PISC_TR_HANDLE;
begin
  result := FBase.TRHandle;
end;

procedure TMDOBlobStream.LoadFromFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMDOBlobStream.LoadFromStream(Stream: TStream);
begin
  CheckWritable;
  EnsureBlobInitialized;
  Stream.Position := 0;
  SetSize(Stream.Size);
  if FBlobSize <> 0 then
    Stream.ReadBuffer(FBuffer^, FBlobSize);
  FModified := True;
end;

procedure TMDOBlobStream.OpenBlob;
begin
  CheckReadable;
  Call(isc_open_blob2(StatusVector, DBHandle, TRHandle, @FHandle,
                     @FBlobID, 0, nil), True);
  try
    GetBlobInfo;
    SetSize(FBlobSize);
    MDOBlob.ReadBlob(@FHandle, FBuffer, FBlobSize);
  except
    Call(isc_close_blob(StatusVector, @FHandle), False);
    raise;
  end;
  Call(isc_close_blob(StatusVector, @FHandle), True);
end;

function TMDOBlobStream.Read(var Buffer; Count: Longint): LongInt;
begin
  CheckReadable;
  EnsureBlobInitialized;
  if (Count <= 0) then
  begin
    result := 0;
    exit;
  end;
  if (FPosition + Count > FBlobSize) then
    result := FBlobSize - FPosition
  else
    result := Count;
  Move(FBuffer[FPosition], Buffer, result);
  Inc(FPosition, Result);
end;

procedure TMDOBlobStream.SaveToFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMDOBlobStream.SaveToStream(Stream: TStream);
begin
  CheckReadable;
  EnsureBlobInitialized;
  if FBlobSize <> 0 then
  begin
    Seek(0, soFromBeginning);
    Stream.WriteBuffer(FBuffer^, FBlobSize);
  end;
end;

function TMDOBlobStream.Seek(Offset: Longint; Origin: Word): LongInt;
begin
  EnsureBlobInitialized;
  case Origin of
    soFromBeginning     : FPosition := Offset;
    soFromCurrent	: Inc(FPosition, Offset);
    soFromEnd           : FPosition := FBlobSize + Offset;
  end;
  result := FPosition;
end;

procedure TMDOBlobStream.SetBlobID(Value: TISC_QUAD);
begin
  System.Move(Value, FBlobID, SizeOf(TISC_QUAD));
  FBlobInitialized := False;
end;

procedure TMDOBlobStream.SetDatabase(Value: TMDODataBase);
begin
  FBase.Database := Value;
  FBlobInitialized := False;
end;

procedure TMDOBlobStream.SetMode(Value: TBlobStreamMode);
begin
  FMode := Value;
  FBlobInitialized := False;
end;

procedure TMDOBlobStream.SetSize(NewSize: Long);
begin
  if (NewSize <> FBlobSize) then
  begin
    ReallocMem(FBuffer, NewSize);
    FBlobSize := NewSize;
    if NewSize = 0 then
      FBuffer := nil;
  end;
end;

procedure TMDOBlobStream.SetTransaction(Value: TMDOTransaction);
begin
  FBase.Transaction := Value;
  FBlobInitialized := False;
end;

procedure TMDOBlobStream.Truncate;
begin
  SetSize(0);
end;

function TMDOBlobStream.Write(const Buffer; Count: Longint): LongInt;
begin
  CheckWritable;
  EnsureBlobInitialized;
  result := Count;
  if Count <= 0 then
    exit;
  if (FPosition + Count > FBlobSize) then
    SetSize(FPosition + Count);
  Move(Buffer, FBuffer[FPosition], Count);
  Inc(FPosition, Count);
  FModified := True;
end;

end.
