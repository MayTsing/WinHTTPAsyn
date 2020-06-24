unit uDataRequests;

interface

uses
  Classes, Windows;

type
  TDataRequest = class;
  TDataRequestProgressEvent = procedure(Sender: TObject; CurrentSize, ContentLength: DWORD) of object;
  TDataRequestDownloadFinished = procedure(Sender: TDataRequest; ACode: DWORD) of object;

  TDataRequestStates = set of (drsConnected, drsSendFinished, drsResponeFinished, drsCancel);
  TDataRequest = class
  private
    FOwner : TObject;
    FRefCnt: integer;       // 引用计数自释放
    FStates: TDataRequestStates;
    FProcessState: Integer;
    FParams: TStringStream;
    FDatas: TStringStream;
    FReadFinishedFree: Boolean;
    FCode: DWORD;
    FOnProgress: TDataRequestProgressEvent;
    FOnDownloadFinished: TDataRequestDownloadFinished;
    FUserData: string;
    function GetOutData: TStream;
    function GetOutLength: Int64;
  protected
    procedure DoDownloadFinished; virtual;
    function  GetParamsLen: int64; stdcall;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;
    procedure AddRef;
    procedure DecRef;
    procedure Cancel;

    class procedure Close(var ARequest: TDataRequest); static;

    function  PrepareData: Int64;
    procedure FinishRequest(ACode:DWORD);
    procedure Progress(Size, ContentLength: DWORD); stdcall;
    procedure SetParams(const s: RawByteString);
    function  ReadParams(var Buffer; Count: Longint): Longint;  stdcall;
    procedure WriteData(Buffer: Pointer; Size: DWORD); stdcall;
    property  ParamsLen: int64 read GetParamsLen;
    property  OnProgress: TDataRequestProgressEvent read FOnProgress write FOnProgress;
    property  OnDownloadFinished: TDataRequestDownloadFinished read FOnDownloadFinished write FOnDownloadFinished;
    property  Code: DWORD read FCode write FCode;
    property  OutData: TStream read GetOutData;
    property  OutLength: Int64 read GetOutLength;
    property  UserData: string read FUserData write FUserData;
    property  States: TDataRequestStates read FStates;
  end;

implementation

uses
  SysUtils;


function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      INC   EAX
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      DEC   EAX
end;

constructor TDataRequest.Create(AOwner: TObject);
begin
  FReadFinishedFree := False;
  FOwner := AOwner;
  FParams:= TStringStream.Create('', TEncoding.UTF8);
  FDatas := TStringStream.Create('', TEncoding.UTF8);

  {$ifdef Debug}
  Writeln('TDataRequest Create');
  {$endif}

end;

destructor TDataRequest.Destroy;
begin
  assert(FRefCnt = 0, '引用计数自释放模式，需要调用DecRef 进行释放。');

  FParams.Free;
  FDatas.Free;

  {$ifdef Debug}
  Writeln('TDataRequest Destroy');
  {$endif}

  inherited;
end;

procedure TDataRequest.AddRef;
begin
  InterlockedIncrement(FRefCnt);
  {$ifdef Debug}
  Writeln('TDataRequest AddRef ' + intToStr(FRefCnt));
  {$endif}

end;

procedure TDataRequest.DecRef;
begin
  InterlockedDecrement(FRefCnt);
  assert(FRefCnt >= 0, '引用计数自释放模式必须同AddRef配合使用');

  {$ifdef Debug}
  Writeln('TDataRequest DecRef ' + intToStr(FRefCnt));
  {$endif}

  if FRefCnt = 0 then
    Free;
end;

procedure TDataRequest.Cancel;
begin
  Include(FStates, drsCancel);
  {$ifdef Debug}
  Writeln('TDataRequest Cancel request');
  {$endif}
end;

class procedure TDataRequest.Close(var ARequest: TDataRequest);
begin
  if Assigned(ARequest) then
  begin
    ARequest.Cancel;
    ARequest.DecRef;
    ARequest := nil;
  end;
end;

procedure TDataRequest.DoDownloadFinished;
begin
  if Assigned(FOnDownloadFinished) then
    FOnDownloadFinished(Self, FCode);
end;

procedure TDataRequest.FinishRequest(ACode: DWORD);
begin
  FCode := ACode;
  Include(FStates, drsResponeFinished);
  if (FStates * [drsCancel] = []) then
    DoDownloadFinished;
end;

function TDataRequest.GetOutData: TStream;
begin
  Result := FDatas;
end;

function TDataRequest.GetOutLength: Int64;
begin
  Result := FDatas.Size;
end;

function TDataRequest.GetParamsLen: int64;
begin
  Result := FParams.Size;
end;

procedure TDataRequest.Progress(Size, ContentLength: DWORD);
begin
  if (FStates * [drsCancel] = []) and Assigned(OnProgress) then
    OnProgress(Self, Size, ContentLength);
end;

procedure TDataRequest.SetParams(const s: RawByteString);
begin
  FParams.Write(PByteArray(s)[0], Length(s));
end;

function TDataRequest.ReadParams(var Buffer; Count: Integer): Longint;
begin
  Result := FParams.Read(Buffer, Count);
end;

function TDataRequest.PrepareData: Int64;
begin
  FParams.Position := 0;
  Result := FParams.Size;
end;

procedure TDataRequest.WriteData(Buffer: Pointer; Size: DWORD);
begin
  FDatas.Write(Buffer^, Size);
end;

end.
