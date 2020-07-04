unit uWinHttps;

{*
  Http �ͻ�����������

  -- fpack@163.com

  version 1.0
  - Windows ʵ�� WinHttp�ӿ�, ʹ���첽����ģʽ
    WinHttp ͷ����pas�� Delphi10.2 �л�ȡ��������΢��Щ�޸ġ�
    ʹ�ö�̬����Dllģʽ����ʼ��API

*}

interface

uses
  Classes, SysUtils, Windows, uWinHttpAPI;

type
  TDataRequest = class;
  TDataRequestClass = class of TDataRequest;
  THttpConnection = class;
  THttpContext = class;

  PConnectParams = ^TConnectParams;
  TConnectParams = record
    Server: string;
    Port: Word;
    IsHttps: boolean;
    ProxyName: string;
    ProxyByPass: string;
    ConnectionTimeOut: DWORD;
    SendTimeout: DWORD;
    ReceiveTimeout: DWORD;
    procedure Init(const ASvr: string; APort: word; AIsSSL: boolean);


  end;

  TWinHttpUpload = function(Sender: TObject; CurrentSize, ContentLength: DWORD): boolean of object;
  TWinHttpDownload = function(Sender: TObject; CurrentSize, ContentLength, ChunkSize: DWORD; const ChunkData): boolean of object;
  TWinHttpProgress = procedure(Sender: TObject; CurrentSize, ContentLength: DWORD) of object;


  TResponseDataFun = reference to procedure(Sender: TDataRequest; Code: DWORD);
  TDataRequestProgressEvent = procedure(Sender: TObject; CurrentSize, ContentLength: DWORD) of object;
  TRequestFinishedEvent = procedure(Sender: TDataRequest; ACode: DWORD) of object;
//  TResponseDataFun = reference to procedure(Sender: THttpContext; Code: DWORD);

  TDataRequestStates = set of (drsConnected, drsSendFinished, drsResponeFinished, drsCancel);
  TDataBuildOptions = set of (dboAutoFree);

  IResponseEvent = interface
    ['{BF376E5D-B65F-4F7B-A659-2C66B1F6CF84}']
    function GetIsActived: Boolean; stdcall;
    procedure SetIsActived(const Value: Boolean); stdcall;
    procedure Load(Sender: THttpContext; Code: DWORD); stdcall;
    procedure Close; stdcall;
    property IsActived: Boolean read GetIsActived write SetIsActived;
  end;

  THttpObjectStatus = set of (hosFree);
  TContextOptions = set of (coDisableZip,       // ����Ҫ�����ѹ������
                            coDumpResponseData  // ��������ֵ, ֻҪ״̬��
                            );

  THttpObjectState = set of (hosWriting, hosWrited, hosReading, hosReaded,
      hosCloseing, hosDestroying, hosFreeNotification);
  //csLoading ,

  // HTTP ����������
  THttpContext = class
  private
    {$ifdef Debug}
    FDebugID: Cardinal;
    {$endif}
    FOwner: TObject;
    FSender: TObject;
    FComponentState: THttpObjectState;
    FRequestHandle: HINTERNET;

    FAPI: string;
    FMethod: string;
    FIsHttps: boolean;
    FOptions : TContextOptions;

    FBuffer: RawByteString;
    FBufferSize: DWORD;
    FDownloadChunkSize: DWORD;  // ���ش���鳤��
    // response Data
    FCode: DWORD;
    FEncoding :RawByteString;

    FOutDataLength: DWORD;
    FResponseTmpData: TStream;
    FResponseReadSize: DWORD;   // ��ȡ�ܳ���

    // ���Ĵ�����Ϣ
    FLastError: DWORD;
    FErrorMsg: string;

    FWaitEvent: THandle;    // ͬ���ź�
    FOnFinshed: TNotifyEvent;
    FDatas: TDataRequest;

    procedure InitBuff;
    function  InternalGetInfo(Info: DWORD): RawByteString;
    function  InternalGetInfo32(Info: DWORD): DWORD;
    function  SetOption(AOpt, AFlags: DWORD): Boolean;
    function  AddHeader(const Data: string): boolean;
    procedure WriteLastError;
    procedure SetDatas(const Value: TDataRequest);
  protected
    fOnDownload: TWinHttpDownload;
    fOnProgress: TWinHttpProgress;
    FOnUpload: TWinHttpUpload;
    procedure WriteLastRequestError;
    procedure ConvertRequestBuffer;
    procedure PushBuffer(Buffer: Pointer; Size: DWORD);
    function  QueryData: boolean;
    function  ReadData(Size: DWORD): Boolean;

    procedure DoSendRequestComplete(dwStatusInformationLength: DWORD);
    procedure DoHeadersAvailable(dwStatusInformationLength: DWORD);
    procedure DoDataAvailable(lpvStatusInformation: LPVOID);
    procedure DoReadComplete(lpvStatusInformation: LPVOID; dwStatusInformationLength: DWORD);
    function  DoRequest: Cardinal;
    procedure DoRequestError(lpvStatusInformation: LPWINHTTP_ASYNC_RESULT);
    procedure DoResponseData;
    function  IsHttps: Boolean;
    function  IsCancel: Boolean;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    procedure CloseRequst;
    function Request: Integer;

    property API: string read FAPI write FAPI;
    property Method: string read FMethod write FMethod;
    property Owner: TObject read FOwner;
    property Sender: TObject read FSender;

    property LastError: DWORD read FLastError write FLastError;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;

    property DownloadChunkSize: DWORD read FDownloadChunkSize write FDownloadChunkSize;
    property Code: DWORD read FCode;
    property Datas: TDataRequest read FDatas write SetDatas;
    property OnDownload: TWinHttpDownload read fOnDownload write fOnDownload;
    property OnFinshed: TNotifyEvent read FOnFinshed write FOnFinshed;
    property OnProgress: TWinHttpProgress read fOnProgress write fOnProgress;
    property OnUpload: TWinHttpUpload read FOnUpload write FOnUpload;
    property ComponentState: THttpObjectState read FComponentState;

  end;

  THttpConnection = class
  private
    FOwner: TObject;
    FComponentState: THttpObjectState;
    function GetParams: TConnectParams;
    procedure SetParams(const Value: TConnectParams);
  protected
    FContextList: TList;
    FParams: TConnectParams;
    FURL: string;
    procedure DoInit; virtual;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    function BuildContext(Sender: TObject; const API, AMethod: string): THttpContext; virtual;

    function Exec(Sender: TObject; API, Method, AParams: string;
                ACall: TRequestFinishedEvent; AOpts: TDataBuildOptions): TDataRequest;

    function Async(Sender: TObject; API, Method: string; AData: TDataRequest;
        AOptions: TContextOptions = []): Boolean; overload;
    function Async(Sender: TObject; API, Method, AParams: string): Boolean; overload;
    function Await(Sender: TObject; const API, Method: string;
        AData: TDataRequest; AOptions: TContextOptions = []): Boolean; overload;
    property Params: TConnectParams read GetParams write SetParams;
    property ComponentState: THttpObjectState read FComponentState;
  end;

  THttpConnectionClass = class of THttpConnection;


  EWinHTTP = Class(Exception);

  THttpRequestContext = class(THttpContext)
  protected
  public
  end;

  // WinHttp API
  TWinHttpConnection = class(THttpConnection)
  private
    FSession: HINTERNET;
    FConnection: HINTERNET;
  protected
    procedure InternalConnect;
    procedure InternalDisconnect;
    procedure InternetCloseHandle(var h: HINTERNET);
    procedure DoInit; override;
  public
    destructor  Destroy; override;
    function BuildContext(Sender: TObject; const API, AMethod: string): THttpContext; override;
  end;



  TDataRequest = class
  private
    FOwner : TObject;
    FRefCnt: integer;       // ���ü������ͷ�
    FStates: TDataRequestStates;
    FIsOwnerParam: boolean;
    FParams: TStream;
    FIsOwnerData: boolean;
    FRequestID :Integer;  // ����Ľ��״̬ S_OK ִ������ɹ�
    FDatas: TStream;
    FReadFinishedFree: Boolean;
    FCode: DWORD;
    FOnProgress: TDataRequestProgressEvent;
    FOnDownloadFinished: TRequestFinishedEvent;
    FUserData: string;
    FErrorMsg: string;
    FLastError: DWORD;
    function GetOutData: TStream;
    function GetOutLength: Int64;
    procedure SetOutData(const Value: TStream);
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
    class function BuildOf(AClass: TDataRequestClass; AOwner: TObject; const AParams: RawByteString; const AUserData: string;
                    AFinishedFun: TRequestFinishedEvent; AOpts: TDataBuildOptions = []): TDataRequest;
    class function Build(AOwner: TObject; const AParams: RawByteString; const AUserData: string;
                    AFinishedFun: TRequestFinishedEvent; AOpts: TDataBuildOptions = []): TDataRequest; overload; static;
    class function Build(AOwner: TObject; const AParams: RawByteString; const AUserData: string;
                    AOpts: TDataBuildOptions = []): TDataRequest; overload; static;
    class function Build(AOwner: TObject; const AParams: RawByteString; AOpts: TDataBuildOptions): TDataRequest; overload; static;
    class function Build(AOwner: TObject; const AParams, AOutData: TStream; AOpts: TDataBuildOptions = []): TDataRequest; overload; static;

    function  PrepareData: Int64;
    procedure FinishRequest(ACode:DWORD);
    procedure Progress(Size, ContentLength: DWORD); stdcall;
    procedure SetParams(const s: RawByteString); overload;
    procedure SetParams(s: TStream); overload;
    function  ReadParams(var Buffer; Count: Longint): Longint;  stdcall;
    procedure WriteData(Buffer: Pointer; Size: DWORD); stdcall;
    function  OutText: RawByteString;
    property  ParamsLen: int64 read GetParamsLen;
    property  OnProgress: TDataRequestProgressEvent read FOnProgress write FOnProgress;
    property  OnDownloadFinished: TRequestFinishedEvent read FOnDownloadFinished write FOnDownloadFinished;
    property  Code: DWORD read FCode write FCode;
    property  OutData: TStream read GetOutData write SetOutData;
    property  OutLength: Int64 read GetOutLength;
    property  UserData: string read FUserData write FUserData;
    property  States: TDataRequestStates read FStates;
    property  LastError: DWORD read FLastError;
    property  ErrorMsg: string read FErrorMsg;
  end;

  function GetHttpConnecitonClass: THttpConnectionClass;
  function BuildConnParam(const ASvr: string; APort: word; AIsSSL: boolean): TConnectParams;


implementation

uses
  uLogWriter, SynZip, untCommFuns;

type
  TWorkThread = class(TThread)
  private
    FContext: THttpContext;
    FWaitCount: Cardinal;
    FAPI: string;
    FLoading: boolean;
    FWaitEvent: THandle;
  protected
    constructor Create(AContext: THttpContext);
    procedure Execute; override;
  end;

var
  OSVersionInfo: TOSVersionInfoEx;
  LogOutLevel: Integer = MAXWORD;
  MaxRequestID: Cardinal = 0;
  FResponseList: TList = nil;
  Mutex :TMutex = nil;
  {$ifdef debug}
  _MaxDebugID: cardinal = 0;
  {$endif}


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

procedure WriteLog(ALevel: TLogWriteKind; const s: string); overload;
begin
  if ord(ALevel) < LogOutLevel then
    LogWriter.Add(ALevel, s);
  //OutputDebugString(Pchar(s));
  //Writeln(Pchar(s));
  {$ifdef CONSOLE}
  Writeln('#' + CONSTNAMES_Kind[alevel] + ':' + s);
  {$endif}

end;

procedure WriteLog(ALevel: TLogWriteKind; const Fmt: string; const args: array of const); overload;
begin
  WriteLog(ALevel, format(Fmt, args));
end;

function Ansi7ToUnicode(const Ansi: RawByteString): RawByteString;
var
  n, i: PtrInt;
begin
  // fast ANSI 7 bit conversion
  result := '';
  if Ansi='' then
    exit;
  n := length(Ansi);
  SetLength(result,n*2+1);
  for i := 0 to n do // to n = including last #0
    PWordArray(pointer(result))^[i] := PByteArray(pointer(Ansi))^[i];
end;

function SysErrorMessagePerModule(Code: DWORD; ModuleName: PChar): string;
var
  tmpLen: DWORD;
  err: PChar;
begin
  result := '';
  if Code=NO_ERROR then
    exit;

  tmpLen := FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    pointer(GetModuleHandle(ModuleName)),Code, LOCALE_USER_DEFAULT,@err,0,nil);
  try
    while (tmpLen>0) and (ord(err[tmpLen-1]) in [0..32,ord('.')]) do
      dec(tmpLen);
    SetString(result,err,tmpLen);
  finally
    LocalFree(HLOCAL(err));
  end;

  if result='' then
  begin
    result := SysErrorMessage(Code);
    if result='' then
    begin
      case code of
        ERROR_WINHTTP_CANNOT_CONNECT: result := 'cannot connect';
        ERROR_WINHTTP_TIMEOUT       : result := 'timeout';
        ERROR_WINHTTP_INVALID_SERVER_RESPONSE : result := 'invalid server response';
        else result := IntToHex(Code,8);
      end;
    end;
  end;
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var LastError: Integer;
    Error: Exception;
begin
  LastError := GetLastError;
  if LastError <> NO_ERROR then
    Error := ModuleException.CreateFmt('%s error %d (%s)',
      [ModuleName,LastError, SysErrorMessagePerModule(LastError, ModuleName)]) else
    Error := ModuleException.CreateFmt('Undefined %s error',[ModuleName]);
  raise Error;
end;

function GetHttpConnecitonClass: THttpConnectionClass;
begin
  Result := TWinHttpConnection;
end;

function BuildConnParam(const ASvr: string; APort: word; AIsSSL: boolean): TConnectParams;
var
  r: TConnectParams;
begin
  r.Init(ASvr, APort, AIsSSL);
  Result := r;
end;


constructor THttpConnection.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FContextList:= TList.Create;
end;

destructor THttpConnection.Destroy;
begin
  Include(FComponentState, hosDestroying);
  // ContextList �������ͷ�����
//  for I := FContextList.Count - 1 downto 0 do
//  begin
//    cObj := THttpContext(FContextList[i]);
//    Include(cObj.FComponentState, hosFreeNotification);
//    FContextList.Delete(i);
//  end;

  FContextList.Free;
  inherited;
end;

procedure THttpConnection.DoInit;
begin
end;

function THttpConnection.GetParams: TConnectParams;
begin
  Result := FParams;
end;

procedure THttpConnection.SetParams(const Value: TConnectParams);
begin
  FParams := Value;
  if FParams.Port = INTERNET_DEFAULT_PORT then
  begin
    if FParams.IsHttps then
      FParams.Port := INTERNET_DEFAULT_HTTPS_PORT
    else FParams.Port := INTERNET_DEFAULT_HTTP_PORT;
  end;

  if FParams.ConnectionTimeOut = 0 then
    FParams.ConnectionTimeOut := 30000;
  if FParams.SendTimeout = 0 then
    FParams.SendTimeout := 30000;
  if FParams.ReceiveTimeout = 0 then
    FParams.ReceiveTimeout := 30000;

  if FParams.IsHttps then FURL := 'https://'
  else FURL := 'http://';
  FURL := FURL + FParams.Server + ':' + IntToStr(FParams.Port) + '/';
  DoInit;
end;

function THttpConnection.Exec(Sender: TObject; API, Method, AParams: string;
    ACall: TRequestFinishedEvent; AOpts: TDataBuildOptions): TDataRequest;
var
  cContext: THttpContext;
  cData: TDataRequest;
begin
  // �첽��������
  cData := TDataRequest.Build(Sender, UTF8Encode(AParams), AOpts);
  cData.OnDownloadFinished := ACall;

  cContext := BuildContext(Sender, API, Method);
  cContext.Datas := cData;
  cContext.FOptions := [];
  cData.FRequestID := cContext.Request;

  // �첽ִ�в�����ֵ
  //  ��Ҫ����д��־֮��Ĳ�������������ֵ����
  Result := cData;
end;


function THttpConnection.Async(Sender: TObject; API, Method: string;
    AData: TDataRequest; AOptions: TContextOptions = []): Boolean;
var
  cContext: THttpContext;
begin
  // �첽��������
  cContext := BuildContext(Sender, API, Method);
  cContext.Datas := AData;
  cContext.FOptions := AOptions;
  Result := cContext.Request = ID_OK;
end;

function THttpConnection.Async(Sender: TObject; API, Method, AParams: string): Boolean;
begin
  // �첽ִ�в�����ֵ
  //  ��Ҫ����д��־֮��Ĳ�������������ֵ����
  Result := Async(Sender, API, Method,
        TDataRequest.Build(Sender, UTF8Encode(AParams), [dboAutoFree]),
        [coDumpResponseData, coDisableZip]);
end;



function THttpConnection.Await(Sender: TObject; const API, Method: string;
    AData: TDataRequest; AOptions: TContextOptions = []): Boolean;
var
  cContext: THttpContext;
  cWorkThread: TWorkThread;
  hEvent: THandle;
begin
  // �첽��������
  hEvent := CreateEvent(nil, True, False, 'RequestData');
  if hEvent = 0 then
  begin
    WriteLog(lkErr, 'Http Request CreateEvent failed %s', [GetLastError()]);
    Exit(False);
  end;

  WriteLog(lkDebug, 'WinHttp Await :' + AData.FOwner.ClassName + ' Method:' + Method + ' URL: ' + API);

  //WriteLog(lkDebug, '1/4 CreateEvent %x', [hEvent]);
  cContext := BuildContext(Sender, API, Method);
  cContext.Datas := AData;
  cContext.FOptions := AOptions;
  cContext.FWaitEvent := hEvent;

  // ʹ��ͬ���ȴ��߳�
  cWorkThread := TWorkThread.Create(cContext);
  cWorkThread.FAPI := API;

  cWorkThread.Start;
  cWorkThread.WaitFor;
  cWorkThread.Free;

  CloseHandle(hEvent);

  WriteLog(lkDebug, 'WinHttp Await : End ' + API);

  Result := True;
end;


function THttpConnection.BuildContext(Sender: TObject; const API, AMethod: string): THttpContext;
begin
  Result := nil;
end;

{ TWinHttpConnection }

function TWinHttpConnection.BuildContext(Sender: TObject; const API, AMethod: string): THttpContext;
begin
  Result := THttpRequestContext.Create(Self);
  Result.FIsHttps := FParams.IsHttps;
  Result.FSender := Sender;
  Result.API := API;
  Result.Method := AMethod;
end;

destructor TWinHttpConnection.Destroy;
begin
  Include(FComponentState, hosDestroying);
  InternalDisconnect;
  inherited;
end;

procedure TWinHttpConnection.DoInit;
begin
  inherited;

  InternalDisconnect;
  if FParams.Server <> '' then
    InternalConnect;
end;

function GetCallbackName(dwInternetStatus: DWORD): string;
begin
  case dwInternetStatus of
    WINHTTP_CALLBACK_STATUS_RESOLVING_NAME      : Result := 'RESOLVING_NAME';
    WINHTTP_CALLBACK_STATUS_NAME_RESOLVED       : Result := 'NAME_RESOLVED';
    WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER: Result := 'CONNECTING_TO_SERVER';
    WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER : Result := 'CONNECTED_TO_SERVER';
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST     : Result := 'SENDING_REQUEST';
    WINHTTP_CALLBACK_STATUS_REQUEST_SENT        : Result := 'REQUEST_SENT';
    WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE  : Result := 'RECEIVING_RESPONSE';
    WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED   : Result := 'RESPONSE_RECEIVED';
    WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION  : Result := 'CLOSING_CONNECTION';
    WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED   : Result := 'CONNECTION_CLOSED';
    WINHTTP_CALLBACK_STATUS_HANDLE_CREATED      : Result := 'HANDLE_CREATED';
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING      : Result := 'HANDLE_CLOSING';
    WINHTTP_CALLBACK_STATUS_DETECTING_PROXY     : Result := 'DETECTING_PROXY';
    WINHTTP_CALLBACK_STATUS_REDIRECT            : Result := 'REDIRECT';
    WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE : Result := 'INTERMEDIATE_RESPONSE';
    WINHTTP_CALLBACK_STATUS_SECURE_FAILURE      : Result := 'SECURE_FAILURE';
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE   : Result := 'HEADERS_AVAILABLE';
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE      : Result := 'DATA_AVAILABLE';
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE       : Result := 'READ_COMPLETE';
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE      : Result := 'WRITE_COMPLETE';
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR       : Result := 'REQUEST_ERROR';
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE : Result := 'SENDREQUEST_COMPLETE';
    else Result := format('unknown %X', [dwInternetStatus]);
  end;
end;

procedure WinHTTPRequestCallback(
      hInternet: HINTERNET;
      dwContext: DWORD_PTR;
      dwInternetStatus: DWORD;
      lpvStatusInformation: LPVOID;
      dwStatusInformationLength: DWORD); stdcall;
var
  cContext: THttpContext;
begin
  {$ifdef debug}
//  WriteLog(lkDebug, 'Request Callback hInternet:%0.8X  dwContext: $%0.8X  state: %6X %s', [
//      integer(hInternet),
//      integer(dwContext),
//      dwInternetStatus, GetCallbackName(dwInternetStatus)]);
  {$endif}

  if dwContext = 0 then Exit;
  cContext := THttpContext(dwContext);
  if cContext.ComponentState * [hosDestroying, hosCloseing] <> [] then
    Exit;

  case dwInternetStatus of
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE,
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE:
      cContext.DoSendRequestComplete(dwStatusInformationLength);
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE:
      cContext.DoHeadersAvailable(dwStatusInformationLength);
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE:
      cContext.DoDataAvailable(lpvStatusInformation);
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE:
      cContext.DoReadComplete(lpvStatusInformation, dwStatusInformationLength);
    //WINHTTP_CALLBACK_STATUS_REDIRECT:;
    //WINHTTP_CALLBACK_STATUS_CLOSE_COMPLETE:;
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR:
      cContext.DoRequestError(LPWINHTTP_ASYNC_RESULT(lpvStatusInformation));
  end;
end;


procedure TWinHttpConnection.InternalConnect;
var
  iOpenType: integer;
  pCallback: TWinHttpStatusCallback;
  CallbackRes: PtrInt absolute pCallback;
  iProtocols: DWORD;
  sUserAgent: string;
begin
  if OSVersionInfo.dwOSVersionInfoSize=0 then
  begin
    OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
    GetVersionEx(OSVersionInfo);
  end;

  iOpenType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  if FParams.ProxyName='' then
  begin
    // Windows 8.1 and newer
    // https://docs.microsoft.com/en-us/windows/win32/api/winhttp/nf-winhttp-winhttpopen
    if (OSVersionInfo.dwMajorVersion>6) or
       ((OSVersionInfo.dwMajorVersion=6) and (OSVersionInfo.dwMinorVersion>=3)) then
      iOpenType := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY
    else
      iOpenType := WINHTTP_ACCESS_TYPE_NO_PROXY
  end;

  sUserAgent := 'Mozilla/5.0 (Windows M8Client '+ ClassName+')';
  FSession := WinHttpAPI.Open(PChar(sUserAgent), iOpenType,
                              PChar(FParams.ProxyName), PChar(FParams.ProxyByPass),
                              WINHTTP_FLAG_ASYNC); // ȫ�������첽ģʽ

  if not Assigned(FSession) then
    RaiseLastModuleError(WinHttpDll, EWinHTTP);

  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  // HTTP_DEFAULT_RESOLVETIMEOUT
  WinHttpAPI.SetTimeouts(FSession, 0, FParams.ConnectionTimeOut, FParams.SendTimeout, FParams.ReceiveTimeout);

  if FParams.IsHttps then
  begin
     iProtocols := WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 or
                   WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
     // Windows 7 and newer support TLS 1.1 & 1.2
     if (OSVersionInfo.dwMajorVersion>6) or
       ((OSVersionInfo.dwMajorVersion=6) and (OSVersionInfo.dwMinorVersion>=1)) then
       iProtocols :=  iProtocols or
                      WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or
                      WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;

    if not WinHttpAPI.SetOption(FSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
                                @iProtocols, SizeOf(iProtocols)) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);

    // ȫ��Callback
    //pCallback := WinHttpAPI.SetStatusCallback(FSession, WinHTTPSecurityErrorCallback,
    //                                WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, 0);
    //if CallbackRes = WINHTTP_INVALID_STATUS_CALLBACK then
    //  RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;

  fConnection := WinHttpAPI.Connect(FSession, PChar(FParams.Server), FParams.Port, 0);
  if fConnection=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

procedure TWinHttpConnection.InternalDisconnect;
begin
  if Assigned(FSession) then
    WinHttpAPI.SetStatusCallback(FSession, nil, 0, 0); // WinHTTPSecurityErrorCallback
  InternetCloseHandle(FConnection);
  InternetCloseHandle(FSession);
end;

procedure TWinHttpConnection.InternetCloseHandle(var h: HINTERNET);
begin
  if h <> nil then
    WinHttpAPI.CloseHandle(h);
  h := nil;
end;

procedure THttpContext.CloseRequst;
var
  cReqHandle: HINTERNET;
begin
{$ifdef debug}
//  WriteLog(lkDebug, format('THttpContext %3d CloseRequst', [FDebugID]));
{$endif}
  if ComponentState * [hosCloseing] <> [] then
    Exit;

  Include(FComponentState, hosCloseing);
  if Assigned(FRequestHandle) then
  begin
    cReqHandle := FRequestHandle;
    FRequestHandle := nil;
    WinHttpAPI.SetStatusCallback(cReqHandle, nil, 0, 0);
    WinHttpAPI.CloseHandle(cReqHandle);
  end;

  if ComponentState * [hosDestroying, hosFreeNotification] = [] then
    DoResponseData;

  if FWaitEvent <> 0 then
  begin
    if not SetEvent(FWaitEvent) then
      FErrorMsg := Format('SetEvent failed (%d)', [GetLastError()]);
    WriteLog(lkDebug, '3/4 SetEvent %x  %x', [FWaitEvent, integer(cReqHandle)]);
    FWaitEvent := 0;
  end;

  if ComponentState * [hosDestroying, hosFreeNotification] = [] then
    Free;
end;

constructor THttpContext.Create(AOwner: TObject);
begin
  {$ifdef debug}
  inc(_MaxDebugID);
  FDebugID := _MaxDebugID;
  {$endif}
  FCode := 0;
  FOwner := AOwner;
  FResponseReadSize := 0;
  FWaitEvent := 0;
  if Assigned(FOwner) and (FOwner is THttpConnection) then
    THttpConnection(FOwner).FContextList.Add(Self);

  {$ifdef Debug}
    WriteLog(lkDebug, format('THttpContext %3d Create', [_MaxDebugID]));
  {$endif}

end;

destructor THttpContext.Destroy;
begin
  Include(FComponentState, hosDestroying);

  {$ifdef Debug}
    WriteLog(lkDebug , format('THttpContext %3d Destroy', [FDebugID]));
  {$endif}
  if (FComponentState * [hosFreeNotification] <> []) and Assigned(FOwner) then
    if (FOwner is THttpConnection) and (THttpConnection(FOwner).ComponentState * [hosDestroying] = []) then
      THttpConnection(FOwner).FContextList.Remove(Self);

  if Assigned(FRequestHandle) then
  begin
    WinHttpAPI.SetStatusCallback(FRequestHandle, nil, 0, 0);
    WinHttpAPI.CloseHandle(FRequestHandle);
    FRequestHandle := nil;
  end;

  if Assigned(FResponseTmpData) then
    FreeAndNil(FResponseTmpData);

  if Assigned(FDatas) then
  begin
    FDatas.DecRef;
    FDatas := nil;
  end;
  inherited;
end;

function THttpContext.InternalGetInfo(Info: DWORD): RawByteString;
var dwSize, dwIndex: DWORD;
    tmp: RawByteString;
    i: integer;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not WinHttpAPI.QueryHeaders(FRequestHandle, Info, nil, nil, dwSize, @dwIndex) and
     (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    SetLength(tmp,dwSize);
    if WinHttpAPI.QueryHeaders(FRequestHandle, Info, nil, pointer(tmp), dwSize, @dwIndex) then begin
      dwSize := dwSize shr 1;
      SetLength(result,dwSize);
      for i := 0 to dwSize-1 do // fast ANSI 7 bit conversion
        PByteArray(result)^[i] := PWordArray(tmp)^[i];
    end;
  end;
end;

function THttpContext.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpAPI.QueryHeaders(FRequestHandle, Info, nil, @result, dwSize, @dwIndex) then
    result := 0;
end;

procedure THttpContext.PushBuffer(Buffer: Pointer; Size: DWORD);
begin
  // ����û��ѹ��������ֱ�ӷŵ�Ŀ������
  if FEncoding = '' then
  begin
    FDatas.WriteData(buffer, Size);
  end
  else
  begin
    if not Assigned(FResponseTmpData) then
        FResponseTmpData := TMemoryStream.Create;
    FResponseTmpData.Write(buffer^, Size);
  end;

  FResponseReadSize := FResponseReadSize + Size;

  if Assigned(FDatas) then
    FDatas.Progress(FResponseReadSize, FOutDataLength);
  {$ifdef Debug}
  //WriteLog(lkDebug, 'PushBuffer %x  size: %d/%d', [integer(self), FResponseReadSize, FOutDataLength]);
  {$endif}
end;

procedure THttpContext.WriteLastRequestError;
begin
  // last error info
  FLastError := GetLastError;
  if FLastError <> NO_ERROR then
    FErrorMsg := SysErrorMessagePerModule(FLastError, WinHttpDll)
  else
  begin
    FLastError := ERROR_BAD_NET_RESP;
    FErrorMsg := 'Undefined error';
  end;

end;

function THttpContext.QueryData: boolean;
begin
	Result := WinHttpAPI.QueryDataAvailable(FRequestHandle, nil);
  if not Result then
    WriteLastRequestError;
end;

function THttpContext.ReadData(Size: DWORD): Boolean;
begin
  if FBufferSize = 0 then
    InitBuff;

  Result := WinHttpAPI.ReadData(FRequestHandle, @PByteArray(FBuffer)[0], FBufferSize, nil);
  if not Result then
    WriteLastRequestError;
end;

function THttpContext.Request: Integer;
begin
  Include(FComponentState, hosWriting);
  Result := integer(DoRequest);
  Exclude(FComponentState, hosWriting);
  if Result <> S_OK then
  begin
    WriteLastError;
    CloseRequst;
  end;

end;

procedure THttpContext.ConvertRequestBuffer;
begin
  // ת�����յĻ�������
  // GZIP, BR
  if FEncoding = 'gzip' then
  begin
    if Assigned(FResponseTmpData) then
    begin
      GZReadToStream(FResponseTmpData, FDatas.OutData);
      FreeAndNil(FResponseTmpData);
    end;
  end;
end;

procedure THttpContext.DoSendRequestComplete(dwStatusInformationLength: DWORD);
begin
	// ׼������������Ӧ
  if (FDatas.States * [drsCancel] <> []) then
    CloseRequst
  else if not WinHttpAPI.ReceiveResponse(FRequestHandle, nil) then
  begin
    WriteLastRequestError;
    CloseRequst;
  end;
end;

procedure THttpContext.DoHeadersAvailable(dwStatusInformationLength: DWORD);
begin
  if (FDatas.States * [drsCancel] <> [])  then
  begin
    CloseRequst;
  end
  else
  begin
    FCode := InternalGetInfo32(WINHTTP_QUERY_STATUS_CODE);
    if (FOptions * [coDumpResponseData] = []) then
    begin
      FEncoding := InternalGetInfo(WINHTTP_QUERY_CONTENT_ENCODING);
      //FAcceptEncoding := InternalGetInfo(WINHTTP_QUERY_ACCEPT_ENCODING);
      FOutDataLength := InternalGetInfo32(WINHTTP_QUERY_CONTENT_LENGTH);
      //FContentType := InternalGetInfo(WINHTTP_QUERY_CONTENT_TYPE);

      {$ifdef Debug}
      WriteLog(lkDebug, UTF8ToString(InternalGetInfo(WINHTTP_QUERY_RAW_HEADERS_CRLF)));
      writelog(lkDebug, FEncoding);
      {$endif}
      // ��ȡ����
      if (FCode = HTTP_STATUS_OK)  then
      begin
        if not QueryData then
          CloseRequst;
      end
      else
        CloseRequst;
    end
    else
      CloseRequst;
  end;

  {$ifdef debug}
  WriteLog(lkDebug, 'readheader %x  size: %d', [integer(self), FOutDataLength]);
  {$endif}
end;

procedure THttpContext.DoDataAvailable(lpvStatusInformation: LPVOID);
var
  iSize: DWORD;
begin
  if (FDatas.States * [drsCancel] <> []) then
  begin
    CloseRequst;
  end
  else
  begin
    iSize := LPDWORD(lpvStatusInformation)^;
    // iSize = 0 ���ݽ������
    if (iSize > 0) then
    begin
      if not ReadData(iSize) then
        CloseRequst;
    end
    else
    begin
      ConvertRequestBuffer;
      CloseRequst;
    end;
  end;
end;

procedure THttpContext.DoReadComplete(lpvStatusInformation: LPVOID; dwStatusInformationLength: DWORD);
begin
  if dwStatusInformationLength > 0  then
  begin
    PushBuffer(lpvStatusInformation, dwStatusInformationLength);
    if not QueryData then
      CloseRequst;
  end;
end;

procedure THttpContext.DoRequestError(lpvStatusInformation: LPWINHTTP_ASYNC_RESULT);
begin
  case lpvStatusInformation.dwResult of
    API_RECEIVE_RESPONSE      :;
    API_QUERY_DATA_AVAILABLE  :;
    API_READ_DATA             :;
    API_WRITE_DATA            :;
    API_SEND_REQUEST          :;
  end;
  if FCode = 0 then
    FCode := HTTP_STATUS_SERVICE_UNAVAIL;
  WriteLastRequestError;
  CloseRequst;
end;

procedure THttpContext.DoResponseData;
begin
  if Assigned(FDatas) then
  begin
    FDatas.FLastError := FLastError;
    FDatas.FErrorMsg := FErrorMsg;
    FDatas.FinishRequest(FCode);
    if Assigned(FOnFinshed) then
      FOnFinshed(Self);
    FDatas.DecRef;
    FDatas := nil;
  end;
end;

function THttpContext.IsCancel: Boolean;
begin
  Result := (ComponentState * [hosCloseing, hosDestroying, hosFreeNotification] <> []) or
             not Assigned(FDatas) or
             (FDatas.States * [drsCancel] <> []);
end;

function THttpContext.IsHttps: Boolean;
begin
  Result := TWinHttpConnection(Owner).FParams.IsHttps;
end;

procedure THttpContext.WriteLastError;
begin
  LastError := GetLastError;
  if LastError <> NO_ERROR then
    ErrorMsg := Format('%s error %d (%s)', [ClassName,LastError, SysErrorMessagePerModule(LastError, WinHttpDll)])
  else
    ErrorMsg := Format('Undefined %s error',[WinHttpDll]);
end;

procedure THttpContext.SetDatas(const Value: TDataRequest);
begin
  assert(FDatas = nil, 'SetDatas �жദ��ֵ������������ ');

  FDatas := Value;
  if Assigned(FDatas) then
    FDatas.AddRef;
end;

function THttpContext.SetOption(AOpt, AFlags: DWORD): Boolean;
begin
 Result := WinHttpAPI.SetOption(FRequestHandle, AOpt, @AFlags, sizeOf(AFlags));
end;

function THttpContext.AddHeader(const Data: string): boolean;
begin
  Result := True;
  if (Data<>'') then
  begin
    Result := WinHttpAPI.AddRequestHeaders(FRequestHandle, PChar(Data), length(Data),
      WINHTTP_ADDREQ_FLAG_COALESCE);
  end;
end;



function THttpContext.DoRequest: Cardinal;
const
  ALL_ACCEPT: array[0..1] of PWideChar = ('*/*',nil);
  IGNRECERTOPTS = SECURITY_FLAG_IGNORE_UNKNOWN_CA or
                  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
                  SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
                  SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;
//var
//  iFlags: DWORD;
//  pCallback: TWinHttpStatusCallback;
//  CallbackRes: PtrInt absolute pCallback;
//  iDataLen: DWORD;
//  sHeader: String;
var
  bSended: Boolean;
  iFlags: DWORD;
  pCallback: TWinHttpStatusCallback;
  CallbackRes: PtrInt absolute pCallback;
  iBufLen, iBytesWritten: DWORD;
  iCurrent: DWORD;
  L: DWORD;
  pBuf: Pointer;
begin
  Result := ERROR_WINHTTP_INVALID_URL;
  if (API = '') or (Method = '') then
    Exit;

  // options for a true RESTful request
  iFlags := WINHTTP_FLAG_REFRESH;
  if FIsHttps then
    iFlags := iFlags or WINHTTP_FLAG_SECURE;


  FRequestHandle := WinHttpAPI.OpenRequest(TWinHttpConnection(Owner).FConnection,
                PChar(Method), PChar(API), nil, nil, @ALL_ACCEPT, iFlags);
  if not Assigned(FRequestHandle) then
    Exit;
    // callback set
  pCallback := WinHttpAPI.SetStatusCallback(FRequestHandle,
                  WinHTTPRequestCallback,
                  WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, 0);
  if WINHTTP_INVALID_STATUS_CALLBACK = CallbackRes then
    Exit;

  //if FKeepAlive = 0 then
  //  SetOption(cRequest, WINHTTP_OPTION_DISABLE_FEATURE, WINHTTP_DISABLE_KEEP_ALIVE);

  // SSL ignore certificates
  if iFlags and WINHTTP_FLAG_SECURE = WINHTTP_FLAG_SECURE then
  begin
    if not SetOption(WINHTTP_OPTION_SECURITY_FLAGS, IGNRECERTOPTS) then
      Exit;
  end;

  if FOptions * [coDisableZip] = [] then
    if not AddHeader('Accept-Encoding: gzip') then
      Exit;
  if not AddHeader('Content-Type: application/json; charset=utf-8') then
    Exit;

  // Send request
  InitBuff;
  pBuf :=  @PByteArray(FBuffer)[0];

  L := DWORD(FDatas.PrepareData);
  if L <= FBufferSize then
  begin
    iBufLen := FDatas.ReadParams(pBuf^, FBufferSize);
    bSended :=  WinHttpAPI.SendRequest(FRequestHandle, nil,0,
                    pBuf,iBufLen,iBufLen,
                    DWORD_PTR(Pointer(Self)));
  end
  else
  begin
    bSended  :=  WinHttpAPI.SendRequest(FRequestHandle,nil,0,nil,0,L,DWORD_PTR(Pointer(Self)));
    if bSended then
    begin
      iCurrent := 0;
      while iCurrent < L do
      begin
        iBufLen := FDatas.ReadParams(pBuf^, FBufferSize);
        bSended :=WinHttpAPI.WriteData(FRequestHandle, pBuf, iBufLen, @iBytesWritten);
        if not bSended then
          Break;

        inc(iCurrent, iBytesWritten);
        if (FDatas.States * [drsCancel] <> []) or not FOnUpload(Self, iCurrent, L) then
        begin
          bSended := False;
          Break;
          //raise EWinHTTP.CreateFmt('OnUpload Canceled %s',[aMethod]);
        end;
      end;
    end;
  end;

  if bSended then
    Result := S_OK;
end;

procedure THttpContext.InitBuff;
var
  iBytes: DWORD;
begin
  if FBufferSize = 0 then
  begin
    iBytes := DownloadChunkSize;
    if iBytes <= 0 then iBytes := 65536; // 64KB
    SetLength(FBuffer, iBytes);
    FBufferSize := iBytes;
  end;
end;


constructor TWorkThread.Create(AContext: THttpContext);
begin
  inherited Create(True);
  FWaitCount := 0;
  FContext := AContext;
  FLoading := True;
  FWaitEvent := FContext.FWaitEvent;
end;

{ TWorkThread }

procedure TWorkThread.Execute;
const
  MAXCNT = 120000;
var
  res: Cardinal;
  iwait: Cardinal;
begin
  assert(FWaitEvent <> 0, 'Http wait event not create');
  iWait := WAIT_OBJECT_0;
  res := FContext.Request;
  if res = S_OK then
    iWait := WaitForSingleObject(FWaitEvent, MAXCNT);
  if iWait <> WAIT_OBJECT_0 then
    WriteLog(lkWarn, 'Request http %s Fa' , [FAPI]);
end;

procedure TConnectParams.Init(const ASvr: string; APort: word; AIsSSL:
    boolean);
begin
  Server := ASvr;
  Port := APort;
  IsHttps := AIsSSL;
  ProxyName:= '';
  ProxyByPass:= '';
  ConnectionTimeOut:= 0;
  SendTimeout:= 0;
  ReceiveTimeout:= 0;
end;

constructor TDataRequest.Create(AOwner: TObject);
begin
  FReadFinishedFree := False;
  FOwner := AOwner;
  //FParams:= TStringStream.Create('', TEncoding.UTF8);
  FIsOwnerParam := False;
  FIsOwnerData := True;
  FDatas := TStringStream.Create('', TEncoding.UTF8);
  {$ifdef Debug}
  WriteLog(lkDebug, 'TDataRequest Create');
  {$endif}

end;

destructor TDataRequest.Destroy;
begin
  assert(FRefCnt = 0, '���ü������ͷ�ģʽ����Ҫ����DecRef �����ͷš�');

  if FIsOwnerParam then
    FreeAndNil(FParams);
  if FIsOwnerData then
    FreeAndNil(FDatas);

  {$ifdef Debug}
  WriteLog(lkDebug, format('TDataRequest Destroy %s', [UserData]));
  {$endif}

  inherited;
end;

procedure TDataRequest.AddRef;
begin
  InterlockedIncrement(FRefCnt);
  {$ifdef Debug}
//  Writeln('TDataRequest AddRef ' + intToStr(FRefCnt));
  {$endif}
end;

procedure TDataRequest.DecRef;
begin
  InterlockedDecrement(FRefCnt);
  assert(FRefCnt >= 0, '���ü������ͷ�ģʽ����ͬAddRef���ʹ��');

//  {$ifdef Debug}
//  Writeln('TDataRequest DecRef ' + intToStr(FRefCnt));
//  {$endif}

  if FRefCnt = 0 then
    Free;
end;

procedure TDataRequest.Cancel;
begin
  Include(FStates, drsCancel);
  {$ifdef Debug}
//  Writeln('TDataRequest Cancel request');
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

class function TDataRequest.BuildOf(AClass: TDataRequestClass; AOwner: TObject;
    const AParams: RawByteString; const AUserData: string;
    AFinishedFun: TRequestFinishedEvent; AOpts: TDataBuildOptions = []): TDataRequest;
var
  cData: TDataRequest;
begin
  cData := TDataRequest(AClass.NewInstance).Create(AOwner);
  cData.SetParams(AParams);
  cData.UserData := AUserData;
  cData.OnDownloadFinished := AFinishedFun;
  if AOpts * [dboAutoFree] = [] then
    cData.AddRef;
  Result := cData;
end;

class function TDataRequest.Build(AOwner: TObject; const AParams: RawByteString;
    const AUserData: string; AFinishedFun: TRequestFinishedEvent;
    AOpts: TDataBuildOptions): TDataRequest;
begin
  ///  �����������ݶ���
  ///
  ///  ������ AParams       --- ����Json���� UTF8
  ///         AUserData     --- �û��Զ������ݣ����� Tag
  ///         AFinishedFun  --- �������ص�����
  ///         IsAutoFree    --- �Ƿ��Զ��ͷ�
  ///                 True�� ������ɺ�TDataRequest����Free�Զ��ͷţ�����Ҫ��ʾ���ùر�
  ///                 False: ���ⲿ�����ͷ�ʱ��
  ///
  Result := TDataRequest.BuildOf(TDataRequest, AOwner, AParams, AUserData, AFinishedFun, AOpts);
end;

class function TDataRequest.Build(AOwner: TObject; const AParams: RawByteString;
    const AUserData: string; AOpts: TDataBuildOptions): TDataRequest;
begin
  Result := TDataRequest.BuildOf(TDataRequest, AOwner, AParams, AUserData, nil, AOpts);
end;

class function TDataRequest.Build(AOwner: TObject; const AParams: RawByteString;
    AOpts: TDataBuildOptions): TDataRequest;
begin
  Result := TDataRequest.BuildOf(TDataRequest, AOwner, AParams, '', nil, AOpts);
end;

class function TDataRequest.Build(AOwner: TObject; const AParams, AOutData: TStream;
    AOpts: TDataBuildOptions):TDataRequest;
var
  cData: TDataRequest;
begin
  cData := TDataRequest.Create(AOwner);
  cData.SetParams(AParams);
  cData.OutData := AOutData;
  if AOpts * [dboAutoFree] = [] then
    cData.AddRef;
  Result := cData;
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
  begin
    Try
      DoDownloadFinished;
    except on E: exception do
      LogWriter.Err('TDataRequest.FinishRequest', E.ClassName + E.Message);
    End;
  end;
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

function TDataRequest.OutText: RawByteString;
var
  iLen: Cardinal;
begin
  iLen := OutData.Size;
  if iLen > 0 then
  begin
    SetLength(Result, iLen);
    OutData.Position := 0;
    OutData.Read(PByteArray(Result)[0], iLen);
  end
  else
    Result := UTF8Encode('{}');
end;

procedure TDataRequest.Progress(Size, ContentLength: DWORD);
begin
  if (FStates * [drsCancel] = []) and Assigned(OnProgress) then
    OnProgress(Self, Size, ContentLength);
end;

procedure TDataRequest.SetParams(const s: RawByteString);
begin
  if not Assigned(FParams) then
  begin
    FIsOwnerParam := True;
    FParams := TStringStream.Create('', TEncoding.UTF8);
  end;
  FParams.Write(PByteArray(s)[0], Length(s));
end;

procedure TDataRequest.SetParams(s: TStream);
begin
  if Assigned(FParams) then
    FreeAndNil(FParams);
  FIsOwnerParam := False;
  FParams := s;
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

procedure TDataRequest.SetOutData(const Value: TStream);
begin
  if FIsOwnerData and Assigned(FDatas) then
    FreeAndNil(FDatas);
  FDatas := Value;
  FIsOwnerData := False;
end;

procedure TDataRequest.WriteData(Buffer: Pointer; Size: DWORD);
begin
  FDatas.Write(Buffer^, Size);
end;


initialization
  Mutex :=TMutex.Create;
  FResponseList := TList.Create;

finalization
  FResponseList.Free;
  Mutex.Free;
end.


