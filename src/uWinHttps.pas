unit uWinHttps;

{*
  Http 客户端请求连接

  -- fpack@163.com

  version 1.0
  - Windows 实现 WinHttp接口, 使用异步请求模式
    WinHttp 头定义pas从 Delphi10.2 中获取，内容稍微有些修改。
    使用动态加载Dll模式，初始化API

*}

interface

uses
  Classes, SysUtils, Windows, uWinHttpAPI, uDataRequests;

type
  THttpConnection = class;
  THttpContext = class;
  TRequestHandle = Pointer;

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


  TResponseDataFun = reference to procedure(Sender: THttpContext; Code: DWORD);

  IResponseEvent = interface
    ['{BF376E5D-B65F-4F7B-A659-2C66B1F6CF84}']
    function GetIsActived: Boolean; stdcall;
    procedure SetIsActived(const Value: Boolean); stdcall;
    procedure Load(Sender: THttpContext; Code: DWORD); stdcall;
    procedure Close; stdcall;
    property IsActived: Boolean read GetIsActived write SetIsActived;
  end;

  TContextOptions = set of (coDisableZip,       // 不需要服务等压缩数据
                            coDumpResponseData  // 丢弃返回值, 只要状态码
                            );

  // HTTP 请求上下文
  THttpContext = class
  private
    FCloseCount: Integer;
    {$ifdef Debug}
    FDebugID: Cardinal;
    {$endif}
    FOwner: TObject;
    FSender: TObject;
    FRequestHandle: HINTERNET;

    FAPI: string;
    FMethod: string;
    FIsHttps: boolean;
    FOptions : TContextOptions;

    FBuffer: RawByteString;
    FBufferSize: DWORD;
    FDownloadChunkSize: DWORD;  // 下载代码块长度
    // response Data
    FCode: DWORD;
    FEncoding :RawByteString;

    FOutDataLength: DWORD;
    FResponseTmpData: TStream;
    FResponseReadSize: DWORD;   // 读取总长度

    // 最后的错误信息
    FLastError: DWORD;
    FErrorMsg: string;
    FTickCount: Cardinal;       // 不精确的消耗计时


    FOnFinshed: TNotifyEvent;
    FDatas: TDataRequest;

    procedure InitBuff;
    function  InternalGetInfo(Info: DWORD): RawByteString;
    function  InternalGetInfo32(Info: DWORD): DWORD;
    function  GetOutDataLength: DWORD;
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
    function IsHttps: Boolean;
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
  end;

  THttpConnection = class
  private
    FOwner: TObject;
    function GetParams: PConnectParams;
  protected
    FContextList: TList;
    FParams: TConnectParams;
    FURL: string;
  public
    constructor Create(AOwner: TObject; const AParams: TConnectParams); virtual;
    destructor Destroy; override;
    function BuildContext(Sender: TObject; const API, AMethod: string): THttpContext; virtual;
    function Async(Sender: TObject; API, Method: string; AData: TDataRequest;
        AOptions: TContextOptions = []): Boolean;
    function Await(Sender: TObject; const API, Method: string;
        AData: TDataRequest; AOptions: TContextOptions = []): Boolean; overload;
    property Params: PConnectParams read GetParams;
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
    procedure InternetCloseHandle(var h: HINTERNET);
  public
    constructor Create(AOwner: TObject; const AParams: TConnectParams); override;
    destructor Destroy; override;
    function BuildContext(Sender: TObject; const API, AMethod: string): THttpContext; override;
  end;

  function GetHttpConnecitonClass: THttpConnectionClass;

implementation

uses
  uLogWriter, SynZip, untCommFuns;

type
  TWorkThread = class(TThread)
  private
    FContext: THttpContext;
    FCallback: TResponseDataFun;
    FFinished: Boolean;
    FWaitCount: Cardinal;
    FAPI: string;
    FParams: RawByteString;
    FRequestID: TRequestHandle;
    FLoading: boolean;
    procedure DoCallbackFun;
  protected
    constructor Create(AContext: THttpContext; ACall: TResponseDataFun);
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

procedure WriteLog(ALevel: TLogWriteKind; const s: string); overload;
begin
  //if ord(ALevel) < LogOutLevel then
  //  LogWriter.Add(ALevel, s);
  //OutputDebugString(Pchar(s));
  Writeln(Pchar(s));
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

constructor THttpConnection.Create(AOwner: TObject; const AParams: TConnectParams);
begin
  inherited Create;
  FOwner := AOwner;
  FParams := AParams;
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
  FContextList:= TList.Create;
end;

destructor THttpConnection.Destroy;
var
  cObj: TObject;
  I: Integer;
begin
  for I := FContextList.Count - 1 downto 0 do
  begin
    cObj := TObject(FContextList[i]);
    FContextList.Delete(i);
    cObj.Free;
  end;

  FContextList.Free;
  inherited;
end;

function THttpConnection.GetParams: PConnectParams;
begin
  Result := @FParams;
end;

function THttpConnection.Async(Sender: TObject; API, Method: string;
    AData: TDataRequest; AOptions: TContextOptions = []): Boolean;
var
  cContext: THttpContext;
begin
  // 异步处理请求
  cContext := BuildContext(Sender, API, Method);
  cContext.Datas := AData;
  cContext.FOptions := AOptions;
  Result := cContext.Request = ID_OK;
end;

function THttpConnection.Await(Sender: TObject; const API, Method: string;
    AData: TDataRequest; AOptions: TContextOptions = []): Boolean;
var
  cContext: THttpContext;
  cWorkThread: TWorkThread;
begin
  // 异步处理请求
  cContext := BuildContext(Sender, API, Method);
  cContext.Datas := AData;
  cContext.FOptions := AOptions;
//  cContext.FIsAutoFree := False;

  // 使用同步等待线程
  cWorkThread := TWorkThread.Create(cContext, nil);
  cWorkThread.FAPI := API;

  cWorkThread.Start;
  cWorkThread.WaitFor;
  cWorkThread.Free;


  Result := True;
end;


function THttpConnection.BuildContext(Sender: TObject; const API, AMethod:
    string): THttpContext;
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

constructor TWinHttpConnection.Create(AOwner: TObject;
  const AParams: TConnectParams);
begin
  inherited;
  InternalConnect;
end;

destructor TWinHttpConnection.Destroy;
begin
  if Assigned(FSession) then
    WinHttpAPI.SetStatusCallback(FSession, nil, 0, 0); // WinHTTPSecurityErrorCallback

  InternetCloseHandle(FConnection);
  InternetCloseHandle(FSession);
  inherited;
end;

procedure WinHTTPSecurityErrorCallback(hInternet: HINTERNET; dwContext: DWORD_PTR;
  dwInternetStatus: DWORD; lpvStatusInformation: pointer;
  dwStatusInformationLength: DWORD); stdcall;
begin
  raise EWinHTTP.CreateFmt('WinHTTP security error. Status %d, statusInfo: %d',
    [dwInternetStatus, pdword(lpvStatusInformation)^]);
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
  if dwContext = 0 then Exit;
  cContext := THttpContext(dwContext);

  {$ifdef debug}
  WriteLog(lkDebug, 'Request Callback %3d %d $%0.8X %s', [cContext.FDebugID,
      cContext.FCloseCount,
      dwInternetStatus, GetCallbackName(dwInternetStatus)]);
  {$endif}

  if cContext.FCloseCount > 0 then
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
//  WaitForSingleObject()
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
                              WINHTTP_FLAG_ASYNC); // 全部采用异步模式

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

//    pCallback := WinHttpAPI.SetStatusCallback(FSession, WinHTTPSecurityErrorCallback,
//                                    WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, 0);
//    if CallbackRes = WINHTTP_INVALID_STATUS_CALLBACK then
//      RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;

  fConnection := WinHttpAPI.Connect(FSession, PChar(FParams.Server), FParams.Port, 0);
  if fConnection=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
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
  writeln(format('THttpContext %3d - %d CloseRequst', [FDebugID, FCloseCount+1]));
{$endif}

  inc(FCloseCount);
  if FCloseCount > 1 then
    Exit;
  if Assigned(FRequestHandle) then
  begin
    cReqHandle := FRequestHandle;
    FRequestHandle := nil;

    WinHttpAPI.SetStatusCallback(cReqHandle, nil, 0, 0);
    WinHttpAPI.CloseHandle(cReqHandle);
    FTickCount := GetTickCount - FTickCount;
  end;
  DoResponseData;
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
  if Assigned(FOwner) and (FOwner is THttpConnection) then
    THttpConnection(FOwner).FContextList.Add(Self);

  {$ifdef Debug}
    Writeln(format('THttpContext %3d Create', [_MaxDebugID]));
  {$endif}

end;

destructor THttpContext.Destroy;
begin
  {$ifdef Debug}
    Writeln(format('THttpContext %3d Destroy', [_MaxDebugID]));
  {$endif}

  if Assigned(FOwner) and (FOwner is THttpConnection) then
    THttpConnection(FOwner).FContextList.Remove(Self);

  if Assigned(FRequestHandle) then
  begin
    WinHttpAPI.SetStatusCallback(FRequestHandle, nil, 0, 0);
    WinHttpAPI.CloseHandle(FRequestHandle);
  end;

  if Assigned(FResponseTmpData) then
    FResponseTmpData.Free;

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
  // 对于没有压缩的数据直接放到目标流中
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

  WriteLog(lkDebug, 'PushBuffer %x  size: %d/%d', [integer(self), FResponseReadSize, FOutDataLength]);
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
var
  iBytes: DWORD;
begin
  if FBufferSize = 0 then
    InitBuff;

  Result := WinHttpAPI.ReadData(FRequestHandle, @PByteArray(FBuffer)[0], FBufferSize, nil);
  if not Result then
    WriteLastRequestError;
end;

function THttpContext.Request: Integer;
begin
  Result := integer(DoRequest);
  if Result <> S_OK then
  begin
    WriteLastError;
    CloseRequst;
  end;
end;

procedure THttpContext.ConvertRequestBuffer;
var
  cData: TMemoryStream;
  cSource :TStream;
  iReadSize: Integer;
begin
  // 转换接收的缓存数据
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
	// 准备接收请求响应
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
      WriteLog(lkDebug, UTF8ToString(InternalGetInfo(WINHTTP_QUERY_RAW_HEADERS_CRLF)));
      FEncoding := InternalGetInfo(WINHTTP_QUERY_CONTENT_ENCODING);
      //FAcceptEncoding := InternalGetInfo(WINHTTP_QUERY_ACCEPT_ENCODING);
      FOutDataLength := InternalGetInfo32(WINHTTP_QUERY_CONTENT_LENGTH);
      //FContentType := InternalGetInfo(WINHTTP_QUERY_CONTENT_TYPE);

      // 读取数据
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

  //WriteLog(lkDebug, 'readheader %x  size: %d', [integer(self), FOutDataLength]);
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
    // iSize = 0 数据接收完成
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
    FDatas.FinishRequest(FCode);
    if Assigned(FOnFinshed) then
      FOnFinshed(Self);

    FDatas.DecRef;
    FDatas := nil;
  end;
end;

function THttpContext.GetOutDataLength: DWORD;
begin
  Result := FOutDataLength;
  if Result = 0 then
    Result := FResponseReadSize;
end;

function THttpContext.IsCancel: Boolean;
begin
  Result := (FCloseCount > 0) or
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
  assert(FDatas = nil, 'SetDatas 有多处赋值请求数据设置 ');

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
  bSended := False;
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


constructor TWorkThread.Create(AContext: THttpContext; ACall: TResponseDataFun);
begin
  inherited Create(True);
  FWaitCount := 0;
  FContext := AContext;
  FCallback := ACall;
  FLoading := True;
end;

procedure TWorkThread.DoCallbackFun;
begin
end;

{ TWorkThread }

procedure TWorkThread.Execute;
var
  res: Cardinal;
  iCheckRequestCnt: Integer;
begin
  inherited;
  res := FContext.Request;

  if res = S_OK then
  begin
    iCheckRequestCnt := 10;
    Sleep(20);
    while FLoading and (FWaitCount < 120000) do
    begin
      inc(FWaitCount, 20);
      Sleep(20);
    end;
    if FWaitCount > 60000 then
      LogWriter.Add(lkWarn, '数据等待请求超时 ' + inttostr(FWaitCount));
  end;
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


initialization
  Mutex :=TMutex.Create;
  FResponseList := TList.Create;

finalization
  FResponseList.Free;
  Mutex.Free;
end.


