unit ufrmTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, StdActns, ActnList, StdCtrls, uWinHttps;

type
  TForm5 = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Panel2: TPanel;
    memReturn: TMemo;
    memSQL: TMemo;
    Splitter1: TSplitter;
    edtServer: TEdit;
    edtToken: TEdit;
    GetToken: TButton;
    procedure GetTokenClick(Sender: TObject);
  private
    FHttpConnect: THttpConnection;
    procedure AddMsg(const s: string); overload;
    procedure AddMsg(const fmt: string; const args: array of const); overload;
  protected
    procedure BuildHttpConnect;
    procedure DoOnGetTokenValue(Sender: TObject);
  public

    function EncodeString64(const Input: string): string;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  JsonDataObjects, untCommFuns, EncdDecd;

{$R *.dfm}

procedure TForm5.BuildHttpConnect;
var
  rParams: TConnectParams;
  sConfig: string;
begin
  if Assigned(FHttpConnect) then
    Exit;

  sConfig := ReadSegmentValue(edtServer.Text, 0, ';');
  rParams.Server := ReadSegmentValue(sConfig, 0, ':');
  rParams.Port := StrToIntDef(ReadSegmentValue(sConfig, 1, ':'), 0);
  rParams.IsHttps := True;
  rParams.ProxyName := '';
  rParams.ProxyByPass := '';
  rParams.ConnectionTimeOut := 0;
  rParams.SendTimeout := 0;
  rParams.ReceiveTimeout := 0;

  FHttpConnect := THttpConnection(GetHttpConnecitonClass.NewInstance).Create(Self, rParams);
  // TODO -cMM: TForm5.BuildHttpConnect default body inserted
end;

destructor TForm5.Destroy;
begin
  if assigned(FHttpConnect) then
    FHttpConnect.Free;
  inherited;
end;

procedure TForm5.AddMsg(const s: string);
begin
  memReturn.Lines.Add(s);
end;

procedure TForm5.AddMsg(const fmt: string; const args: array of const);
begin
  AddMsg(format(fmt, args));
end;

procedure TForm5.DoOnGetTokenValue(Sender: TObject);
var
  cContext: THttpContext;
  cJons: TJsonObject;
  sData: string;
begin
  cContext := THttpContext(Sender);
  AddMsg('Get token %d', [cContext.Code]);
  sData := '';
  if cContext.Code = 200 then
  begin
    cJons := TJsonObject.Create;
    try
      cContext.OutData.Position := 0;
      cJons.LoadFromStream(cContext.OutData);
      edtToken.Text := cJons.s['data'];
    finally
      cJons.Free;
    end;
  end;
end;

function TForm5.EncodeString64(const Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input, TEncoding.Unicode);
  try
    OutStr := TStringStream.Create('', TEncoding.ASCII);
    try
      EncodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

procedure TForm5.GetTokenClick(Sender: TObject);
var
  cContext: THttpContext;
  pData: PChar;
  sAdd, sUser, sPwd: string;
  sParams: string;
begin
  pData := PChar(edtServer.Text);
  TryReadNextValue(pData, sAdd, ';');
  TryReadNextValue(pData, sUser, ';');
  TryReadNextValue(pData, sPwd, ';');

  sPwd := EncodeString64(Trim(sPwd));
  sParams := '{"loginUser":"'+Trim(sUser)+'","sourceId":"PC","fromIP":"","fromDevice":"","accessToken":"","passWord":"'+sPwd+'"}';

  BuildHttpConnect;

  cContext := FHttpConnect.BuildContext('/v1/Auth', 'POST');
  cContext.SetInData(sParams);
  cContext.OnFinshed := DoOnGetTokenValue;
  cContext.Request;
end;

end.
