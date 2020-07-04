unit uLogWriter;


interface

type
  TLogWriteKind = (lkErr, lkWarn, lkHint, lkTick, lkDebug);
  TLogWriteOptions = set of (lwoTimestamp);
  LogWriter = class
  public
    class procedure Add(AKind:TLogWriteKind; const s: string; AOpts: TLogWriteOptions = [lwoTimestamp]);overload; static;
    class procedure AddFmt(AKind:TLogWriteKind; const s: string; const args: array of const; AOpts: TLogWriteOptions = [lwoTimestamp]); static;
    class procedure Add(const s: string; AOpts: TLogWriteOptions = [lwoTimestamp]);overload; static;
    class procedure Add(const fmt: string; const args: array of const; AOpts:TLogWriteOptions = [lwoTimestamp]); overload; static;
    class procedure Add(const arrs: array of string; AOpts: TLogWriteOptions = [lwoTimestamp]);overload; static;

    class procedure BeginTick;
    class procedure EndTick(const AName: string; const AMsg: string = '');
    class procedure Err(const AName: string; const AMsg: string = ''); overload;
    class procedure Err(const AName: string; const afmt: string; const args: array of const); overload;
    class procedure Msg(const s: string = ''); overload;
    class procedure Msg(const afmt: string; const args: array of const); overload;
    class procedure Warn(const s: string = ''); overload;
    class procedure Warn(const afmt: string; const args: array of const); overload;
  end;

var
  gListOpenTimeCount: cardinal;
  gLogMsgLevel: Integer = {$ifdef Debug} ord(lkDebug) {$else} ord(lkWarn) {$endif};
const
  CONSTNAMES_Kind: array [TLogWriteKind] of string = ('ERROR', 'WARN', 'HINT', 'TICK', 'DEBUG');

implementation
uses
  SysUtils, StrUtils, Windows, untCommFuns;

var
  gTicks: array [0..100] of Cardinal;
  gTickCnt: Integer = 0;
  gWriteCnt: integer = 0;
  cs: TMutex = nil;
  LogTag: string = '';
  LogFileName: string = '';



function GetUserPath: string;
begin
   Result := ExtractFilePath(ParamStr(0));
end;

function GetLogFileName(ADate: TDateTime): string;
var
  sPath: string;
  sTag :string;
begin
  sTag := FormatDateTime('yyyymmdd', ADate);
  if sTag <> LogTag then
  begin
    LogTag := sTag;
    sPath := GetUserPath + 'log\' + FormatDateTime('yyyymm', ADate);
    LogFileName := sPath + '\' + FormatDateTime('yyyymmdd', ADate) + '.log';
    if not DirectoryExists(sPath) then
      if not ForceDirectories(sPath) then
        LogFileName := '';
  end;

  Result := LogFileName
end;

function GetTimestampStr: string;
begin
  Result := FormatDateTime('hh:mm:ss.zzz', Now);
end;

procedure writeLog(const s: string);
{$ifdef LogWriter}
var
  sFileName: string;
  f: TextFile;
{$endif}
begin
  {$ifdef LogWriter}
  sFileName := GetLogFileName(now);
  if sFileName = '' then
    Exit;

  cs.Enter;
  try
    AssignFile(f, sFileName);
    try
      if FileExists(sFileName) then
        Append(f)
      else Rewrite(f);

      if gWriteCnt = 0 then
      begin
        Writeln(f, '');
        Writeln(f, '');
        Writeln(f, FormatDateTime('yyyy-mm-dd hh:mm:ss.zzz', Now));
        Writeln(f, '=============================');
      end;

      Writeln(f, s);
      inc(gWriteCnt);

    finally
      CloseFile(f);
    end;
  finally
    cs.Leave;
  end;
  {$endif}
end;

class procedure LogWriter.Add(const s: string; AOpts: TLogWriteOptions =
    [lwoTimestamp]);
begin
  Add(lkHint, s, AOpts);
end;

class procedure LogWriter.Add(const fmt: string; const args: array of const;
    AOpts: TLogWriteOptions = [lwoTimestamp]);
begin
  Add(Format(fmt, args), AOpts);
end;

class procedure LogWriter.Add(const arrs: array of string;
  AOpts: TLogWriteOptions);
var
  cStr: TStringBuilder;
  I: Integer;
begin
  cStr := TStringBuilder.Create;
  try
    for I := 0 to High(arrs) do
    begin
      cStr.Append(#9);
      cStr.Append(arrs[i]);
      cStr.AppendLine;
    end;

    add(cStr.ToString, AOpts);
  finally
    cStr.Free;
  end;
end;

class procedure LogWriter.BeginTick;
begin
  gTicks[gTickCnt] := GetTickCount;
  inc(gTickCnt);
end;

class procedure LogWriter.EndTick(const AName: string; const AMsg: string = '');
var
  cStr: TStringBuilder;
begin
  Dec(gTickCnt);
  assert(gTickCnt >=0);
  if gTickCnt < 0 then
    gTickCnt := 0;

  // level 2 �����ϲ���¼
  if (gTickCnt > 0) or not (gLogMsgLevel > Ord(lkTick)) then
    Exit;



  cStr := TStringBuilder.Create;
  try
    cStr.Append('#Tick:');
    cStr.AppendFormat('%5d ', [GetTickCount - gTicks[gTickCnt]]);
    gListOpenTimeCount := gListOpenTimeCount + (GetTickCount - gTicks[gTickCnt]);
    cStr.Append(' ', gTickCnt * 4);
    cStr.Append('Level:');
    cStr.Append(gTickCnt+1);
    cStr.Append(' ');
    cStr.Append(AName);
    if AMsg <> '' then
    begin
      cStr.AppendLine;
      cStr.Append(AMsg);
      cStr.Replace(#13#10, #13#10#9);
      cStr.AppendLine;
    end;

    writeLog(cStr.ToString);
  finally
    cStr.Free;
  end;
end;

class procedure LogWriter.Err(const AName, afmt: string;
  const args: array of const);
begin
  Err(AName, Format(afmt, args));
end;

class procedure LogWriter.Err(const AName, AMsg: string);
var
  cStr: TStringBuilder;
begin
  cStr := TStringBuilder.Create;
  try
    cStr.Append('#');
    cStr.Append(CONSTNAMES_Kind[lkErr]);
    cStr.Append(':  ');
    cStr.Append(AName);
    if AMsg <> '' then
    begin
      cStr.AppendLine;
      cStr.Append(AMsg);
      cStr.Replace(#13#10, #13#10#9);
      cStr.AppendLine;
    end;

    writeLog(cStr.ToString);
  finally
    cStr.Free;
  end;

end;

class procedure LogWriter.Msg(const afmt: string; const args: array of const);
begin
  msg(format(afmt, args));
end;

class procedure LogWriter.Msg(const s: string);
begin
  Add(lkHint, s);
end;

class procedure LogWriter.Warn(const s: string = '');
begin
  Add(lkWarn, s);
end;
class procedure LogWriter.Warn(const afmt: string; const args: array of const);
begin
  Warn(format(afmt, args));
end;


class procedure LogWriter.Add(AKind: TLogWriteKind; const s: string;
  AOpts: TLogWriteOptions);
begin
  if ord(AKind) <= gLogMsgLevel then
  begin
    if lwoTimestamp in AOpts  then
      writelog(format('#%s: %s %s', [CONSTNAMES_Kind[AKind], GetTimestampStr, s]))
    else
      writelog(format('#%s: %s', [CONSTNAMES_Kind[AKind], s]))
  end;

end;

class procedure LogWriter.AddFmt(AKind:TLogWriteKind; const s: string;
    const args: array of const; AOpts: TLogWriteOptions);
begin
  Add(AKind, format(s, args), AOpts);
end;

initialization
  {$ifdef LogWriter}
  cs := TMutex.Create;
  {$endif}

finalization
  {$ifdef LogWriter}
  cs.Free;
  {$endif}
end.
