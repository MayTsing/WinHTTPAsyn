program WinHttpDebug;

uses
  Forms,
  ufrmTestMain in 'ufrmTestMain.pas' {Form5},
  SynZip in '..\src\SynZip.pas',
  uWinHttps in '..\src\uWinHttps.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
