program Spider;

uses
  Forms,
  uMain in 'uMain.pas' {MainForm},
  Debuger in 'Debuger.pas',
  DebugInfo in 'DebugInfo.pas',
  ClassUtils in 'ClassUtils.pas',
  DelphiDebugInfo in 'DelphiDebugInfo.pas' {$R *.res},
  EvaluateTypes in 'EvaluateTypes.pas',
  EvaluateProcs in 'EvaluateProcs.pas',
  EvaluateConsts in 'EvaluateConsts.pas',
  Evaluator in 'Evaluator.pas',
  uProcessList in 'uProcessList.pas' {frmProcessList},
  JclTD32Ex in 'JclTD32Ex.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
