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
  JclTD32Ex in 'JclTD32Ex.pas',
  DebugHook in 'DebugHook.pas',
  DbgHookTypes in 'DbgHookTypes.pas',
  DebugerTypes in 'DebugerTypes.pas',
  uActionController in 'uActionController.pas',
  uDebugerThread in 'uDebugerThread.pas',
  uProjectOptions in 'uProjectOptions.pas' {fmProjectOptions},
  uSpiderOptions in 'uSpiderOptions.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Spider';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.