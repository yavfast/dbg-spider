program Spider;

uses
  Vcl.Themes,
  Vcl.Styles,
  Forms,
  uMain in 'uMain.pas' {MainForm},
  Debuger in 'Debuger.pas',
  DebugInfo in 'DebugInfo.pas',
  ClassUtils in 'ClassUtils.pas',
  DelphiDebugInfo in 'DelphiDebugInfo.pas',
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
  uSpiderOptions in 'uSpiderOptions.pas',
  uShareData in 'uShareData.pas' {dmShareData: TDataModule},
  uSelectSource in 'uSelectSource.pas' {fmSelectSource},
  WinAPIUtils in 'WinAPIUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Silver');
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Spider';
  Application.CreateForm(TdmShareData, dmShareData);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
