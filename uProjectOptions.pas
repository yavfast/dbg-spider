unit uProjectOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, PlatformDefaultStyleActnCtrls, ActnList, ActnMan,
  ComCtrls, ActnCtrls, ToolWin, StdCtrls, Buttons, ImgList,
  RibbonSilverStyleActnCtrls, System.Actions;

type
  TOpenType = (otNew, otEdit, otSaveAs);

  TfmProjectOptions = class(TForm)
    alProjectOpt: TActionList;
    acSave: TAction;
    acCancel: TAction;
    acOpenApplication: TAction;
    acSaveProjectName: TAction;
    acSaveProjectStorage: TAction;
    odApplication: TFileOpenDialog;
    sdProjectName: TFileSaveDialog;
    acDelphiSource: TAction;
    acProjectSource: TAction;
    pcProjectOpt: TPageControl;
    tsProject: TTabSheet;
    lbeApplication: TLabeledEdit;
    lbeProjectName: TLabeledEdit;
    lbeProjectStorage: TLabeledEdit;
    btnOpenApplication: TBitBtn;
    btnSaveProjectName: TBitBtn;
    btnSaveProjectStorage: TBitBtn;
    tsSources: TTabSheet;
    lbeProjectSource: TLabeledEdit;
    btnProjectSource: TBitBtn;
    lbeDelphiSource: TLabeledEdit;
    btnDelphiSource: TBitBtn;
    pActions: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    tsRunParams: TTabSheet;
    lbeParameters: TLabeledEdit;
    lbeWorkDir: TLabeledEdit;
    btnSelWorkDir: TBitBtn;
    acSelWorkDir: TAction;
    odSelectWorkDir: TFileOpenDialog;
    procedure acSaveExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acOpenApplicationExecute(Sender: TObject);
    procedure acSaveProjectNameExecute(Sender: TObject);
    procedure acSaveProjectStorageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acDelphiSourceExecute(Sender: TObject);
    procedure acProjectSourceExecute(Sender: TObject);
    procedure acSelWorkDirExecute(Sender: TObject);
  private
    FOpenType: TOpenType;
    function GetApplicationName: String;
    function GetProjectName: String;
    function GetProjectStorage: String;
    function GetDelphiSource: String;
    function GetProjectSource: String;
    function GetRunParams: String;
    function GetWorkingDirectory: String;

    procedure SetApplicationName(const Value: String);
    procedure SetDelphiSource(const Value: String);
    procedure SetProjectName(const Value: String);
    procedure SetProjectSource(const Value: String);
    procedure SetProjectStorage(const Value: String);
    procedure SetRunParams(const Value: String);
    procedure SetWorkingDirectory(const Value: String);
  public
    property OpenType: TOpenType read FOpenType write FOpenType;
    property ApplicationName: String read GetApplicationName write SetApplicationName;
    property ProjectName: String read GetProjectName write SetProjectName;
    property ProjectStorage: String read GetProjectStorage write SetProjectStorage;
    property ProjectSource: String read GetProjectSource write SetProjectSource;
    property DelphiSource: String read GetDelphiSource write SetDelphiSource;
    property RunParams: String read GetRunParams write SetRunParams;
    property WorkingDirectory: String read GetWorkingDirectory write SetWorkingDirectory;
  end;

  function OpenProjectOptions(const OpenType: TOpenType): Integer;

var
  fmProjectOptions: TfmProjectOptions;

implementation

uses
  IOUtils, uShareData, uSelectSource, uActionController;

{$R *.dfm}

function OpenProjectOptions(const OpenType: TOpenType): Integer;
var
  F: TfmProjectOptions;
begin
  Application.CreateForm(TfmProjectOptions, F);
  try
    F.OpenType := OpenType;

    if OpenType in [otEdit, otSaveAs] then
    begin
      F.ProjectName := gvProjectOptions.ProjectName;
      F.ApplicationName := gvProjectOptions.ApplicationName;
      F.ProjectStorage := gvProjectOptions.ProjectStorage;
      F.ProjectSource := gvProjectOptions.ProjectSource;
      F.DelphiSource := gvProjectOptions.DelphiSource;
      F.RunParams := gvProjectOptions.RunParams;
      F.WorkingDirectory := gvProjectOptions.WorkingDirectory;
    end;

    if OpenType = otSaveAs then
      F.ProjectName := Format('%s_copy%s', [
        ChangeFileExt(gvProjectOptions.ProjectName, ''),
        ExtractFileExt(gvProjectOptions.ProjectName)
      ]);

    Result := F.ShowModal;
    if Result = mrOk then
    begin
      ChangeFileExt(F.ProjectName, '.spider');
      gvProjectOptions.Open(F.ProjectName);
      gvProjectOptions.BeginUpdate;
      try
        gvProjectOptions.ApplicationName := F.ApplicationName;
        gvProjectOptions.ProjectStorage := F.ProjectStorage;
        gvProjectOptions.ProjectSource := F.ProjectSource;
        gvProjectOptions.DelphiSource := F.DelphiSource;
        gvProjectOptions.RunParams := F.RunParams;
        gvProjectOptions.WorkingDirectory := F.WorkingDirectory;
      finally
        gvProjectOptions.EndUpdate;
      end;

      if OpenType in [otNew, otSaveAs] then
      begin
        gvProjectOptions.Clear;
        _AC.DoAction(acSetProjectName, [F.ProjectName, OpenType]);
      end;

      if OpenType = otEdit then
        _AC.DoAction(acChangeProjectSettings, []);
    end;
  finally
    F.Release;
  end;
end;

procedure TfmProjectOptions.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmProjectOptions.acDelphiSourceExecute(Sender: TObject);
begin
  lbeDelphiSource.Text := SelectSource(lbeDelphiSource.Text);
end;

procedure TfmProjectOptions.acOpenApplicationExecute(Sender: TObject);
begin
  if lbeApplication.Text <> '' then
    odApplication.FileName := lbeApplication.Text;

  if odApplication.Execute then
  begin
    // Project settings
    lbeApplication.Text := odApplication.FileName;

    if lbeProjectName.Text = '' then
      lbeProjectName.Text := ChangeFileExt(lbeApplication.Text, '.spider');

    if lbeProjectStorage.Text = '' then
      lbeProjectStorage.Text := ExtractFilePath(lbeProjectName.Text) + '_spider_storage';

    // Source settings
    if lbeProjectSource.Text = '' then
      lbeProjectSource.Text := TProjectOptions.GetDefProjectSource(odApplication.FileName);

    if lbeDelphiSource.Text = '' then
      lbeDelphiSource.Text := TProjectOptions.GetDefDelphiSource;

    // Run parameters
    if lbeWorkDir.Text = '' then
      lbeWorkDir.Text := ExtractFilePath(lbeProjectName.Text);
  end;
end;

procedure TfmProjectOptions.acProjectSourceExecute(Sender: TObject);
begin
  lbeProjectSource.Text := SelectSource(lbeProjectSource.Text);
end;

procedure TfmProjectOptions.acSaveExecute(Sender: TObject);
begin
  if not FileExists(ApplicationName) then
  begin
    pcProjectOpt.ActivePage := tsProject;
    ActiveControl := lbeApplication;
    ShowMessageFmt('Application "%s" not found', [ApplicationName]);
    Exit;
  end;

  if not TFile.Exists(ProjectName) then
  begin
    try
      TFile.Create(ProjectName).Free;
      TFile.Delete(ProjectName);
    except
      on E: Exception do
      begin
        pcProjectOpt.ActivePage := tsProject;
        ActiveControl := lbeProjectName;
        ShowMessageFmt('%s', [E.Message]);
        Exit;
      end;
    end;
  end;

  if not TDirectory.Exists(ProjectStorage) then
  begin
    try
      TDirectory.CreateDirectory(ProjectStorage);
      TDirectory.Delete(ProjectStorage);
    except
      on E: Exception do
      begin
        pcProjectOpt.ActivePage := tsProject;
        ActiveControl := lbeProjectName;
        ShowMessageFmt('%s', [E.Message]);
        Exit;
      end;
    end;
  end;

  if (WorkingDirectory <> '') and not TDirectory.Exists(WorkingDirectory) then
  begin
    pcProjectOpt.ActivePage := tsRunParams;
    ActiveControl := lbeWorkDir;
    ShowMessageFmt('Working directory "%s" not exists', [WorkingDirectory]);
    Exit;
  end;

  ModalResult := mrOk;
end;

procedure TfmProjectOptions.acSaveProjectNameExecute(Sender: TObject);
begin
  if lbeProjectName.Text <> '' then
    sdProjectName.FileName := lbeProjectName.Text;

  if sdProjectName.Execute then
  begin
    lbeProjectName.Text := sdProjectName.FileName;

    lbeProjectStorage.Text := ExtractFilePath(lbeProjectName.Text) + '_spider_storage';
  end;
end;

procedure TfmProjectOptions.acSaveProjectStorageExecute(Sender: TObject);
begin
  //
end;

procedure TfmProjectOptions.acSelWorkDirExecute(Sender: TObject);
begin
  odSelectWorkDir.FileName := lbeWorkDir.Text;

  if odSelectWorkDir.Execute then
    lbeWorkDir.Text := ExcludeTrailingPathDelimiter(Trim(odSelectWorkDir.FileName));
end;

procedure TfmProjectOptions.FormCreate(Sender: TObject);
begin
  FOpenType := otNew;

  pcProjectOpt.ActivePage := tsProject;
  ActiveControl := lbeApplication;
end;

procedure TfmProjectOptions.FormShow(Sender: TObject);
begin
  if FOpenType = otNew then
    acOpenApplication.Execute;
end;

function TfmProjectOptions.GetApplicationName: String;
begin
  Result := lbeApplication.Text;
end;

function TfmProjectOptions.GetDelphiSource: String;
begin
  Result := lbeDelphiSource.Text;
end;

function TfmProjectOptions.GetProjectName: String;
begin
  Result := lbeProjectName.Text;
end;

function TfmProjectOptions.GetProjectSource: String;
begin
  Result := lbeProjectSource.Text;
end;

function TfmProjectOptions.GetProjectStorage: String;
begin
  Result := lbeProjectStorage.Text;
end;

function TfmProjectOptions.GetRunParams: String;
begin
  Result := lbeParameters.Text;
end;

function TfmProjectOptions.GetWorkingDirectory: String;
begin
  Result := ExcludeTrailingPathDelimiter(Trim(lbeWorkDir.Text));
end;

procedure TfmProjectOptions.SetApplicationName(const Value: String);
begin
  lbeApplication.Text := Value;
end;

procedure TfmProjectOptions.SetDelphiSource(const Value: String);
begin
  lbeDelphiSource.Text := Value;
end;

procedure TfmProjectOptions.SetProjectName(const Value: String);
begin
  lbeProjectName.Text := Value;
end;

procedure TfmProjectOptions.SetProjectSource(const Value: String);
begin
  lbeProjectSource.Text := Value;
end;

procedure TfmProjectOptions.SetProjectStorage(const Value: String);
begin
  lbeProjectStorage.Text := Value;
end;

procedure TfmProjectOptions.SetRunParams(const Value: String);
begin
  lbeParameters.Text := Value;
end;

procedure TfmProjectOptions.SetWorkingDirectory(const Value: String);
begin
  lbeWorkDir.Text := Value;
end;

end.
