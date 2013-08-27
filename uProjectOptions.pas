unit uProjectOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, PlatformDefaultStyleActnCtrls, ActnList, ActnMan,
  ComCtrls, ActnCtrls, ToolWin, StdCtrls, Buttons, ImgList,
  RibbonSilverStyleActnCtrls;

type
  TOpenType = (otNew, otEdit, otSaveAs);

  TfmProjectOptions = class(TForm)
    actmgrProjectOpt: TActionManager;
    alProjectOpt: TActionList;
    acSave: TAction;
    acCancel: TAction;
    ilProjectOpt: TImageList;
    acOpenApplication: TAction;
    acSaveProjectName: TAction;
    acSaveProjectStorage: TAction;
    pcBackground: TPageControl;
    ts1: TTabSheet;
    p1: TPanel;
    pcProjectOpt: TPageControl;
    tsProject: TTabSheet;
    lbeApplication: TLabeledEdit;
    lbeProjectName: TLabeledEdit;
    lbeProjectStorage: TLabeledEdit;
    btnOpenApplication: TBitBtn;
    btnSaveProjectName: TBitBtn;
    btnSaveProjectStorage: TBitBtn;
    tsSessions: TTabSheet;
    cbActions: TCoolBar;
    actbActions: TActionToolBar;
    odApplication: TFileOpenDialog;
    sdProjectName: TFileSaveDialog;
    procedure acSaveExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acOpenApplicationExecute(Sender: TObject);
    procedure acSaveProjectNameExecute(Sender: TObject);
    procedure acSaveProjectStorageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOpenType: TOpenType;
    function GetApplicationName: String;
    function GetProjectName: String;
    function GetProjectStorage: String;
  public
    property OpenType: TOpenType read FOpenType write FOpenType;
    property ApplicationName: String read GetApplicationName;
    property ProjectName: String read GetProjectName;
    property ProjectStorage: String read GetProjectStorage;
  end;

var
  fmProjectOptions: TfmProjectOptions;

implementation

uses
  IOUtils;

{$R *.dfm}

procedure TfmProjectOptions.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmProjectOptions.acOpenApplicationExecute(Sender: TObject);
begin
  if lbeApplication.Text <> '' then
    odApplication.FileName := lbeApplication.Text;

  if odApplication.Execute then
  begin
    lbeApplication.Text := odApplication.FileName;

    if lbeProjectName.Text = '' then
      lbeProjectName.Text := ChangeFileExt(lbeApplication.Text, '.spider');

    if lbeProjectStorage.Text = '' then
      lbeProjectStorage.Text := ExtractFilePath(lbeProjectName.Text) + '_spider_storage';
  end;
end;

procedure TfmProjectOptions.acSaveExecute(Sender: TObject);
begin
  if not FileExists(ApplicationName) then
  begin
    ActiveControl := lbeApplication;
    ShowMessageFmt('Application "%s" not found', [ApplicationName]);
    Exit;
  end;

  try
    TFile.Create(ProjectName).Free;
    TFile.Delete(ProjectName);
  except
    on E: Exception do
    begin
      ActiveControl := lbeProjectName;
      ShowMessageFmt('%s', [E.Message]);
      Exit;
    end;
  end;

  try
    TDirectory.CreateDirectory(ProjectStorage);
    TDirectory.Delete(ProjectStorage);
  except
    on E: Exception do
    begin
      ActiveControl := lbeProjectName;
      ShowMessageFmt('%s', [E.Message]);
      Exit;
    end;
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

procedure TfmProjectOptions.FormCreate(Sender: TObject);
begin
  FOpenType := otNew;
  ActiveControl := lbeApplication;

  actbActions.ParentBackground := True;
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

function TfmProjectOptions.GetProjectName: String;
begin
  Result := lbeProjectName.Text;
end;

function TfmProjectOptions.GetProjectStorage: String;
begin
  Result := lbeProjectStorage.Text;
end;

end.
