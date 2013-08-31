unit uSelectSource;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PlatformDefaultStyleActnCtrls, ActnMan, ActnList, ActnCtrls,
  ToolWin, ComCtrls, RibbonSilverStyleActnCtrls, Grids;

type
  TfmSelectSource = class(TForm)
    alSelectSource: TActionList;
    acmgrSelectSource: TActionManager;
    acOk: TAction;
    acCancel: TAction;
    acAdd: TAction;
    acRemove: TAction;
    acEdit: TAction;
    cbTop: TCoolBar;
    cbActions: TCoolBar;
    actbTop: TActionToolBar;
    actbActions: TActionToolBar;
    sgSource: TStringGrid;
    odSelectSource: TFileOpenDialog;
    procedure acOkExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acRemoveExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure alSelectSourceUpdate(Action: TBasicAction;
      var Handled: Boolean);
  private
    FList: TStringList;
    procedure AddSource(const FolderName: String);
    procedure SetSourceList(const Value: String);
    function GetSourceList: String;
    procedure UpdateView;
  public
    property SourceList: String read GetSourceList write SetSourceList;
  end;

  function SelectSource(const SourceList: String): String;

var
  fmSelectSource: TfmSelectSource;

implementation

uses
  uShareData;

{$R *.dfm}

function SelectSource(const SourceList: String): String;
var
  F: TfmSelectSource;
begin
  Result := SourceList;

  Application.CreateForm(TfmSelectSource, F);
  try
    F.SourceList := SourceList;
    if F.ShowModal = mrOk then
      Result := F.SourceList;
  finally
    F.Release;
  end;
end;

procedure TfmSelectSource.acAddExecute(Sender: TObject);
begin
  if odSelectSource.Execute then
    AddSource(odSelectSource.FileName);
end;

procedure TfmSelectSource.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmSelectSource.acEditExecute(Sender: TObject);
var
  CurSource: String;
begin
  if FList.Count = 0 then Exit;
  
  CurSource := '';
  if sgSource.Row >= 0 then
  begin
    if FList.Count > sgSource.Row then
    begin
      CurSource := FList.Strings[sgSource.Row];
      odSelectSource.FileName := CurSource;
    end;

    if odSelectSource.Execute then
    begin
      FList.Strings[sgSource.Row] := odSelectSource.FileName;
      UpdateView;
    end;
  end;
end;

procedure TfmSelectSource.acOkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmSelectSource.acRemoveExecute(Sender: TObject);
begin
  if sgSource.Row >= 0 then
  begin
    if FList.Count > 0 then
      FList.Delete(sgSource.Row);
    UpdateView;
  end;
end;

procedure TfmSelectSource.AddSource(const FolderName: String);
begin
  if FList.IndexOf(FolderName) < 0 then
  begin
    FList.Add(FolderName);
    UpdateView;
  end;
end;

procedure TfmSelectSource.alSelectSourceUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acEdit.Enabled := FList.Count > 0;
  acRemove.Enabled := acEdit.Enabled;
end;

procedure TfmSelectSource.FormCreate(Sender: TObject);
begin
  actbTop.ParentBackground := True;
  actbActions.ParentBackground := True;

  FList := TStringList.Create;
  FList.Duplicates := dupIgnore;
  FList.Delimiter := ';';
  FList.StrictDelimiter := True;
end;

procedure TfmSelectSource.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FList);
end;

function TfmSelectSource.GetSourceList: String;
begin
  Result := FList.DelimitedText;
end;

procedure TfmSelectSource.SetSourceList(const Value: String);
begin
  FList.DelimitedText := Value;
  UpdateView;
end;

procedure TfmSelectSource.UpdateView;
var
  I: Integer;
begin
  sgSource.RowCount := 1;
  sgSource.Cells[0, 0] := '';

  sgSource.RowCount := FList.Count;

  for I := 0 to FList.Count - 1 do
    sgSource.Cells[0, I] := FList.Strings[I];
end;

end.
