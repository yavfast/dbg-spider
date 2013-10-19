unit uProcessList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, ActnList, StdCtrls, Buttons,
  PlatformDefaultStyleActnCtrls, ActnMan, ActnCtrls, ToolWin, ComCtrls,
  RibbonSilverStyleActnCtrls, System.Actions;

type
  TfrmProcessList = class(TForm)
    sgProcessList: TStringGrid;
    AL: TActionList;
    acOk: TAction;
    acCancel: TAction;
    acRefresh: TAction;
    cbTop: TCoolBar;
    actbTop: TActionToolBar;
    pActions: TPanel;
    btnAttach: TBitBtn;
    btnCancel: TBitBtn;
    acmgr1: TActionManager;
    procedure acRefreshExecute(Sender: TObject);
    procedure acOkExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetSelProcessID: DWORD;
    function GetSelProcessName: String;
  end;

var
  frmProcessList: TfrmProcessList;

implementation

uses PsAPI, uShareData;

procedure GetProcessList(List: TStringList);
var
  PIDArray: array [0..1023] of DWORD;
  cb: DWORD;
  I: Integer;
  ProcCount: Integer;
  hMod: HMODULE;
  hProcess: THandle;
  ProcessID: DWORD;
  ModuleName: array [0..300] of Char;
begin
  if List = nil then Exit;

  if EnumProcesses(@PIDArray, SizeOf(PIDArray), cb) then
  begin
    ProcCount := cb div SizeOf(DWORD);
    for I := 0 to ProcCount - 1 do
    begin
      ProcessID := PIDArray[I];
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
      if (hProcess <> 0) then
      begin
        EnumProcessModules(hProcess, @hMod, SizeOf(hMod), cb);
        GetModuleFilenameEx(hProcess, hMod, ModuleName, SizeOf(ModuleName));
        List.AddObject(ModuleName, TObject(ProcessID));
        CloseHandle(hProcess);
      end;
    end;
  end;
end;

{$R *.dfm}

procedure TfrmProcessList.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmProcessList.acOkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmProcessList.acRefreshExecute(Sender: TObject);
var
  PL: TStringList;
  I: Integer;
begin
  PL := TStringList.Create;
  try
    GetProcessList(PL);

    sgProcessList.RowCount := PL.Count + 1;
    sgProcessList.Cells[0, 0] := 'Process name';

    for I := 0 to PL.Count - 1 do
    begin
      sgProcessList.Cells[0, I + 1] := PL.Strings[I];
      sgProcessList.Objects[0, I + 1] := PL.Objects[I];
    end;
  finally
    FreeAndNil(PL);
  end;
end;

procedure TfrmProcessList.FormCreate(Sender: TObject);
begin
  actbTop.ParentBackground := True;
end;

procedure TfrmProcessList.FormShow(Sender: TObject);
begin
  acRefresh.Execute;
end;

function TfrmProcessList.GetSelProcessID: DWORD;
begin
  Result := DWORD(sgProcessList.Objects[sgProcessList.Col, sgProcessList.Row]);
end;

function TfrmProcessList.GetSelProcessName: String;
begin
  Result := sgProcessList.Cells[sgProcessList.Col, sgProcessList.Row];
end;

end.
