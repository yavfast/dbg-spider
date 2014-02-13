unit uSourceViewFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  SynEdit, SynMemo, Vcl.StdCtrls;

type
  TSourceViewFrame = class(TFrame)
    synmSourceView: TSynMemo;
    eSrcFileName: TEdit;
  private
    FSourceFileName: String;
    procedure SetSourceFileName(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Clear;

    procedure GotoLine(const LineNo: Integer; const Alignment: TVerticalAlignment);
    procedure SelectLine(const LineNo: Integer);

    property SourceFileName: String read FSourceFileName write SetSourceFileName;
  end;

implementation

uses
  SynEditTypes;

{$R *.dfm}

{ TSourceViewFrame }

procedure TSourceViewFrame.BeginUpdate;
begin
  synmSourceView.BeginUpdate;
end;

procedure TSourceViewFrame.Clear;
begin
  FSourceFileName := '';

  synmSourceView.Clear;
  eSrcFileName.Text := '';
end;

constructor TSourceViewFrame.Create(AOwner: TComponent);
begin
  inherited;

  FSourceFileName := '';
end;

procedure TSourceViewFrame.EndUpdate;
begin
  synmSourceView.EndUpdate;
end;

procedure TSourceViewFrame.GotoLine(const LineNo: Integer; const Alignment: TVerticalAlignment);
begin
  if (LineNo > 0) and (LineNo < synmSourceView.Lines.Count) then
  begin
    case Alignment of
      taVerticalCenter:
        synmSourceView.GotoLineAndCenter(LineNo);
    else
      synmSourceView.TopLine := LineNo;
    end;
  end;
end;

procedure TSourceViewFrame.SelectLine(const LineNo: Integer);
begin
  if (LineNo > 0) and (LineNo < synmSourceView.Lines.Count) then
  begin
    synmSourceView.SetCaretAndSelection(
      BufferCoord(1, LineNo),
      BufferCoord(1, LineNo),
      BufferCoord(1, LineNo + 1)
    );
  end;
end;

procedure TSourceViewFrame.SetSourceFileName(const Value: String);
begin
  if FSourceFileName <> Value then
  begin
    FSourceFileName := Value;

    eSrcFileName.Text := FSourceFileName;

    if FileExists(FSourceFileName) then
      synmSourceView.Lines.LoadFromFile(FSourceFileName)
    else
      synmSourceView.Clear;
  end;
end;

end.
