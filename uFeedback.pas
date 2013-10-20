unit uFeedback;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Buttons, System.Actions, Vcl.ActnList;

type
  TfrmFeedback = class(TForm)
    cbbType: TComboBoxEx;
    lbFeedbackType: TLabel;
    lbMessage: TLabel;
    mMessage: TMemo;
    btnSend: TBitBtn;
    AL: TActionList;
    acSend: TAction;
    lbEmail: TLabel;
    eEMail: TEdit;
    procedure acSendExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetFeedbackText: String;
    function GetFeedbackType: String;
    { Private declarations }
  public
    property FeedbackType: String read GetFeedbackType;
    property FeedbackText: String read GetFeedbackText;
  end;

var
  frmFeedback: TfrmFeedback;

implementation

uses
  uShareData;

{$R *.dfm}

procedure TfrmFeedback.acSendExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmFeedback.FormCreate(Sender: TObject);
begin
  cbbType.ItemIndex := 0;
  mMessage.Text := '';
end;

function TfrmFeedback.GetFeedbackText: String;
begin
  Result := Format('[%s] %s', [eEMail.Text, mMessage.Text]);
end;

function TfrmFeedback.GetFeedbackType: String;
begin
  Result := cbbType.ItemsEx.ComboItems[cbbType.ItemIndex].Caption;
end;

end.
