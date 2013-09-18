unit uShareData;

interface

uses
  SysUtils, Classes, ImgList, Controls, JvImageList, XPMan, ActnMan,
  ActnColorMaps, SynEditHighlighter, SynHighlighterPas, SynEditMiscClasses,
  SynEditRegexSearch, SynEditSearch, SynEditOptionsDialog;

type
  TdmShareData = class(TDataModule)
    ilActionsSmall: TImageList;
    imlMainSmall: TJvImageList;
    imlMain: TJvImageList;
    synPas1: TSynPasSyn;
    synRegexSearch1: TSynEditRegexSearch;
    synEditSearch1: TSynEditSearch;
    synEditOptDlg1: TSynEditOptionsDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmShareData: TdmShareData;

implementation

{$R *.dfm}

end.
