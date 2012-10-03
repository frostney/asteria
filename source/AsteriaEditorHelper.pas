unit AsteriaEditorHelper;

interface

uses
  Classes, SysUtils,
  AsteriaGlobal,
  BESEN, BESENValue;

type
  TasEditorHelper = class
  public
    procedure PrintLn(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

uses
  uMain;

procedure TasEditorHelper.PrintLn(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
begin
  frmMain.memOutput.Lines.Add(TBESEN(BesenInst).ToStr(Arguments^[0]^));
end;

end.

