unit AsteriaEditorHelper;

interface

uses
  Classes, SysUtils,
  AsteriaGlobal,
  BESEN, BESENValue, BESENStringUtils;

type

  { TasEditorHelper }

  TasEditorHelper = class
  public
    procedure PrintLn(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure Require(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

uses
  uMain;

procedure TasEditorHelper.PrintLn(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
begin
  frmMain.memOutput.Lines.Add(TBESEN(BesenInst).ToStr(Arguments^[0]^));
end;

procedure TasEditorHelper.Require(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
var
  Filename: String;
  FilenameExt: String;
begin
  Filename := TBESEN(BesenInst).ToStr(Arguments^[0]^);
  FilenameExt := ExtractFileExt(Filename);

  if (FilenameExt = '.json') then
  begin
    ResultValue := BesenInst.Execute(BESENConvertToUTF8('(function() { return ' + BESENGetFileContent(FileName) + '})();'));
  end else
  begin
    if DirectoryExists(Filename) then
      Filename := Filename + DirectorySeparator + 'index.js';

    ResultValue := BesenInst.Execute(BESENConvertToUTF8('(function() { var exports = {}; ' + BESENGetFileContent(FileName) + ' return exports; })();'));
  end;

end;

end.

