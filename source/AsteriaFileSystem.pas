unit AsteriaFileSystem;

interface

uses
  Classes, SysUtils,
  BESEN, BESENNativeObject, BESENValue, BESENStringUtils,
  AsteriaGlobal;

type

  { TasFileSystem }

  TasFileSystem = class
  public
    constructor Create;

    procedure FileExists(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure DirExists(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);

    procedure ReadFile(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure WriteFile(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);

    procedure Rename(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

  { TasPathModule }

  TasPathModule = class
  public
    constructor Create;

    procedure Join(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure Basename(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure Extname(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure Relative(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

{ TasPathModule }

constructor TasPathModule.Create;
begin

end;

procedure TasPathModule.Join(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
var
  i: Integer;
  NewDir: String;
begin
  for i := 0 to CountArguments do
  begin
    NewDir := TBESEN(BesenInst).ToStr(Arguments^[i]^) + DirectorySeparator;
  end;

  ResultValue := BESENStringValue(NewDir);
end;

procedure TasPathModule.Basename(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin
  ResultValue := BESENStringValue(SysUtils.ExtractFileName(TBESEN(BesenInst).ToStr(Arguments^[0]^)));
end;

procedure TasPathModule.Extname(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin
  ResultValue := BESENStringValue(SysUtils.ExtractFileExt(TBESEN(BesenInst).ToStr(Arguments^[0]^)));
end;

procedure TasPathModule.Relative(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin
  ResultValue := BESENStringValue(SysUtils.ExtractRelativepath(GetCurrentDir(), TBESEN(BesenInst).ToStr(Arguments^[0]^)));
end;

{ TasFileSystem }

constructor TasFileSystem.Create;
begin

end;

procedure TasFileSystem.FileExists(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin
  ResultValue := BESENBooleanValue(SysUtils.FileExists(TBESEN(BesenInst).ToStr(Arguments^[0]^)));
end;

procedure TasFileSystem.DirExists(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin
  ResultValue := BESENBooleanValue(DirectoryExists(TBESEN(BesenInst).ToStr(Arguments^[0]^)));
end;

procedure TasFileSystem.ReadFile(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
var
  Filename: String;
  StringList: TStringList;
begin
  Filename := TBESEN(BesenInst).ToStr(Arguments^[0]^);

  StringList := TStringList.Create;
  StringList.LoadFromFile(Filename);

  ResultValue := BESENStringValue(StringList.Text);

  StringList.Free;
end;

procedure TasFileSystem.WriteFile(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
var
  Filename: String;
  Content: String;
  StringList: TStringList;
begin
  Filename := TBESEN(BesenInst).ToStr(Arguments^[0]^);
  Content := TBESEN(BesenInst).ToStr(Arguments^[1]^);

  StringList := TStringList.Create;
  StringList.Text := Content;
  StringList.SaveToFile(Filename);

  StringList.Free;
end;

procedure TasFileSystem.Rename(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin
  SysUtils.RenameFile(TBESEN(BesenInst).ToStr(Arguments^[0]^), TBESEN(BesenInst).ToStr(Arguments^[1]^));
end;

end.

