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
  end;

  { TasPathModule }

  TasPathModule = class
  public
    constructor Create;

    procedure Join(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure Basename(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure Extname(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

{ TasPathModule }

constructor TasPathModule.Create;
begin

end;

procedure TasPathModule.Join(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

procedure TasPathModule.Basename(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

procedure TasPathModule.Extname(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

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

end.

