unit AsteriaFileSystem;

interface

uses
  Classes, SysUtils,
  BESEN, BESENNativeObject, BESENValue, BESENStringUtils,
  AsteriaGlobal;

type

  { TasFileSystem }

  TasFileSystem = class(TBESENNativeObject)
  public
    constructor Create;

    procedure FileExists(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure DirExists(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);

    procedure ReadFile(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure WriteFile(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

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
begin

end;

procedure TasFileSystem.WriteFile(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

end.

