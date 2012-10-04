unit AsteriaDocument;

interface

uses
  Classes, SysUtils,
  BESEN, BESENValue, BESENStringUtils;

type

  { TasDocument }

  TasDocument = class
  public
    constructor Create;
    procedure GetElementById(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure AddEventListener(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

{ TasDocument }

constructor TasDocument.Create;
begin

end;

procedure TasDocument.GetElementById(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

procedure TasDocument.AddEventListener(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

end.

