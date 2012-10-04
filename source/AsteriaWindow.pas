unit AsteriaWindow;

interface

uses
  Classes, SysUtils;

type

  { TasWindow }

  TasWindow = class
  public
    constructor Create;

    procedure SetTimeout(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure SetInterval(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
    procedure RequestAnimationFrame(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: Integer; var ResultValue: TBESENValue);
  end;

implementation

{ TasWindow }

constructor TasWindow.Create;
begin

end;

procedure TasWindow.SetTimeout(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

procedure TasWindow.SetInterval(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

procedure TasWindow.RequestAnimationFrame(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: Integer;
  var ResultValue: TBESENValue);
begin

end;

end.

