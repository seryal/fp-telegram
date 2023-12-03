unit DTTelegramBot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgbot_dt
  ;

type

  { TDTLongPolBot }

  TDTLongPolBot = class(TCustomDTTelegramBot)
  private

  protected

  public

  published
    property Token;
    property StartText;
    property HelpText;
    property LongPollingTime;
    property OnReceiveMessageUpdate;
    property OnReceiveCallack;
    property OnReceiveUpdate;
  end;

procedure Register;

implementation

uses
  LResources
  ;

procedure Register;
begin
  {$I dttelegrambot_icon.lrs}
  RegisterComponents('Misc', [TDTLongPolBot]);
end;

end.
