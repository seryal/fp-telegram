unit testbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, tgsendertypes, IniFiles;

type

  { TTestTelegramBase }

  TTestTelegramBase= class(TTestCase)
  private
    FConf: TIniFile;
    FBot: TTelegramSender;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Bot: TTelegramSender read FBot;
    property Conf: TIniFile read FConf;
  end;

implementation

{ TTestTelegramBase }

procedure TTestTelegramBase.SetUp;
begin
  FConf:=TMemIniFile.Create('testtelegram.ini');
  FBot:=TTelegramSender.Create(FConf.ReadString('Bot', 'Token', EmptyStr));
  if FBot.Token=EmptyStr then
    Fail('Please, specify bot token in testtelegram.ini! See readme.md');
  FBot.APIEndPoint:=FConf.ReadString('API', 'Endpoint', 'https://api.telegram.org/bot'); // For Russian specific case
end;

procedure TTestTelegramBase.TearDown;
begin
  FreeAndNil(FBot);
  FreeAndNil(FConf);
end;

end.

