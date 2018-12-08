unit testbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, tgsendertypes, IniFiles, fpjson;

type

  { TTestTelegramBase }

  TTestTelegramBase= class(TTestCase)
  private
    FConf: TIniFile;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Conf: TIniFile read FConf;
    procedure SaveJSONData(AData: TJSONData; const AFileName: String);
    procedure SaveString(const AString, AFileName: String);
  end;

  { TTestTelegramClass }

  TTestTelegramClass= class(TTestTelegramBase)
  private
    FBot: TTelegramSender;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Bot: TTelegramSender read FBot;
  end;

implementation

{ TTestTelegramClass }

procedure TTestTelegramClass.SetUp;
begin
  inherited SetUp;
  FBot:=TTelegramSender.Create(FConf.ReadString('Bot', 'Token', EmptyStr));
  if FBot.Token=EmptyStr then
    Fail('Please, specify bot token in testtelegram.ini! See readme.md');
end;

procedure TTestTelegramClass.TearDown;
begin
  FreeAndNil(FBot);
  inherited TearDown;
end;

{ TTestTelegramBase }

procedure TTestTelegramBase.SetUp;
begin
  FConf:=TMemIniFile.Create('testtelegram.ini');
  TelegramAPI_URL:=FConf.ReadString('API', 'Endpoint', TelegramAPI_URL); // For Russian specific case
end;

procedure TTestTelegramBase.TearDown;
begin
  FreeAndNil(FConf);
end;

procedure TTestTelegramBase.SaveJSONData(AData: TJSONData;
  const AFileName: String);
begin
  if not Assigned(AData) then
  begin
    Fail('JSON data is empty!');
    Exit;
  end;
  SaveString(AData.FormatJSON, AFileName);
end;

procedure TTestTelegramBase.SaveString(const AString, AFileName: String);
var
  AStrings: TStringList;
begin
  AStrings:=TStringList.Create;
  try
    AStrings.Text:=AString;
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

end.

