unit testsender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, tgsendertypes, testbase;

type

  { TTestSender }

  TTestSender= class(TTestTelegramClass)
  private
    FChatID: Int64;
  protected
    procedure SetUp; override;
  published
    procedure sendMessage;
    procedure InlineKeyboard;
  end;

  { TTestSenderProcedure }

  TTestSenderProcedure=class(TTestTelegramBase)
  published
    procedure sendMessage;
  end;

implementation

const
  Msg='Test message sent from %s. Test procedure: %s';
  Msg_md='Test message sent from %s. Test procedure: _%s_';

{ TTestSenderProcedure }

procedure TTestSenderProcedure.sendMessage;
var
  AToken: String;
  AChatID: Int64;
  AReply: String;
begin
  AToken:=Conf.ReadString('Bot', 'Token', EmptyStr);
  AChatID:=Conf.ReadInt64('Chat', 'ID', 0);
  if not TgBotSendMessage(AToken, AChatID, Format(Msg, [Self.ClassName, TestName]), AReply) then
    Fail('Fail to send message from telegram bot!');
  SaveString(AReply, '~JSONResponce.json');
end;

{ TTestSender }

procedure TTestSender.SetUp;
begin
  inherited SetUp;
  FChatID:=Conf.ReadInt64('Chat', 'ID', 0);
  if FChatID=0 then
    Fail('Please, specify chat ID in testtelegram.ini! See readme.md');
end;

procedure TTestSender.sendMessage;
begin
  Bot.sendMessage(FChatID, Format(Msg, [Self.ClassName, TestName]));
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
  Bot.sendMessage(FChatID, Format(Msg_md, [Self.ClassName, TestName]), pmMarkdown);
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

procedure TTestSender.InlineKeyboard;
var
  ReplyMarkup: TReplyMarkup;
  Buttons: TInlineKeyboardButtons;
begin
  ReplyMarkup:=TReplyMarkup.Create;
  try
    Buttons:=ReplyMarkup.CreateInlineKeyBoard.Add;
    Buttons.AddButtonUrl('Github.com',
      'https://github.com/Al-Muhandis/fp-telegram');
    ReplyMarkup.InlineKeyBoard.Add.AddButtons(['Button 1', 'Callback data 1', 'Button 2', 'Callback data 2']);
    Bot.sendMessage(FChatID, Format(Msg_md, [Self.ClassName, TestName]), pmMarkdown, False,
      ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

initialization

  RegisterTests([TTestSender, TTestSenderProcedure]);
end.

