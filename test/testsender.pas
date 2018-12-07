unit testsender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tgsendertypes, testbase;

type

  { TTestSender }

  TTestSender= class(TTestTelegramBase)
  private
    FChatID: Int64;
  protected
    procedure SetUp; override;
  published
    procedure sendMessage;
    procedure InlineKeyboard;
  end;

implementation

const
  Msg='Test message sent from %s. Test procedure: %s';
  Msg_md='Test message sent from %s. Test procedure: _%s_';

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

  RegisterTest(TTestSender);
end.

