unit testsender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, tgsendertypes, testbase, tgtypes;

type

  { TTestSender }
  { Test sending messages. Object style }
  TTestSender= class(TTestTelegramClass)
  published
    procedure sendMessage;
    procedure InlineKeyboard;
  end;

  { TTestSenderProcedure }
  { Test sending messages. Simple procedure style}
  TTestSenderProcedure=class(TTestTelegramBase)
  published
    procedure sendMessage;
  end;

  { TTestReceiveLongPolling }
  { Test receiving updates via longpolling from the test bot. Please send test messages to the bot
    immediately before running the test! }
  TTestReceiveLongPolling=class(TTestTelegramClass)
  private
    FReceived: Boolean;
    procedure BotReceiveUpdate({%H-}ASender: TObject; AnUpdate: TTelegramUpdateObj);
    procedure SetReceived(AValue: Boolean);
  protected
    procedure SetUp; override;
    property Received: Boolean read FReceived write SetReceived;
  published
    procedure ReceiveUpdate;
  end;

implementation

const
  Msg='Test message sent from %s. Test procedure: %s';
  Msg_md='Test message sent from %s. Test procedure: _%s_';

{ TTestReceiveLongPolling }

procedure TTestReceiveLongPolling.BotReceiveUpdate(ASender: TObject;
  AnUpdate: TTelegramUpdateObj);
begin
  SaveJSONData(Bot.JSONResponse, '~responce.json');
  SaveString(AnUpdate.AsString, '~update.json');
  Received:=True;
end;

procedure TTestReceiveLongPolling.SetReceived(AValue: Boolean);
begin
  if FReceived=AValue then Exit;
  FReceived:=AValue;
end;

procedure TTestReceiveLongPolling.SetUp;
begin
  inherited SetUp;
  Bot.OnReceiveUpdate:=@BotReceiveUpdate;
  FReceived:=False;
end;

procedure TTestReceiveLongPolling.ReceiveUpdate;
begin
  Bot.getUpdates;
  if not Received then
    Fail('No updates were received. Send, for example, a message to the test bot')
  else
    Bot.getUpdatesEx(100, 1); //
end;

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

procedure TTestSender.sendMessage;
begin
  Bot.sendMessage(ChatID, Format(Msg, [Self.ClassName, TestName]));
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
  Bot.sendMessage(ChatID, Format(Msg_md, [Self.ClassName, TestName]), pmMarkdown);
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
    Bot.sendMessage(ChatID, Format(Msg_md, [Self.ClassName, TestName]), pmMarkdown, False,
      ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

initialization

  RegisterTests([TTestSender, TTestSenderProcedure, TTestReceiveLongPolling]);
end.

