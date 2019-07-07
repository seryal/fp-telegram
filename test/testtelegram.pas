unit testtelegram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, tgsendertypes, testbase, tgtypes;

type

  { TTestSender }
  { Test sending messages. Object style }
  TTestSender= class(TTestTelegramClass)
  private
    FVideoUrl: String;
  protected
    procedure SetUp; override;
  public
    property VideoUrl: String read FVideoUrl;
  published
    procedure sendMessage;
    procedure InlineKeyboard;
    procedure sendVideo;
    procedure ChatMember;
  end;

  { TTestSenderProcedure }
  { Test sending messages. Simple procedure style}
  TTestSenderProcedure=class(TTestTelegramBase)
  published
    procedure sendMessage;
  end;

  { TTestReceiveLongPollingBase }
  { Test receiving updates via longpolling from the test bot. Please send test messages to the bot
    immediately before running the test! }
  TTestReceiveLongPolling=class(TTestTelegramClass)
  private
    FReceived: Boolean;
    procedure BotReceiveUpdate({%H-}ASender: TObject; AnUpdate: TTelegramUpdateObj);
  protected
    procedure SetUp; override;
    property Received: Boolean read FReceived write FReceived;
  published
    procedure ReceiveUpdate;
  end;

  { TTestPayments }

  TTestPayments=class(TTestTelegramClass)
  private
    FCurrency: String;
    FPortionAmount: Integer;
    FPortionLabel: String;
    FProviderToken: String;
    FReceived: Boolean;
    procedure BotReceivePreCheckoutQuery({%H-}ASender: TObject;
      APreCheckoutQuery: TTelegramPreCheckOutQuery);
    procedure BotReceiveSuccessfulPayment({%H-}ASender: TObject;
      AMessage: TTelegramMessageObj);
  protected
    procedure SetUp; override;
  public
    property Currency: String read FCurrency write FCurrency; // 3-letter ISO 4217 currency code
    property ProviderToken: String read FProviderToken write FProviderToken; // Payments provider token
    property PortionLabel: String read FPortionLabel write FPortionLabel;  //  Price portion label
    property PortionAmount: Integer read FPortionAmount write FPortionAmount; // Price portion amount
    property Received: Boolean read FReceived write FReceived;
  published
    procedure sendInvoice; // test send invoice
    procedure ReceivePreCheckoutQuery; // test receiving test pre_checkout_query update (after invoice is sent!)
    procedure ReceiveSuccessfulPayment;
  end;

implementation

const
  Msg='Test message sent from %s. Test procedure: %s';
  vd_cptn='Test video sent from %s. Test procedure: %s';
  Msg_md='Test message sent from %s. Test procedure: _%s_';

{ TTestReceiveLongPolling }

procedure TTestReceiveLongPolling.BotReceiveUpdate(ASender: TObject;
  AnUpdate: TTelegramUpdateObj);
begin
  SaveJSONData(Bot.JSONResponse, '~responce.json');
  SaveString(AnUpdate.AsString, '~update.json');
  Received:=True;
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

{ TTestPayments }

procedure TTestPayments.BotReceivePreCheckoutQuery(ASender: TObject;
  APreCheckoutQuery: TTelegramPreCheckOutQuery);
begin
  SaveJSONData(Bot.JSONResponse, '~responce.json');
  SaveString(APreCheckoutQuery.AsString, '~PreCheckoutQuery.json');
  Received:=True;
  Bot.answerPreCheckoutQuery(APreCheckoutQuery.ID, True);
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

procedure TTestPayments.BotReceiveSuccessfulPayment(ASender: TObject;
  AMessage: TTelegramMessageObj);
begin
  SaveJSONData(Bot.JSONResponse, '~responce.json');
  SaveString(AMessage.SuccessfulPayment.AsString, '~SuccessfulPayment.json');
  Received:=True;
end;

procedure TTestPayments.SetUp;
begin
  inherited SetUp;
  Bot.OnReceivePreCheckoutQuery:=@BotReceivePreCheckoutQuery;
  Bot.OnReceiveSuccessfulPayment:=@BotReceiveSuccessfulPayment;
  FProviderToken:=Conf.ReadString('Payment', 'ProviderToken', EmptyStr);
  FCurrency:=Conf.ReadString('Payment', 'Currency', EmptyStr);
  FPortionLabel:=Conf.ReadString('Payment', 'PricePortionLabel', EmptyStr);
  FPortionAmount:=Conf.ReadInteger('Payment', 'PricePortionAmount', 0);
end;

procedure TTestPayments.sendInvoice;
var
  Prices: TLabeledPriceArray;
begin
  Prices:=TLabeledPriceArray.Create(PortionLabel, PortionAmount);
  try
    Bot.sendInvoice(ChatID, 'TestProduct', 'This is test product for the testing purpose', 'payload001',
      ProviderToken, 'DeepLinkingStartParameter', Currency, Prices, nil);
  finally
    FreeAndNil(Prices);
  end;
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

procedure TTestPayments.ReceivePreCheckoutQuery;
begin
  Bot.getUpdates;
  if not Received then
    Fail('No updates were received. Please send the (test) payment for the created previously invoice')
  else
    Bot.getUpdatesEx(100, 1);  // Telegram API will understand that previous updates have been processed
end;

procedure TTestPayments.ReceiveSuccessfulPayment;
begin
  Bot.getUpdates;
  if not Received then
    Fail('No updates were received. Please receive PreCheckoutQuery before')
  else
    Bot.getUpdatesEx(100, 1);  // Telegram API will understand that previous updates have been processed
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

procedure TTestSender.SetUp;
begin
  inherited SetUp;
  FVideoUrl:=Conf.ReadString('Send', 'videourl', EmptyStr);
end;

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

procedure TTestSender.sendVideo;
begin
  Bot.sendVideo(ChatID, VideoUrl, Format(vd_cptn, [Self.ClassName, TestName]));
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

procedure TTestSender.ChatMember;
begin
  Bot.getChatMember(ChatID, UserID);
  SaveJSONData(Bot.JSONResponse, '~responce.json');
  if Bot.LastErrorCode<>0 then
    Fail('Error from telegram API server. Error code: '+IntToStr(Bot.LastErrorCode)+
      '. Description: '+Bot.LastErrorDescription);
end;

initialization

  RegisterTests([TTestSender, TTestSenderProcedure, TTestReceiveLongPolling, TTestPayments]);
end.

