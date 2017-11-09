unit tgsendertypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson;

type
  TParseMode = (pmMarkdown, pmHTML);
  TLogMessageEvent = procedure(Sender: TObject; LogType: TEventType; const Msg: String) of object;
  TInlineKeyboardButton = class;

  { TReplyMarkup }

  TReplyMarkup = class(TJSONObject)
  private
    function GetInlineKeyBoard: TJSONArray;
    procedure SetInlineKeyBoard(AValue: TJSONArray);
  public
    property InlineKeyBoard: TJSONArray read GetInlineKeyBoard write SetInlineKeyBoard;
  end;

  { TInlineKeyboardButton }

  TInlineKeyboardButton = class(TJSONObject)
  private
    function CheckOptnlNull: Boolean;
    procedure CheckOptnlAndSet(const ParamName, ParamValue: String);
    function Getcallback_data: String;
    function Getswitch_inline_query: String;
    function Getswitch_inline_query_current_chat: String;
    function Gettext: String;
    function Geturl: String;
    procedure Setcallback_data(AValue: String);
    procedure Setswitch_inline_query(AValue: String);
    procedure Setswitch_inline_query_current_chat(AValue: String);
    procedure Settext(AValue: String);
    procedure Seturl(AValue: String);
  public
    constructor Create(const AText: String);
    property text: String read Gettext write Settext;
    property url: String read Geturl write Seturl;
    property callback_data: String read Getcallback_data write Setcallback_data;
    property switch_inline_query: String read Getswitch_inline_query write Setswitch_inline_query;
    property switch_inline_query_current_chat: String read Getswitch_inline_query_current_chat
      write Setswitch_inline_query_current_chat;
  end;

// to-do сделать классы для отправки сообщений в телеграм

  { TTelegramSender }

  TTelegramSender = class
  private
    FOnLogMessage: TLogMessageEvent;
    FResponse: String;
    FRequestBody: String;
    FToken: String;
    FWebhookRequest: Boolean;
    procedure DebugMessage(const Msg: String); // будет отправлять в журнал все запросы и ответы. Полезно на время разработки
    procedure ErrorMessage(const Msg: String);
    procedure InfoMessage(const Msg: String);
    function HTTPPostJSON(const Method: String): Boolean;
    function SendMethod(MethodParameters: array of const): Boolean;
    procedure SetRequestBody(AValue: String);
    procedure SetWebhookRequest(AValue: Boolean);
  public
    constructor Create(const AToken: String);
    function sendMessage(chat_id: Int64; const AMessage: String;
      ParseMode: TParseMode = pmMarkdown; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendPhoto(chat_id: Int64; const APhoto: String; const ACaption: String = ''): Boolean;
    function sendVideo(chat_id: Int64; const AVideo: String; const ACaption: String = ''): Boolean;
    { Пусть пользователь сам решит какого типа логирование он будет использовать }
    property OnLogMessage: TLogMessageEvent read FOnLogMessage write FOnLogMessage;
    property RequestBody: String read FRequestBody write SetRequestBody;
    property Response: String read FResponse;
    property Token: String read FToken write FToken;
    { If you're using webhooks, you can perform a request to the API while sending an answer...
      In this case the method to be invoked in the method parameter of the request.}
    property WebhookRequest: Boolean read FWebhookRequest write SetWebhookRequest;
  end;

implementation

const
  s_sendMessage='sendMessage';
  s_sendPhoto='sendPhoto';
  s_sendVideo='sendVideo';

  s_Method='method';
  s_Url = 'url';
  s_Text = 'text';
  s_ChatId = 'chat_id';
  s_ParseMode = 'parse_mode';
  s_ReplyMarkup = 'reply_markup';
  s_InlineKeyboard = 'inline_keyboard';
  s_SwitchInlineQuery = 'switch_inline_query';
  s_CallbackData = 'callback_data';
  s_SwitchInlineQueryCurrentChat = 's_switch_inline_query_current_chat';

  ParseModes: array[TParseMode] of PChar = ('Markdown', 'HTML');

  API_URL='https://api.telegram.org/bot';

{ TReplyMarkup }

function TReplyMarkup.GetInlineKeyBoard: TJSONArray;
begin
  Result:=Arrays[s_InlineKeyboard];
end;

procedure TReplyMarkup.SetInlineKeyBoard(AValue: TJSONArray);
begin
  Arrays[s_InlineKeyboard]:=AValue;
end;

{ TInlineKeyboardButton }

procedure TInlineKeyboardButton.Settext(AValue: String);
begin
  Strings[s_text]:=AValue;
end;

procedure TInlineKeyboardButton.Setcallback_data(AValue: String);
begin
  CheckOptnlAndSet(s_callbackdata, AValue);
end;

function TInlineKeyboardButton.Geturl: String;
begin
  Result:=Strings[s_url];
end;

function TInlineKeyboardButton.CheckOptnlNull: Boolean;
begin
  Result:=not (Assigned(Find(s_CallbackData)) or Assigned(Find(s_SwitchInlineQuery)) or
    Assigned(Find(s_SwitchInlineQueryCurrentChat)) or Assigned(Find(s_Url)))
end;

procedure TInlineKeyboardButton.CheckOptnlAndSet(const ParamName, ParamValue: String);
var
  Op: Boolean;
begin
  Op:=CheckOptnlNull;
  if op or (not op and Assigned(Find(ParamName))) then // Only one optional parameters must set!
    Strings[ParamName]:=ParamValue
 { else
     DoError('Error')}
end;

function TInlineKeyboardButton.Getcallback_data: String;
begin
  Result:=Strings[s_callbackdata];
end;

function TInlineKeyboardButton.Getswitch_inline_query: String;
begin
  Result:=Strings[s_SwitchInlineQuery];
end;

function TInlineKeyboardButton.Getswitch_inline_query_current_chat: String;
begin
  Result:=Strings[s_SwitchInlineQueryCurrentChat];
end;

function TInlineKeyboardButton.Gettext: String;
begin
  Result:=Strings[s_text];
end;

procedure TInlineKeyboardButton.Setswitch_inline_query(AValue: String);
begin
  CheckOptnlAndSet(s_SwitchInlineQuery, AValue);
end;

procedure TInlineKeyboardButton.Setswitch_inline_query_current_chat(
  AValue: String);
begin
  CheckOptnlAndSet(s_SwitchInlineQueryCurrentChat, AValue);
end;

procedure TInlineKeyboardButton.Seturl(AValue: String);
begin
  CheckOptnlAndSet(s_url, AValue);
end;

constructor TInlineKeyboardButton.Create(const AText: String);
begin
  inherited Create;
  Add(s_text, AText);
end;

{ TTelegramSender }

procedure TTelegramSender.DebugMessage(const Msg: String);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(Self, etDebug, Msg);
end;

procedure TTelegramSender.ErrorMessage(const Msg: String);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(Self, etError, Msg);
end;

procedure TTelegramSender.InfoMessage(const Msg: String);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(Self, etInfo, Msg);
end;

function TTelegramSender.HTTPPostJSON(const Method: String): Boolean;
var
  HTTP: TFPHTTPClient;
begin
  HTTP:=TFPHTTPClient.Create(nil);
  try
    HTTP.RequestBody:=TStringStream.Create(FRequestBody);
    try
      HTTP.AddHeader('Content-Type','application/json');
      FResponse:=HTTP.Post(API_URL+FToken+'/'+Method);
    finally
      HTTP.RequestBody.Free;
    end;
    Result:=True;
  except
    Result:=False;
  end;
  HTTP.Free;
end;

procedure TTelegramSender.SetRequestBody(AValue: String);
begin
  if FRequestBody=AValue then Exit;
  FRequestBody:=AValue;
end;

procedure TTelegramSender.SetWebhookRequest(AValue: Boolean);
begin
  if FWebhookRequest=AValue then Exit;
  FWebhookRequest:=AValue;
end;

function TTelegramSender.SendMethod(MethodParameters: array of const): Boolean;
var
  sendObj: TJSONObject;
  Method: String;

begin
  sendObj:=TJSONObject.Create(MethodParameters);
  try
    if not FWebhookRequest then
    begin
      Method:=sendObj.Strings['method'];
      sendObj.Delete(0);  // Имя метода присутствует в адресе API. См. HTTPPostJSON
      RequestBody:=sendObj.AsJSON;
      DebugMessage('Request: '+FRequestBody);
      Result:=HTTPPostJson(Method);
      DebugMessage('Response: '+FResponse);
    end
    else
    begin
      RequestBody:=sendObj.AsJSON;
      DebugMessage('Request in HTTP reply: '+FRequestBody);
      Result:=True;
    end;
  finally
    sendObj.Free;
    if not Result then
      ErrorMessage('It is not succesful request to API! Request body: '+FRequestBody);
  end;
end;

constructor TTelegramSender.Create(const AToken: String);
begin
  inherited Create;
  FToken:=AToken;
  FWebhookRequest:=False;
end;

{  https://core.telegram.org/bots/api#sendmessage  }
function TTelegramSender.sendMessage(chat_id: Int64; const AMessage: String;
  ParseMode: TParseMode = pmMarkdown; ReplyMarkup: TReplyMarkup = nil): Boolean;
begin
  if Assigned(ReplyMarkup) then
  begin

    Result:=SendMethod([s_Method, s_sendMessage, s_ChatId, chat_id, s_text, AMessage,
      s_ParseMode, ParseModes[ParseMode], s_ReplyMarkup, ReplyMarkup.Clone])
  end
  else
    Result:=SendMethod([s_Method, s_sendMessage, s_ChatId, chat_id, s_text, AMessage,
      s_ParseMode, ParseModes[ParseMode]]);
end;

{ https://core.telegram.org/bots/api#sendphoto }
function TTelegramSender.sendPhoto(chat_id: Int64; const APhoto: String;
  const ACaption: String): Boolean;
begin
  Result:=SendMethod(['method', s_sendPhoto, 'chat_id', chat_id, 'photo', APhoto, 'caption', ACaption]);
end;

{ https://core.telegram.org/bots/api#sendvideo }
function TTelegramSender.sendVideo(chat_id: Int64; const AVideo: String;
  const ACaption: String): Boolean;
begin
  Result:=SendMethod(['method', s_sendVideo, 'chat_id', chat_id, 'video', AVideo, 'caption', ACaption]);
end;

end.

