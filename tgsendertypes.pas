unit tgsendertypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type
  TParseMode = (pmMarkdown, pmHTML);

  TLogMessageEvent = procedure(Sender: TObject; LogType: TEventType; const Msg: String) of object;

// to-do сделать классы для отправки сообщений в телеграм

  { TTelegramSender }

  TTelegramSender = class
  private
    FHTTP: TFPHTTPClient;
    FOnLogMessage: TLogMessageEvent;
    FResponse: String;
    FRequestBody: String;
    FToken: String;
    FWebhookRequest: Boolean;
    procedure DebugMessage(const Msg: String); // будет отправлять в журнал все запросы и ответы. Полезно на время разработки
    procedure ErrorMessage(const Msg: String);
    procedure InfoMessage(const Msg: String);
    function HTTPPostJSON(const Method: String): Boolean;
    function SendMethod(const Method: String; MethodParameters: array of const): Boolean;
    procedure SetRequestBody(AValue: String);
    procedure SetWebhookRequest(AValue: Boolean);
  public
    constructor Create(const AToken: String);
    destructor Destroy; override;
    function sendMessage(chat_id: Int64; const AMessage: String;
      ParseMode: TParseMode = pmMarkdown): Boolean;
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

uses
  fpjson;

const
  s_sendMessage='sendMessage';
  s_sendPhoto='sendPhoto';
  s_sendVideo='sendVideo';

  ParseModes: array[TParseMode] of PChar = ('Markdown', 'HTML');

  API_URL='https://api.telegram.org/bot';

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
begin
  Result:=False;
  FHTTP.RequestBody:=TStringStream.Create(FRequestBody);
  try
    FHTTP.AddHeader('Content-Type','application/json');
    FResponse:=FHTTP.Post(API_URL+FToken+'/'+Method);
    Result:=True;
  finally
    FHTTP.RequestBody.Free;
  end;
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

function TTelegramSender.SendMethod(const Method: String;
  MethodParameters: array of const): Boolean;
var
  sendObj: TJSONObject;
begin
  if not FWebhookRequest then
  begin
    sendObj:=TJSONObject.Create(MethodParameters);
    RequestBody:=sendObj.AsJSON;
    DebugMessage('Request: '+FRequestBody);
    Result:=HTTPPostJson(Method);
  end
  else
  begin
    sendObj:=TJSONObject.Create(MethodParameters);
    sendObj.Add('method', Method);
    RequestBody:=sendObj.AsJSON;
    Result:=True;
    DebugMessage('Request in HTTP reply: '+FRequestBody);
  end;
  if Result then
    DebugMessage('Response: '+FResponse)
  else
    ErrorMessage('It is not succesful request to API! Request body: '+FRequestBody);
end;

constructor TTelegramSender.Create(const AToken: String);
begin
  inherited Create;
  FToken:=AToken;
  FWebhookRequest:=False;
  FHTTP:=TFPHTTPClient.Create(nil);
end;

destructor TTelegramSender.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

{  https://core.telegram.org/bots/api#sendmessage  }
function TTelegramSender.sendMessage(chat_id: Int64; const AMessage: String;
  ParseMode: TParseMode = pmMarkdown): Boolean;
begin
  Result:=SendMethod(s_sendMessage,
    ['chat_id', chat_id, 'parse_mode', ParseModes[ParseMode], 'text', AMessage]);
end;

{ https://core.telegram.org/bots/api#sendphoto }
function TTelegramSender.sendPhoto(chat_id: Int64; const APhoto: String;
  const ACaption: String): Boolean;
begin
  Result:=SendMethod(s_sendPhoto, ['chat_id', chat_id, 'photo', APhoto, 'caption', ACaption]);
end;

{ https://core.telegram.org/bots/api#sendvideo }
function TTelegramSender.sendVideo(chat_id: Int64; const AVideo: String;
  const ACaption: String): Boolean;
begin
  Result:=SendMethod(s_sendVideo, ['chat_id', chat_id, 'video', AVideo, 'caption', ACaption]);
end;

end.

