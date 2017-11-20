unit tgsendertypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson;

type
  TParseMode = (pmDefault, pmMarkdown, pmHTML);
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

  { TInlineKeyboardButtons }

  TInlineKeyboardButtons = class(TJSONArray)
  public
    constructor Create(const AButtonText, CallbackData: String); overload;
    function AddButton(const AButtonText, CallbackData: String): Integer;
  end;

  { TTelegramSender }

  TTelegramSender = class
  private
    FOnLogMessage: TLogMessageEvent;
    FResponse: String;
    FRequestBody: String;
    FToken: String;
    FRequestWhenAnswer: Boolean;
    procedure DebugMessage(const Msg: String); // будет отправлять в журнал все запросы и ответы. Полезно на время разработки
    procedure ErrorMessage(const Msg: String);
    procedure InfoMessage(const Msg: String);
    function HTTPPostFile(const Method, FileField, FileName: String; AFormData: TStrings): Boolean;
    function HTTPPostJSON(const Method: String): Boolean;
    function SendFile(const AMethod, AFileField, AFileName: String;
      MethodParameters: TStrings): Boolean;
    function SendMethod(const Method: String; MethodParameters: array of const): Boolean;
    function SendMethod(const Method: String; MethodParameters: TJSONObject): Boolean; overload;
    procedure SetRequestBody(AValue: String);
    procedure SetRequestWhenAnswer(AValue: Boolean);
  public
    constructor Create(const AToken: String);
    function editMessageText(const AMessage: String; chat_id: Int64 = 0; message_id: Int64 = 0;
      ParseMode: TParseMode = pmDefault; DisableWebPagePreview: Boolean=False;
      inline_message_id: String = ''; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendDocumentByFileName(chat_id: Int64; const AFileName: String;
      const ACaption: String; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendMessage(chat_id: Int64; const AMessage: String; ParseMode: TParseMode = pmDefault;
      DisableWebPagePreview: Boolean=False; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendPhoto(chat_id: Int64; const APhoto: String; const ACaption: String = ''): Boolean;
    function sendVideo(chat_id: Int64; const AVideo: String; const ACaption: String = ''): Boolean;
    { Пусть пользователь сам решит какого типа логирование он будет использовать }
    property OnLogMessage: TLogMessageEvent read FOnLogMessage write FOnLogMessage;
    property RequestBody: String read FRequestBody write SetRequestBody;
    property Response: String read FResponse;
    property Token: String read FToken write FToken;
    { If you're using webhooks, you can perform a request to the API while sending an answer...
      In this case the method to be invoked in the method parameter of the request.}
    property RequestWhenAnswer: Boolean read FRequestWhenAnswer write SetRequestWhenAnswer;
  end;

implementation

const
//  API names constants

  s_editMessageText='editMessageText';
  s_sendMessage='sendMessage';
  s_sendPhoto='sendPhoto';
  s_sendVideo='sendVideo';
  s_sendDocument='sendDocument';

  s_Method='method';
  s_Url = 'url';
  s_Text = 'text';
  s_ChatId = 'chat_id';
  s_MessageId = 'message_id';
  s_InlineMessageId = 'inline_message_id';
  s_Document = 'document';
  s_Caption = 'caption';
  s_ParseMode = 'parse_mode';
  s_ReplyMarkup = 'reply_markup';
  s_DsblWbpgPrvw = 'disable_web_page_preview';
  s_InlineKeyboard = 'inline_keyboard';
  s_SwitchInlineQuery = 'switch_inline_query';
  s_CallbackData = 'callback_data';
  s_SwitchInlineQueryCurrentChat = 's_switch_inline_query_current_chat';

  ParseModes: array[TParseMode] of PChar = ('Markdown', 'Markdown', 'HTML');

  API_URL='https://api.telegram.org/bot';

{ TInlineKeyboardButtons }

constructor TInlineKeyboardButtons.Create(const AButtonText,
  CallbackData: String);
begin
  inherited Create;
  AddButton(AButtonText, CallbackData);
end;

function TInlineKeyboardButtons.AddButton(const AButtonText, CallbackData: String): Integer;
var
  btn: TInlineKeyboardButton;
begin
  btn:=TInlineKeyboardButton.Create(AButtonText);
  btn.callback_data:=CallbackData;
  Result:=Add(btn);
end;

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

function TTelegramSender.HTTPPostFile(const Method, FileField, FileName: String;
  AFormData: TStrings): Boolean;
var
  HTTP: TFPHTTPClient;
  AStream: TStringStream;
begin
  HTTP:=TFPHTTPClient.Create(nil);
  AStream:=TStringStream.Create('');
  try
    HTTP.AddHeader('Content-Type','multipart/form-data');
    HTTP.FileFormPost(API_URL+FToken+'/'+Method, AFormData, FileField, FileName, AStream);
    FResponse:=AStream.DataString;
    Result:=True;
  except
    Result:=False;
  end;
  AStream.Free;
  HTTP.Free;
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

function TTelegramSender.SendFile(const AMethod, AFileField, AFileName: String;
  MethodParameters: TStrings): Boolean;
begin
  Result:=False;
  DebugMessage('Request for method "'+AMethod+'": '+FRequestBody);
  DebugMessage('Sending file '+AFileName);
  try
    Result:=HTTPPostFile(AMethod, AFileField, AFileName, MethodParameters);
    DebugMessage('Response: '+FResponse);
  except
    ErrorMessage('It is not succesful request to API! Request body: '+FRequestBody);
  end;
end;

procedure TTelegramSender.SetRequestBody(AValue: String);
begin
  if FRequestBody=AValue then Exit;
  FRequestBody:=AValue;
end;

procedure TTelegramSender.SetRequestWhenAnswer(AValue: Boolean);
begin
  if FRequestWhenAnswer=AValue then Exit;
  FRequestWhenAnswer:=AValue;
end;

function TTelegramSender.SendMethod(const Method: String;
  MethodParameters: array of const): Boolean;
var
  sendObj: TJSONObject;
begin
  sendObj:=TJSONObject.Create(MethodParameters);
  Result:=SendMethod(Method, sendObj);
  sendObj.Free;
end;

function TTelegramSender.SendMethod(const Method: String; MethodParameters: TJSONObject): Boolean;
begin
  Result:=False;
  if not FRequestWhenAnswer then
  begin
    RequestBody:=MethodParameters.AsJSON;
    DebugMessage('Request for method "'+Method+'": '+FRequestBody);
    try
      Result:=HTTPPostJson(Method);
      DebugMessage('Response: '+FResponse);
    except
      ErrorMessage('It is not succesful request to API! Request body: '+FRequestBody);
    end;
  end
  else
  begin
    MethodParameters.Strings[s_Method]:=Method;
    RequestBody:=MethodParameters.AsJSON;
    DebugMessage('Request in HTTP reply: '+FRequestBody);
    Result:=True;
  end;
end;

constructor TTelegramSender.Create(const AToken: String);
begin
  inherited Create;
  FToken:=AToken;
  FRequestWhenAnswer:=False;
end;

function TTelegramSender.editMessageText(const AMessage: String;
  chat_id: Int64; message_id: Int64; ParseMode: TParseMode;
  DisableWebPagePreview: Boolean; inline_message_id: String;
  ReplyMarkup: TReplyMarkup): Boolean;
var
  sendObj: TJSONObject;
begin
  Result:=False;
  sendObj:=TJSONObject.Create;
  with sendObj do
  try
    if chat_id<>0 then
      Add(s_ChatId, chat_id);
    if message_id<>0 then
      Add(s_MessageId, message_id);
    if inline_message_id<>EmptyStr then
      Add(s_InlineMessageId, inline_message_id);
    Add(s_Text, AMessage);
    if ParseMode<>pmDefault then
      Add(s_ParseMode, ParseModes[ParseMode]);
    Add(s_DsblWbpgPrvw, DisableWebPagePreview);
    if Assigned(ReplyMarkup) then
      Add(s_ReplyMarkup, ReplyMarkup.Clone); // Clone of ReplyMarkup object will have released with sendObject
    Result:=SendMethod(s_editMessageText, sendObj);
  finally
    Free;
  end;
end;

function TTelegramSender.sendDocumentByFileName(chat_id: Int64; const AFileName: String;
  const ACaption: String; ReplyMarkup: TReplyMarkup): Boolean;
var
  sendObj: TStringList;
begin
  Result:=False;
  sendObj:=TStringList.Create;
  with sendObj do
  try
    Add(s_ChatId+'='+IntToStr(chat_id));
    if ACaption<>EmptyStr then
      Add(s_Caption+'='+ACaption);
    if Assigned(ReplyMarkup) then
      Add(s_ReplyMarkup+'='+ReplyMarkup.AsJSON);
    Result:=SendFile(s_sendDocument, s_Document, AFileName, sendObj);
  finally
    Free;
  end;
end;

{  https://core.telegram.org/bots/api#sendmessage  }
function TTelegramSender.sendMessage(chat_id: Int64; const AMessage: String;
  ParseMode: TParseMode = pmDefault; DisableWebPagePreview: Boolean=False;
  ReplyMarkup: TReplyMarkup = nil): Boolean;
var
  sendObj: TJSONObject;
begin
  Result:=False;
  sendObj:=TJSONObject.Create;
  with sendObj do
  try
    Add(s_ChatId, chat_id);
    Add(s_Text, AMessage);
    if ParseMode<>pmDefault then
      Add(s_ParseMode, ParseModes[ParseMode]);
    Add(s_DsblWbpgPrvw, DisableWebPagePreview);
    if Assigned(ReplyMarkup) then
      Add(s_ReplyMarkup, ReplyMarkup.Clone); // Clone of ReplyMarkup object will have released with sendObject
    Result:=SendMethod(s_sendMessage, sendObj);
  finally
    Free;
  end;
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

