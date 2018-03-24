unit tgsendertypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, tgtypes, ghashmap, tgstatlog;

type
  TParseMode = (pmDefault, pmMarkdown, pmHTML);
  TInlineQueryResultType = (qrtArticle, qrtPhoto, qrtVideo, qrtMpeg4Gif, qrtUnknown);
  TLogMessageEvent = procedure(ASender: TObject; LogType: TEventType; const Msg: String) of object;
  TInlineKeyboardButton = class;
  TKeyboardButton = class;

  TOnUpdateEvent = procedure (ASender: TObject; AnUpdate: TTelegramUpdateObj) of object;

  TCommandEvent = procedure (ASender: TObject; const ACommand: String;
    AMessage: TTelegramMessageObj) of object;
  TCallbackEvent = procedure (ASender: TObject; ACallback: TCallbackQueryObj) of object;
  TMessageEvent = procedure (ASender: TObject; AMessage: TTelegramMessageObj) of object;
  TInlineQueryEvent = procedure (ASender: TObject; AnInlineQuery: TTelegramInlineQueryObj) of object;
  TChosenInlineResultEvent = procedure (ASender: TObject;
    AChosenInlineResult: TTelegramChosenInlineResultObj) of object;

  { TStringHash }

  TStringHash = class
    class function hash(s: String; n: Integer): Integer;
  end;

  generic TStringHashMap<T> = class(specialize THashMap<String,T,TStringHash>) end;

  TCommandHandlersMap = specialize TStringHashMap<TCommandEvent>;

  { TReplyMarkup }

  TReplyMarkup = class(TJSONObject)
  private
    function GetForceReply: Boolean;
    function GetInlineKeyBoard: TJSONArray;
    function GetOneTimeKeyboard: Boolean;
    function GetReplyKeyboardMarkup: TJSONArray;
    function GetResizeKeyboard: Boolean;
    function GetSelective: Boolean;
    procedure SetForceReply(AValue: Boolean);
    procedure SetInlineKeyBoard(AValue: TJSONArray);
    procedure SetOneTimeKeyboard(AValue: Boolean);
    procedure SetReplyKeyboardMarkup(AValue: TJSONArray);
    procedure SetResizeKeyboard(AValue: Boolean);
    procedure SetSelective(AValue: Boolean);
  public  { Only one from InlineKeyboard or ReplyMarkup is must to set }
    property InlineKeyBoard: TJSONArray read GetInlineKeyBoard write SetInlineKeyBoard;
{ ٌReplyKeyboard porerties }
    property ReplyKeyboardMarkup: TJSONArray read GetReplyKeyboardMarkup
      write SetReplyKeyboardMarkup;
{ Only if ReplyKeyboard is present then optional}
    property ResizeKeyboard: Boolean read GetResizeKeyboard write SetResizeKeyboard;
    property OneTimeKeyboard: Boolean read GetOneTimeKeyboard write SetOneTimeKeyboard;
{ ForceReply properties
  If property ForceReply is set then ReplyMarkup must be only ForceReply type }
    property ForceReply: Boolean read GetForceReply write SetForceReply;
    property Selective: Boolean read GetSelective write SetSelective;
  end;

  { TKeyboardButton }

  TKeyboardButton = class(TJSONObject)
  private
    function GetRequestContact: Boolean;
    function GetRequestLocation: Boolean;
    function Gettext: String;
    procedure SetRequestContact(AValue: Boolean);
    procedure SetRequestLocation(AValue: Boolean);
    procedure Settext(AValue: String);
  public
    constructor Create(const AText: String);
    property text: String read Gettext write Settext;
    property RequestContact: Boolean read GetRequestContact write SetRequestContact; // Optional
    property RequestLocation: Boolean read GetRequestLocation write SetRequestLocation; // Optional
  end;

  { TKeyboardButtons }

  TKeyboardButtons = class(TJSONArray)
  private
    function GetButtons(Index : Integer): TKeyboardButton;
    procedure SetButtons(Index : Integer; AValue: TKeyboardButton);
  public
    constructor Create(const AText: String); overload;
    constructor Create(const AButtons: array of String); overload;
    function AddButton(const AButtonText: String): Integer;
    procedure AddButtons(const AButtons: array of String);
    property Buttons[Index : Integer]: TKeyboardButton read GetButtons write SetButtons;
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
    constructor Create(const AButtons: array of String); overload;
    function AddButton(const AButtonText, CallbackData: String): Integer;
    procedure AddButtons(const AButtons: array of String);
  end;

  { TInputMessageContent }

  TInputMessageContent = class(TJSONObject)
  private
    function GetMessageText: String;
    function GetParseMode: TParseMode;
    procedure SetMessageText(AValue: String);
    procedure SetParseMode(AValue: TParseMode);
  public
    constructor Create(const AMessageText: String; AParseMode: TParseMode = pmDefault);
    property MessageText: String read GetMessageText write SetMessageText;
    property ParseMode: TParseMode read GetParseMode write SetParseMode;
  end;

  { TInlineQueryResult }

  TInlineQueryResult = class(TJSONObject)
  private
    function GetDescription: String;
    function GetID: String;
    function GetInputMessageContent: TInputMessageContent;
    function GetIQRType: TInlineQueryResultType;
    function GetMimeType: String;
    function GetMpeg4Height: Integer;
    function GetMpeg4Url: String;
    function GetMpeg4Width: Integer;
    function GetPhotoHeight: Integer;
    function GetPhotoUrl: String;
    function GetPhotoWidth: Integer;
    function GetReplyMarkup: TReplyMarkup;
    function GetThumbUrl: String;
    function GetTitle: String;
    function GetVideoUrl: String;
    procedure SetDescription(AValue: String);
    procedure SetID(AValue: String);
    procedure SetInputMessageContent(AValue: TInputMessageContent);
    procedure SetIQRType(AValue: TInlineQueryResultType);
    procedure SetMimeType(AValue: String);
    procedure SetMpeg4Height(AValue: Integer);
    procedure SetMpeg4Url(AValue: String);
    procedure SetMpeg4Width(AValue: Integer);
    procedure SetPhotoHeight(AValue: Integer);
    procedure SetPhotoUrl(AValue: String);
    procedure SetPhotoWidth(AValue: Integer);
    procedure SetReplyMarkup(AValue: TReplyMarkup);
    procedure SetThumbUrl(AValue: String);
    procedure SetTitle(AValue: String);
    procedure SetVideoUrl(AValue: String);
  public
    property IQRType: TInlineQueryResultType read GetIQRType write SetIQRType;
    property ID: String read GetID write SetID;
    property Title: String read GetTitle write SetTitle;
    property InputMessageContent: TInputMessageContent read GetInputMessageContent write SetInputMessageContent;
    property ReplyMarkup: TReplyMarkup read GetReplyMarkup write SetReplyMarkup;
    property Description: String read GetDescription write SetDescription;

    property PhotoUrl: String read GetPhotoUrl write SetPhotoUrl;
    property ThumbUrl: String read GetThumbUrl write SetThumbUrl;

    property VideoUrl: String read GetVideoUrl write SetVideoUrl;
    property MimeType: String read GetMimeType write SetMimeType;

    property PhotoWidth: Integer read GetPhotoWidth write SetPhotoWidth;
    property PhotoHeight: Integer read GetPhotoHeight write SetPhotoHeight;

    property Mpeg4Url: String read GetMpeg4Url write SetMpeg4Url;
    property Mpeg4Height: Integer read GetMpeg4Height write SetMpeg4Height;
    property Mpeg4Width: Integer read GetMpeg4Width write SetMpeg4Width;
  end;

  { TTelegramSender }

  TTelegramSender = class
  private
    FBotUsername: String;
    FCurrentChatId: Int64;
    FCurrentMessage: TTelegramMessageObj;
    FCurrentUser: TTelegramUserObj;
    FBotUser: TTelegramUserObj;
    FLanguage: string;
    FLastErrorCode: Integer;
    FLastErrorDescription: String;
    FOnReceiveCallbackQuery: TCallbackEvent;
    FOnReceiveChannelPost: TMessageEvent;
    FOnReceiveChosenInlineResult: TChosenInlineResultEvent;
    FOnReceiveInlineQuery: TInlineQueryEvent;
    FOnReceiveMessage: TMessageEvent;
    FUpdate: TTelegramUpdateObj;
    FJSONResponse: TJSONData;
    FOnLogMessage: TLogMessageEvent;
    FOnReceiveUpdate: TOnUpdateEvent;
    FProcessUpdate: Boolean;
    FResponse: String;
    FRequestBody: String;
    FToken: String;
    FRequestWhenAnswer: Boolean;
    FCommandHandlers: TCommandHandlersMap; 
    FChannelCommandHandlers: TCommandHandlersMap;
    FUpdateLogger: TtgStatLog;
    function CurrentLanguage(AUser: TTelegramUserObj): String;
    function CurrentLanguage(AMessage: TTelegramMessageObj): String;
    function GetChannelCommandHandlers(const Command: String): TCommandEvent;
    function GetCommandHandlers(const Command: String): TCommandEvent;
    procedure SetBotUsername(AValue: String);
    procedure SetChannelCommandHandlers(const Command: String;
      AValue: TCommandEvent);
    procedure SetCommandHandlers(const Command: String; AValue: TCommandEvent);
    function HTTPPostFile(const Method, FileField, FileName: String; AFormData: TStrings): Boolean;
    function HTTPPostJSON(const Method: String): Boolean;
    procedure ProcessCommands(AMessage: TTelegramMessageObj; AHandlers: TCommandHandlersMap);
    function ResponseToJSONObject: TJSONObject;
    function ResponseHandle: Boolean;
    function SendFile(const AMethod, AFileField, AFileName: String;
      MethodParameters: TStrings): Boolean;
    function SendMethod(const Method: String; MethodParameters: array of const): Boolean;
    function SendMethod(const Method: String; MethodParameters: TJSONObject): Boolean; overload;
    procedure SetJSONResponse(AValue: TJSONData);
    procedure SetLastErrorCode(AValue: Integer);
    procedure SetLastErrorDescription(AValue: String);
    procedure SetOnReceiveCallbackQuery(AValue: TCallbackEvent);
    procedure SetOnReceiveChannelPost(AValue: TMessageEvent);
    procedure SetOnReceiveChosenInlineResult(AValue: TChosenInlineResultEvent);
    procedure SetOnReceiveInlineQuery(AValue: TInlineQueryEvent);
    procedure SetOnReceiveMessage(AValue: TMessageEvent);
    procedure SetOnReceiveUpdate(AValue: TOnUpdateEvent);
    procedure SetProcessUpdate(AValue: Boolean);
    procedure SetRequestBody(AValue: String);
    procedure SetRequestWhenAnswer(AValue: Boolean);
    procedure SetUpdateLogger(AValue: TtgStatLog);
    class function StringToJSONObject(const AString: String): TJSONObject;
  protected
    procedure DoReceiveMessageUpdate(AMessage: TTelegramMessageObj); virtual;
    procedure DoReceiveCallbackQuery(ACallback: TCallbackQueryObj); virtual;
    procedure DoReceiveChannelPost(AChannelPost: TTelegramMessageObj); virtual;
    procedure DoReceiveInlineQuery(AnInlineQuery: TTelegramInlineQueryObj);  virtual;
    procedure DoReceiveChosenInlineResult(AChosenInlineResult: TTelegramChosenInlineResultObj); virtual;
    procedure DebugMessage(const Msg: String); virtual; // будет отправлять в журнал все запросы и ответы. Полезно на время разработки
    procedure ErrorMessage(const Msg: String); virtual;
    procedure InfoMessage(const Msg: String); virtual;
    function IsBanned({%H-}ChatID: Int64): Boolean; virtual;
    function IsSimpleUser({%H-}ChatID: Int64): Boolean; virtual;
    procedure SetLanguage(const AValue: String); virtual;
  public
    constructor Create(const AToken: String);
    destructor Destroy; override;
    procedure DoReceiveUpdate(AnUpdate: TTelegramUpdateObj); virtual;
    function editMessageText(const AMessage: String; chat_id: Int64; message_id: Int64;
      ParseMode: TParseMode = pmDefault; DisableWebPagePreview: Boolean=False;
      inline_message_id: String = ''; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function editMessageText(const AMessage: String; ParseMode: TParseMode = pmDefault;
      DisableWebPagePreview: Boolean=False; ReplyMarkup: TReplyMarkup = nil): Boolean; overload;
    function getMe: Boolean;
    function getUpdates(offset: Int64 = 0; limit: Integer = 0; timeout: Integer = 0;
      allowed_updates: TUpdateSet = []): Boolean;

    function CurrentIsSimpleUser: Boolean; overload;
    function CurrentIsBanned: Boolean; overload;
    function sendDocumentByFileName(chat_id: Int64; const AFileName: String;
      const ACaption: String; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendLocation(chat_id: Int64; Latitude, Longitude: Real; LivePeriod: Integer = 0;
      ParseMode: TParseMode = pmDefault; DisableWebPagePreview: Boolean=False;
      ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendMessage(chat_id: Int64; const AMessage: String; ParseMode: TParseMode = pmDefault;
      DisableWebPagePreview: Boolean=False; ReplyMarkup: TReplyMarkup = nil): Boolean;
    function sendMessage(const chat_id, AMessage: String; ParseMode: TParseMode = pmDefault;
      DisableWebPagePreview: Boolean=False; ReplyMarkup: TReplyMarkup = nil): Boolean; overload;
    function sendMessage(const AMessage: String; ParseMode: TParseMode = pmDefault;
      DisableWebPagePreview: Boolean=False; ReplyMarkup: TReplyMarkup = nil): Boolean; overload;
    function sendPhoto(chat_id: Int64; const APhoto: String; const ACaption: String = ''): Boolean;
    function sendPhoto(const APhoto: String; const ACaption: String = ''): Boolean; overload;
    function sendVideo(chat_id: Int64; const AVideo: String; const ACaption: String = ''): Boolean;
    function sendVideo(const AVideo: String; const ACaption: String = ''): Boolean; overload;
    function answerInlineQuery(const AnInlineQueryID: String; Results: TJSONArray;
      CacheTime: Integer = 300; IsPersonal: Boolean = False; const NextOffset: String = '';
      const SwitchPmText: String = ''; const SwitchPmParameter: String = ''): Boolean;
    property BotUser: TTelegramUserObj read FBotUser;
    property JSONResponse: TJSONData read FJSONResponse write SetJSONResponse;
    property CurrentChatId: Int64 read FCurrentChatId;
    property CurrentUser: TTelegramUserObj read FCurrentUser;
    property CurrentMessage: TTelegramMessageObj read FCurrentMessage;
    property CurrentUpdate: TTelegramUpdateObj read FUpdate;
    property Language: string read FLanguage write SetLanguage;
    property OnLogMessage: TLogMessageEvent read FOnLogMessage write FOnLogMessage;
    property RequestBody: String read FRequestBody write SetRequestBody;
    property Response: String read FResponse;
    property Token: String read FToken write FToken;
    { If you're using webhooks, you can perform a request to the API while sending an answer...
      In this case the method to be invoked in the method parameter of the request.}
    property RequestWhenAnswer: Boolean read FRequestWhenAnswer write SetRequestWhenAnswer;
    { if ProcessUpdate then the incoming update object will be processed.
      May be useful for multithreaded work when updates is receiving in one Sender object and
      processing and sending to telegram server in another Sender object. In this case ProcessUpdate = False}
    property ProcessUpdate: Boolean read FProcessUpdate write SetProcessUpdate;
    property CommandHandlers [const Command: String]: TCommandEvent read GetCommandHandlers
      write SetCommandHandlers;  // It can create command handlers by assigning their to array elements
    property ChannelCommandHandlers [const Command: String]: TCommandEvent read GetChannelCommandHandlers
      write SetChannelCommandHandlers;  // It can create command handlers by assigning their to array elements
    property BotUsername: String read FBotUsername write SetBotUsername;
    property LastErrorCode: Integer read FLastErrorCode write SetLastErrorCode;
    property LastErrorDescription: String read FLastErrorDescription write SetLastErrorDescription;

    property UpdateLogger: TtgStatLog read FUpdateLogger write SetUpdateLogger; //We will log the update object completely if need

    property OnReceiveUpdate: TOnUpdateEvent read FOnReceiveUpdate write SetOnReceiveUpdate;
    property OnReceiveMessage: TMessageEvent read FOnReceiveMessage write SetOnReceiveMessage;
    property OnReceiveCallbackQuery: TCallbackEvent read FOnReceiveCallbackQuery write SetOnReceiveCallbackQuery;
    property OnReceiveChannelPost: TMessageEvent read FOnReceiveChannelPost write SetOnReceiveChannelPost;
    property OnReceiveInlineQuery: TInlineQueryEvent read FOnReceiveInlineQuery write SetOnReceiveInlineQuery;
    property OnReceiveChosenInlineResult: TChosenInlineResultEvent read FOnReceiveChosenInlineResult write SetOnReceiveChosenInlineResult;
  end;

implementation

uses
  jsonparser, jsonscanner;
const
//  API names constants

  s_editMessageText='editMessageText';
  s_sendMessage='sendMessage';
  s_sendPhoto='sendPhoto';
  s_sendVideo='sendVideo';
  s_sendDocument='sendDocument';
  s_sendLocation='sendLocation';
  s_getUpdates='getUpdates';
  s_getMe='getMe';
  s_answerInlineQuery='answerInlineQuery';

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
  s_Latitude = 'latitude';
  s_Longitude = 'longitude';
  s_LivePeriod = 'live_period';
  s_DsblWbpgPrvw = 'disable_web_page_preview';
  s_InlineKeyboard = 'inline_keyboard';
  s_Keyboard = 'keyboard';
  s_ResizeKeyboard = 'resize_keyboard';
  s_OneTimeKeyboard = 'one_time_keyboard';
  s_RequestContact = 'request_contact';
  s_RequestLocation = 'request_location';
  s_SwitchInlineQuery = 'switch_inline_query';
  s_CallbackData = 'callback_data';
  s_SwitchInlineQueryCurrentChat = 's_switch_inline_query_current_chat';
  s_Selective = 'selective';
  s_ForceReply = 'force_reply';
  s_Offset = 'offset';
  s_Limit = 'limit';
  s_Timeout = 'timeout';
  s_AllowedUpdates = 'allowed_updates';
  s_Ok = 'ok';
  s_ErrorCode = 'error_code';
  s_Description = 'description';
  s_Result = 'result';
  s_BotCommand = 'bot_command';

  s_ID = 'id';
  s_InputMessageContent = 'input_message_content';
  s_Type = 'type';
  s_Title = 'title';

  s_MessageText = 'message_text';

  s_InlineQueryID = 'inline_query_id';
  s_Results = 'results';
  s_CacheTime = 'cache_time';
  s_IsPersonal = 'is_personal';
  s_NextOffset = 'next_offset';
  s_SwitchPmText = 'switch_pm_text';
  s_SwitchPmParameter = 'switch_pm_parameter';

  s_PhotoUrl ='photo_url';
  s_ThumbUrl ='thumb_url';
  s_VideoUrl ='video_url';
  s_MimeType ='mime_type';
  s_PhotoHeight = 'photo_height';
  s_PhotoWidth = 'photo_width';
  s_Mpeg4Url = 'mpeg4_url';
  s_Mpeg4Width = 'mpeg4_width';
  s_Mpeg4Height = 'mpeg4_height';


  ParseModes: array[TParseMode] of PChar = ('', 'Markdown', 'HTML');
  QueryResultTypeArray: array[TInlineQueryResultType] of PChar =
    ('article', 'photo', 'video', 'mpeg4_gif', '');

  API_URL='https://api.telegram.org/bot';

function StringToIQRType(const S: String): TInlineQueryResultType;
var
  iqrt: TInlineQueryResultType;
begin
  Result:=qrtUnknown;
  for iqrt:=Low(QueryResultTypeArray) to High(QueryResultTypeArray) do
    if SameStr(QueryResultTypeArray[iqrt], S) then
      Exit(iqrt);
end;

function StringToParseMode(const S: String): TParseMode;
var
  pm: TParseMode;
begin
  Result:=pmDefault;
  for pm:=Low(ParseModes) to High(ParseModes) do
    if SameStr(ParseModes[pm], S) then
      Exit(pm);
end;

{ TInputMessageContent }

function TInputMessageContent.GetMessageText: String;
begin
  Result:=Strings[s_MessageText];
end;

function TInputMessageContent.GetParseMode: TParseMode;
begin
  Result:=StringToParseMode(Get(s_ParseMode, EmptyStr));
end;

procedure TInputMessageContent.SetMessageText(AValue: String);
begin
  Strings[s_MessageText]:=AValue;
end;

procedure TInputMessageContent.SetParseMode(AValue: TParseMode);
begin
  Strings[s_ParseMode]:=ParseModes[AValue];
end;

constructor TInputMessageContent.Create(const AMessageText: String;
  AParseMode: TParseMode);
begin
  if AParseMode=pmDefault then
    inherited Create([s_MessageText, AMessageText])
  else
    inherited Create([s_MessageText, AMessageText, s_ParseMode, ParseModes[AParseMode]])
end;

{ TInlineQueryResult }

function TInlineQueryResult.GetDescription: String;
begin
  Result:=Get(s_Description, EmptyStr);
end;

function TInlineQueryResult.GetID: String;
begin
  Result:=Strings[s_ID];
end;

function TInlineQueryResult.GetInputMessageContent: TInputMessageContent;
begin
  Result:=Objects[s_InputMessageContent] as TInputMessageContent;
end;

function TInlineQueryResult.GetIQRType: TInlineQueryResultType;
begin
  Result:=StringToIQRType(Strings[s_Type]);
end;

function TInlineQueryResult.GetMimeType: String;
begin
  Result:=Get(s_MimeType, '');
end;

function TInlineQueryResult.GetMpeg4Height: Integer;
begin
  Result:=Get(s_Mpeg4Height, 0);
end;

function TInlineQueryResult.GetMpeg4Url: String;
begin
  Result:=Get(s_Mpeg4Url, '');
end;

function TInlineQueryResult.GetMpeg4Width: Integer;
begin
  Result:=Get(s_Mpeg4Width, 0);
end;

function TInlineQueryResult.GetPhotoHeight: Integer;
begin
  Result:=Get(s_PhotoHeight, 0);
end;

function TInlineQueryResult.GetPhotoUrl: String;
begin
  Result:=Get(s_PhotoUrl, '');
end;

function TInlineQueryResult.GetPhotoWidth: Integer;
begin
  Result:=Get(s_PhotoWidth, 0);
end;

function TInlineQueryResult.GetReplyMarkup: TReplyMarkup;
begin
  Result:=Find(s_ReplyMarkup, jtObject) as TReplyMarkup;
end;

function TInlineQueryResult.GetThumbUrl: String;
begin
  Result:=Get(s_ThumbUrl, '');
end;

function TInlineQueryResult.GetTitle: String;
begin
  Result:=Strings[s_Title];
end;

function TInlineQueryResult.GetVideoUrl: String;
begin
  Result:=Get(s_VideoUrl, '');
end;

procedure TInlineQueryResult.SetDescription(AValue: String);
begin
  Strings[s_Description]:=AValue;
end;

procedure TInlineQueryResult.SetID(AValue: String);
begin
  Strings[s_ID]:=AValue;
end;

procedure TInlineQueryResult.SetInputMessageContent(AValue: TInputMessageContent
  );
begin
  Objects[s_InputMessageContent]:=AValue;
end;

procedure TInlineQueryResult.SetIQRType(AValue: TInlineQueryResultType);
begin
  Strings[s_Type]:=QueryResultTypeArray[AValue];
end;

procedure TInlineQueryResult.SetMimeType(AValue: String);
begin
  Strings[s_MimeType]:=AValue;
end;

procedure TInlineQueryResult.SetMpeg4Height(AValue: Integer);
begin
  Integers[s_Mpeg4Height]:=AValue;
end;

procedure TInlineQueryResult.SetMpeg4Url(AValue: String);
begin
  Strings[s_Mpeg4Url]:=AValue;
end;

procedure TInlineQueryResult.SetMpeg4Width(AValue: Integer);
begin
  Integers[s_Mpeg4Height]:=AValue;
end;

procedure TInlineQueryResult.SetPhotoHeight(AValue: Integer);
begin
  Integers[s_PhotoHeight]:=AValue;
end;

procedure TInlineQueryResult.SetPhotoUrl(AValue: String);
begin
  Strings[s_PhotoUrl]:=AValue;
end;

procedure TInlineQueryResult.SetPhotoWidth(AValue: Integer);
begin
  Integers[s_PhotoWidth]:=AValue;
end;

procedure TInlineQueryResult.SetReplyMarkup(AValue: TReplyMarkup);
begin
  Objects[s_ReplyMarkup]:=AValue;
end;

procedure TInlineQueryResult.SetThumbUrl(AValue: String);
begin
  Strings[s_ThumbUrl]:=AValue;
end;

procedure TInlineQueryResult.SetTitle(AValue: String);
begin
  Strings[s_Title]:=AValue;
end;

procedure TInlineQueryResult.SetVideoUrl(AValue: String);
begin
  Strings[s_VideoUrl]:=AValue;
end;

{ TStringHash }

class function TStringHash.hash(s: String; n: Integer): Integer;
var
  c: Char;
begin
  Result := 0;
  for c in s do
    Inc(Result,Ord(c));
  Result := Result mod n;
end;

  { TKeyboardButtons }

function TKeyboardButtons.GetButtons(Index : Integer): TKeyboardButton;
begin
  Result:=Items[Index] as TKeyboardButton;
end;

procedure TKeyboardButtons.SetButtons(Index : Integer; AValue: TKeyboardButton);
begin
  Items[Index]:=AValue;
end;

constructor TKeyboardButtons.Create(const AText: String);
begin
  inherited Create;
  AddButton(AText);
end;

constructor TKeyboardButtons.Create(const AButtons: array of String);
begin
  inherited Create;
  AddButtons(AButtons);
end;

function TKeyboardButtons.AddButton(const AButtonText: String): Integer;
begin
  Result:=Add(TKeyboardButton.Create(AButtonText));
end;

procedure TKeyboardButtons.AddButtons(const AButtons: array of String);
var
  i: Integer;
begin
  for i:=0 to Length(AButtons)-1 do
    Add(TKeyboardButton.Create(AButtons[i]));
end;

{ TKeyboardButton }

function TKeyboardButton.GetRequestContact: Boolean;
begin
  Result:=Get(s_RequestContact, False);
end;

function TKeyboardButton.GetRequestLocation: Boolean;
begin
  Result:=Get(s_RequestLocation, False);
end;

function TKeyboardButton.Gettext: String;
begin
  Result:=Strings[s_text];
end;

procedure TKeyboardButton.SetRequestContact(AValue: Boolean);
begin
  Booleans[s_RequestContact]:=AValue;
end;

procedure TKeyboardButton.SetRequestLocation(AValue: Boolean);
begin
  Booleans[s_RequestLocation]:=AValue;
end;

procedure TKeyboardButton.Settext(AValue: String);
begin
  Strings[s_text]:=AValue;
end;

constructor TKeyboardButton.Create(const AText: String);
begin
  inherited Create;
  Add(s_text, AText);
end;

{ TInlineKeyboardButtons }

constructor TInlineKeyboardButtons.Create(const AButtonText,
  CallbackData: String);
begin
  inherited Create;
  AddButton(AButtonText, CallbackData);
end;

constructor TInlineKeyboardButtons.Create(const AButtons: array of String);
begin
  inherited Create;
  AddButtons(AButtons);
end;

function TInlineKeyboardButtons.AddButton(const AButtonText, CallbackData: String): Integer;
var
  btn: TInlineKeyboardButton;
begin
  btn:=TInlineKeyboardButton.Create(AButtonText);
  btn.callback_data:=CallbackData;
  Result:=Add(btn);
end;

procedure TInlineKeyboardButtons.AddButtons(const AButtons: array of String);
var
  btn: TInlineKeyboardButton;
  i, c: Integer;
begin
  c:=Length(AButtons) div 2;
  for i:=0 to c-1 do
  begin
    btn:=TInlineKeyboardButton.Create(AButtons[i*2]);
    btn.callback_data:=AButtons[i*2+1];
    Add(btn);
  end;
end;

{ TReplyMarkup }

function TReplyMarkup.GetForceReply: Boolean;
begin
  Result:=Booleans[s_ForceReply];
end;

function TReplyMarkup.GetInlineKeyBoard: TJSONArray;
begin
  Result:=Arrays[s_InlineKeyboard];
end;

function TReplyMarkup.GetOneTimeKeyboard: Boolean;
begin
  Result:=Get(s_OneTimeKeyboard, False);
end;

function TReplyMarkup.GetReplyKeyboardMarkup: TJSONArray;
begin
  Result:=Arrays[s_Keyboard];
end;

function TReplyMarkup.GetResizeKeyboard: Boolean;
begin
  Result:=Get(s_ResizeKeyboard, False);     // default False
end;

function TReplyMarkup.GetSelective: Boolean;
begin
  Result:=Get(s_Selective, False);     // default ??? False
end;

procedure TReplyMarkup.SetForceReply(AValue: Boolean);
begin
  if not AValue then
  begin
    if IndexOfName(s_ForceReply)>-1 then
      Delete(s_ForceReply);
    if IndexOfName(s_Selective)>-1 then
      Delete(s_Selective);
  end
  else
    Booleans[s_ForceReply]:=True;
end;

procedure TReplyMarkup.SetInlineKeyBoard(AValue: TJSONArray);
begin
  Arrays[s_InlineKeyboard]:=AValue;
end;

procedure TReplyMarkup.SetOneTimeKeyboard(AValue: Boolean);
begin
  Booleans[s_OneTimeKeyboard]:=AValue;
end;

procedure TReplyMarkup.SetReplyKeyboardMarkup(AValue: TJSONArray);
begin
  Arrays[s_Keyboard]:=AValue;
end;

procedure TReplyMarkup.SetResizeKeyboard(AValue: Boolean);
begin
  Booleans[s_ResizeKeyboard]:=AValue;
end;

procedure TReplyMarkup.SetSelective(AValue: Boolean);
begin
  if AValue then
    if not ForceReply then
      ForceReply:=True;
  Booleans[s_Selective]:=AValue;
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

class function TTelegramSender.StringToJSONObject(const AString: String): TJSONObject;
var
  lParser: TJSONParser;
begin
  Result := nil;
  if AString<>EmptyStr then
  begin
    lParser := TJSONParser.Create(AString, DefaultOptions);
    try
      try
        Result := lParser.Parse as TJSONObject
      except
        Result:=nil;
      end;
    finally
      lParser.Free;
    end;
  end;
end;

procedure TTelegramSender.DebugMessage(const Msg: String);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(Self, etDebug, Msg);
end;

function TTelegramSender.GetCommandHandlers(const Command: String
  ): TCommandEvent;
begin
  Result:=FCommandHandlers.Items[Command];
end;

procedure TTelegramSender.SetBotUsername(AValue: String);
begin
  if FBotUsername=AValue then Exit;
  FBotUsername:=AValue;
end;

function TTelegramSender.CurrentLanguage(AUser: TTelegramUserObj): String;
begin
  Result:=AUser.Language_code;
end;

function TTelegramSender.CurrentLanguage(AMessage: TTelegramMessageObj): String;
begin
  Result:=EmptyStr;
  if Assigned(AMessage.From) then
    Result:=AMessage.From.Language_code
  else begin
    if Assigned(AMessage.ReplyToMessage) then
      Result:=CurrentLanguage(AMessage.ReplyToMessage)
  end;
end;

function TTelegramSender.GetChannelCommandHandlers(const Command: String
  ): TCommandEvent;
begin
  Result:=FChannelCommandHandlers.Items[Command];
end;

procedure TTelegramSender.SetChannelCommandHandlers(const Command: String;
  AValue: TCommandEvent);
begin
  FChannelCommandHandlers.Items[Command]:=AValue;
end;

procedure TTelegramSender.SetCommandHandlers(const Command: String;
  AValue: TCommandEvent);
begin
  FCommandHandlers.Items[Command]:=AValue;
end;

procedure TTelegramSender.DoReceiveMessageUpdate(AMessage: TTelegramMessageObj);
begin
  FCurrentMessage:=AMessage;
  FCurrentChatID:=AMessage.ChatId;
  FCurrentUser:=AMessage.From;
  if CurrentIsBanned then
    Exit;
  SetLanguage(CurrentLanguage(AMessage));
  ProcessCommands(AMessage, FCommandHandlers);
  if Assigned(FOnReceiveMessage) then
    FOnReceiveMessage(Self, AMessage);
end;

procedure TTelegramSender.DoReceiveCallbackQuery(ACallback: TCallbackQueryObj);
begin
  FCurrentMessage:=ACallback.Message;
  FCurrentUser:=ACallback.From;
  FCurrentChatID:=FCurrentUser.ID; { Bot will send to private chat if in channel is called } {ACallback.Message.ChatId;}
  if CurrentIsBanned then
    Exit;
  SetLanguage(CurrentLanguage(ACallback.From));
  if Assigned(FCurrentMessage) then
    if Assigned(FCurrentMessage.From) then
      FBotUsername:=FCurrentMessage.From.Username;
  if Assigned(FOnReceiveCallbackQuery) then
    FOnReceiveCallbackQuery(Self, ACallback);
end;

procedure TTelegramSender.DoReceiveChannelPost(AChannelPost: TTelegramMessageObj);
begin
  FCurrentMessage:=AChannelPost;
  FCurrentChatID:=AChannelPost.ChatId;
  FCurrentUser:=AChannelPost.From;
  if CurrentIsBanned then
    Exit;
  SetLanguage(CurrentLanguage(AChannelPost));
  ProcessCommands(AChannelPost, FChannelCommandHandlers);
  if Assigned(FOnReceiveChannelPost) then
    FOnReceiveChannelPost(Self, AChannelPost);
end;

procedure TTelegramSender.DoReceiveInlineQuery(
  AnInlineQuery: TTelegramInlineQueryObj);
begin
  FCurrentMessage:=nil;
  FCurrentChatID:=AnInlineQuery.From.ID; // This is doubtful. It will be necessary to re-check
  FCurrentUser:=AnInlineQuery.From;
  if CurrentIsBanned then
    Exit;
  SetLanguage(CurrentLanguage(AnInlineQuery.From));
  if Assigned(FOnReceiveInlineQuery) then
    FOnReceiveInlineQuery(Self, AnInlineQuery);
end;

procedure TTelegramSender.DoReceiveChosenInlineResult(
  AChosenInlineResult: TTelegramChosenInlineResultObj);
begin
  FCurrentMessage:=nil;
  FCurrentChatID:=AChosenInlineResult.From.ID; // This is doubtful. It will be necessary to re-check
  FCurrentUser:=AChosenInlineResult.From;
  if CurrentIsBanned then
    Exit;
  SetLanguage(CurrentLanguage(AChosenInlineResult.From));
  if Assigned(FOnReceiveChosenInlineResult) then
    FOnReceiveChosenInlineResult(Self, AChosenInlineResult);
end;

procedure TTelegramSender.DoReceiveUpdate(AnUpdate: TTelegramUpdateObj);
begin
  if Assigned(FUpdate) then
    FUpdate.Free;
  FUpdate:=AnUpdate;
  FCurrentMessage:=nil;
  FCurrentChatId:=0;
  FCurrentUser:=nil;
  FLanguage:='';
  if Assigned(AnUpdate) then
  begin
    if FProcessUpdate then
    begin
      case AnUpdate.UpdateType of
        utMessage: DoReceiveMessageUpdate(AnUpdate.Message);
        utCallbackQuery: DoReceiveCallbackQuery(AnUpdate.CallbackQuery);
        utChannelPost: DoReceiveChannelPost(AnUpdate.ChannelPost);
        utInlineQuery: DoReceiveInlineQuery(AnUpdate.InlineQuery);
        utChosenInlineResult: DoReceiveChosenInlineResult(AnUpdate.ChosenInlineResult);
      end;
      if Assigned(FUpdateLogger) then
        if CurrentIsSimpleUser then  // This is to ensure that admins and moderators do not affect the statistics
          FUpdateLogger.Log(FUpdate.AsString);
    end;
    if Assigned(FOnReceiveUpdate) then
      FOnReceiveUpdate(Self, AnUpdate);
  end;
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

function TTelegramSender.IsBanned(ChatID: Int64): Boolean;
begin
  Result:=False;
end;

function TTelegramSender.IsSimpleUser(ChatID: Int64): Boolean;
begin
  Result:=True;
end;

function TTelegramSender.CurrentIsSimpleUser: Boolean;
begin
  Result:=IsSimpleUser(FCurrentChatId);
end;

function TTelegramSender.CurrentIsBanned: Boolean;
begin
  Result:=IsBanned(FCurrentChatId);
end;

function TTelegramSender.HTTPPostFile(const Method, FileField, FileName: String;
  AFormData: TStrings): Boolean;
var
  HTTP: TFPHTTPClient;
  AStream: TStringStream;
begin
  HTTP:=TFPHTTPClient.Create(nil);
  AStream:=TStringStream.Create(EmptyStr);
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

procedure TTelegramSender.ProcessCommands(AMessage: TTelegramMessageObj;
  AHandlers: TCommandHandlersMap);
var
  lCommand, Txt: String;
  lMessageEntityObj: TTelegramMessageEntityObj;
  H: TCommandEvent;
  AtPos: Integer;
begin
  Txt:=AMessage.Text;
  for lMessageEntityObj in AMessage.Entities do
  begin
    if (lMessageEntityObj.TypeEntity = s_BotCommand) and (lMessageEntityObj.Offset = 0) then
    begin
      lCommand := Copy(Txt, lMessageEntityObj.Offset, lMessageEntityObj.Length);
      AtPos:=Pos('@', lCommand);
      if AtPos>0 then
        lCommand:=LeftStr(lCommand, AtPos-1);
      if AHandlers.contains(lCommand) then
      begin
        H:=AHandlers.Items[lCommand];
        H(Self, lCommand, AMessage);
      end;
    end;
  end;
end;

function TTelegramSender.ResponseToJSONObject: TJSONObject;
begin
  Result:=StringToJSONObject(FResponse);
end;

function TTelegramSender.ResponseHandle: Boolean;
var
  lJSON: TJSONObject;
begin
  Result:=False;
  lJSON:=ResponseToJSONObject;
  if Assigned(lJSON) then
  begin
    if lJSON.Booleans[s_Ok] then
    begin
      JSONResponse := lJSON.Find(s_Result);
      FLastErrorCode:=0;
      FLastErrorDescription:='';
      Result:=True;
    end
    else begin
      FLastErrorCode:=lJSON.Integers[s_ErrorCode];
      FLastErrorDescription:=lJSON.Get(s_Description, EmptyStr);
    end;
    lJSON.Free;
  end;
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

procedure TTelegramSender.SetUpdateLogger(AValue: TtgStatLog);
begin
  if FUpdateLogger=AValue then Exit;
  FUpdateLogger:=AValue;
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
  JSONResponse:=nil;
  FResponse:='';
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

    if Result then
      if not ResponseHandle then
      begin
        Result:=False;
        ErrorMessage('Error request: '+FResponse);
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

procedure TTelegramSender.SetJSONResponse(AValue: TJSONData);
begin
  if FJSONResponse=AValue then Exit;
  if Assigned(FJSONResponse) then
    FJSONResponse.Free;
  if Assigned(AValue) then
    FJSONResponse:=AValue.Clone
  else
    FJSONResponse:=nil;
end;

procedure TTelegramSender.SetLastErrorCode(AValue: Integer);
begin
  if FLastErrorCode=AValue then Exit;
  FLastErrorCode:=AValue;
end;

procedure TTelegramSender.SetLastErrorDescription(AValue: String);
begin
  if FLastErrorDescription=AValue then Exit;
  FLastErrorDescription:=AValue;
end;

procedure TTelegramSender.SetLanguage(const AValue: String);
begin
  if FLanguage=AValue then Exit;
  FLanguage:=AValue;
end;

procedure TTelegramSender.SetOnReceiveCallbackQuery(AValue: TCallbackEvent);
begin
  if FOnReceiveCallbackQuery=AValue then Exit;
  FOnReceiveCallbackQuery:=AValue;
end;

procedure TTelegramSender.SetOnReceiveChannelPost(AValue: TMessageEvent);
begin
  if FOnReceiveChannelPost=AValue then Exit;
  FOnReceiveChannelPost:=AValue;
end;

procedure TTelegramSender.SetOnReceiveChosenInlineResult(
  AValue: TChosenInlineResultEvent);
begin
  if FOnReceiveChosenInlineResult=AValue then Exit;
  FOnReceiveChosenInlineResult:=AValue;
end;

procedure TTelegramSender.SetOnReceiveInlineQuery(AValue: TInlineQueryEvent);
begin
  if FOnReceiveInlineQuery=AValue then Exit;
  FOnReceiveInlineQuery:=AValue;
end;

procedure TTelegramSender.SetOnReceiveMessage(AValue: TMessageEvent);
begin
  if FOnReceiveMessage=AValue then Exit;
  FOnReceiveMessage:=AValue;
end;

procedure TTelegramSender.SetOnReceiveUpdate(AValue: TOnUpdateEvent);
begin
  if FOnReceiveUpdate=AValue then Exit;
  FOnReceiveUpdate:=AValue;
end;

procedure TTelegramSender.SetProcessUpdate(AValue: Boolean);
begin
  if FProcessUpdate=AValue then Exit;
  FProcessUpdate:=AValue;
end;

constructor TTelegramSender.Create(const AToken: String);
begin
  inherited Create;
  FToken:=AToken;
  FRequestWhenAnswer:=False;
  FProcessUpdate:=True;
  FCurrentChatId:=0;
  FCurrentUser:=nil;
  FCurrentMessage:=nil;
  FUpdate:=nil;
  FUpdateLogger:=nil;
  FJSONResponse:=nil;
  FLanguage:='';
  BotUsername:='';
  FCommandHandlers:=TCommandHandlersMap.create;
  FChannelCommandHandlers:=TCommandHandlersMap.create;

end;

destructor TTelegramSender.Destroy;
begin
  JSONResponse:=nil;
  if Assigned(FUpdateLogger) then
    FreeAndNil(FUpdateLogger);
  FChannelCommandHandlers.Free;
  FCommandHandlers.Free;
  if Assigned(FBotUser) then
    FBotUser.Free;
  if Assigned(FUpdate) then
    FUpdate.Free;
  inherited Destroy;
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

function TTelegramSender.editMessageText(const AMessage: String;
  ParseMode: TParseMode; DisableWebPagePreview: Boolean;
  ReplyMarkup: TReplyMarkup): Boolean;
begin  { try to edit message if the message is present and chat is private with sender user }
  if Assigned(FCurrentMessage) and (FCurrentChatId = FCurrentMessage.ChatId) then
    Result:=editMessageText(AMessage, FCurrentChatId, FCurrentMessage.MessageId,
      ParseMode, DisableWebPagePreview, EmptyStr, ReplyMarkup)
  else
    Result:=sendMessage(AMessage, ParseMode, DisableWebPagePreview, ReplyMarkup);
end;

function TTelegramSender.getMe: Boolean;
var
  sendObj: TJSONObject;
begin
  Result:=False;
  sendObj:=TJSONObject.Create;
  with sendObj do
  try
    Result:=SendMethod(s_getMe, sendObj);
    if Result then
      if Assigned(JSONResponse) then
      begin
        FBotUser := TTelegramUserObj.CreateFromJSONObject(JSONResponse as TJSONObject) as TTelegramUserObj;
        if Assigned(FBotUser) then
          FBotUsername:=FBotUser.Username;
      end;
  finally
    Free;
  end;
end;

// todo for long polling receiver
function TTelegramSender.getUpdates(offset: Int64; limit: Integer;
  timeout: Integer; allowed_updates: TUpdateSet): Boolean;
var
  sendObj: TJSONObject;
  lJSONArray: TJSONArray;
  lUpdateObj: TTelegramUpdateObj;
  lJSONEnum: TJSONEnum;
begin
  Result:=False;
  sendObj:=TJSONObject.Create;
  with sendObj do
  try
    if offset<>0 then
      Add(s_Offset, offset);
    if limit<>0 then    // if not specified then default[ = 100]
      Add(s_Limit, limit);
    if timeout<>0 then
      Add(s_Timeout, timeout);
    if allowed_updates <> [] then
      Add(s_AllowedUpdates, AllowedUpdatesToJSON(allowed_updates));
    FRequestWhenAnswer:=False; // You must do only HTTP request because because it's important to get a response in the form of an update array
    Result:=SendMethod(s_getUpdates, sendObj);
    if Result then
      if Assigned(JSONResponse) then
      begin
        lJSONArray:=JSONResponse as TJSONArray;
        for lJSONEnum in lJSONArray do
        begin
          lUpdateObj := TTelegramUpdateObj.CreateFromJSONObject(lJSONEnum.Value as TJSONObject) as TTelegramUpdateObj;
          DoReceiveUpdate(lUpdateObj);
        end;
      end;
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

function TTelegramSender.sendLocation(chat_id: Int64; Latitude,
  Longitude: Real; LivePeriod: Integer; ParseMode: TParseMode;
  DisableWebPagePreview: Boolean; ReplyMarkup: TReplyMarkup): Boolean;
var
  sendObj: TJSONObject;
begin
  Result:=False;
  sendObj:=TJSONObject.Create;
  with sendObj do
  try
    Add(s_ChatId, chat_id);
    Add(s_Latitude, Latitude);
    Add(s_Longitude, Longitude);
    if LivePeriod<>0 then
      Add(s_LivePeriod, LivePeriod);
    if ParseMode<>pmDefault then
      Add(s_ParseMode, ParseModes[ParseMode]);
    Add(s_DsblWbpgPrvw, DisableWebPagePreview);
    if Assigned(ReplyMarkup) then
      Add(s_ReplyMarkup, ReplyMarkup.Clone); // Clone of ReplyMarkup object will have released with sendObject
    Result:=SendMethod(s_sendLocation, sendObj);
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

function TTelegramSender.sendMessage(const chat_id, AMessage: String;
  ParseMode: TParseMode; DisableWebPagePreview: Boolean;
  ReplyMarkup: TReplyMarkup): Boolean;
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

function TTelegramSender.sendMessage(const AMessage: String;
  ParseMode: TParseMode; DisableWebPagePreview: Boolean;
  ReplyMarkup: TReplyMarkup): Boolean;
begin
  Result:=sendMessage(FCurrentChatId, AMessage, ParseMode, DisableWebPagePreview, ReplyMarkup);
end;

{ https://core.telegram.org/bots/api#sendphoto }
function TTelegramSender.sendPhoto(chat_id: Int64; const APhoto: String;
  const ACaption: String): Boolean;
begin
  Result:=SendMethod(s_sendPhoto, ['chat_id', chat_id, 'photo', APhoto, 'caption', ACaption]);
end;

function TTelegramSender.sendPhoto(const APhoto: String; const ACaption: String
  ): Boolean;
begin
  Result:=sendPhoto(FCurrentChatId, APhoto, ACaption);
end;

{ https://core.telegram.org/bots/api#sendvideo }
function TTelegramSender.sendVideo(chat_id: Int64; const AVideo: String;
  const ACaption: String): Boolean;
begin
  Result:=SendMethod(s_sendVideo, ['chat_id', chat_id, 'video', AVideo, 'caption', ACaption]);
end;

function TTelegramSender.sendVideo(const AVideo: String; const ACaption: String
  ): Boolean;
begin
  Result:=sendVideo(FCurrentChatId, AVideo, ACaption);
end;

function TTelegramSender.answerInlineQuery(const AnInlineQueryID: String;
  Results: TJSONArray; CacheTime: Integer; IsPersonal: Boolean;
  const NextOffset: String; const SwitchPmText: String;
  const SwitchPmParameter: String): Boolean;
begin  // todo: do not include default parameters... but is it really so necessary?
  Result:=SendMethod(s_answerInlineQuery, [s_InlineQueryID, AnInlineQueryID, s_Results, Results.Clone,
    s_CacheTime, CacheTime, s_IsPersonal, IsPersonal, s_NextOffset, NextOffset,
    s_switchPmText, SwitchPmText, s_SwitchPmParameter, SwitchPmParameter]);
end;

end.

