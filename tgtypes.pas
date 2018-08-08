unit tgtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, jsonparser, fgl;

type
  TTelegramUpdateObj = class;
  TTelegramMessageObj = class;
  TTelegramMessageEntityObj = class;
  TTelegramInlineQueryObj = class;
  TTelegramChosenInlineResultObj = class;
  TTelegramUserObj = class;
  TTelegramChatObj = class;
  TCallbackQueryObj = class;
  TTelegramLocation = class;
  TArrayOfPhotoSize = class(TJSONArray);
  TTelegramPhotoSize = class;
  TTelegramUpdateObjList = specialize TFPGObjectList<TTelegramMessageEntityObj>;
  TTelegramPhotoSizeList = specialize TFPGObjectList<TTelegramPhotoSize>;

  TUpdateType = (utMessage, utEditedMessage, utChannelPost, utEditedChannelPost, utInlineQuery,
    utChosenInlineResult, utCallbackQuery, utShippingQuery, utPreCheckoutQuery, utUnknown);
  TChatType = (ctPrivate, ctGroup, ctSuperGroup, ctChannel, ctUnknown);
  TUpdateSet = set of TUpdateType;

  { TTelegramObj }

  TTelegramObj = class
  private
    fJSON: TJSONObject;
  public
    constructor Create(JSONObject: TJSONObject); virtual;  // Caution! The JSONObject must be released separately
    destructor Destroy; override;
    class function CreateFromJSONObject(JSONObject: TJSONObject): TTelegramObj;
    function AsString: String;
  end;

  { TTelegramUpdateObj }

  TTelegramUpdateObj = class(TTelegramObj)
  private
    { Once in this object (according to the description in Telegram bot API) can be present only one
      parameter then the field FUpdateParameter will be one }
    FUpdateParameter: TTelegramObj;
    fUpdateId: Integer;
    FUpdateType: TUpdateType;
    function GetCallbackQuery: TCallbackQueryObj;
    function GetChannelPost: TTelegramMessageObj;
    function GetChosenInlineResult: TTelegramChosenInlineResultObj;
    function GetEditedChannelPost: TTelegramMessageObj;
    function GetEditedMessage: TTelegramMessageObj;
    function GetInlineQuery: TTelegramInlineQueryObj;
    function GetMessage: TTelegramMessageObj;
    function ParseUpdateParameter: TUpdateType;
  public
    constructor Create(JSONObject: TJSONObject); override;
    function Clone: TTelegramUpdateObj;
    destructor Destroy; override;
    property UpdateId: Integer read fUpdateId;
    property UpdateType: TUpdateType read FUpdateType;
    property Message: TTelegramMessageObj read GetMessage;
    property EditedMessage: TTelegramMessageObj read GetEditedMessage;
    property CallbackQuery: TCallbackQueryObj read GetCallbackQuery;
    property InlineQuery: TTelegramInlineQueryObj read GetInlineQuery;
    property ChosenInlineResult: TTelegramChosenInlineResultObj read GetChosenInlineResult;
    property ChannelPost: TTelegramMessageObj read GetChannelPost;
    property EditedChannelPost: TTelegramMessageObj read GetEditedChannelPost;
  end;

  { TTelegramMessageObj }

  TTelegramMessageObj = class(TTelegramObj)
  private
    FChat: TTelegramChatObj;
    FFrom: TTelegramUserObj;
    FLocation: TTelegramLocation;
    fMessageId: Integer;
    fChatId: Int64;
    FPhoto: TTelegramPhotoSizeList;
    FReplyToMessage: TTelegramMessageObj;
    fText: string;
    fEntities: TTelegramUpdateObjList;
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property MessageId: Integer read fMessageId;
    property From: TTelegramUserObj read FFrom;
    property Chat: TTelegramChatObj read FChat;
    property ChatId: Int64 read fChatId;
    property ReplyToMessage: TTelegramMessageObj read FReplyToMessage;
    property Text: string read fText;
    property Entities: TTelegramUpdateObjList read fEntities;
    property Location: TTelegramLocation read FLocation;
    property Photo: TTelegramPhotoSizeList read FPhoto;
  end;

  { TTelegramMessageEntityObj }

  TTelegramMessageEntityObj = class(TTelegramObj)
  private
    fTypeEntity: string;
    fOffset: Integer;
    fLength: Integer;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property TypeEntity: string read fTypeEntity;
    property Offset: Integer read fOffset;
    property Length: Integer read fLength;
  end;

  { TCallbackQueryObj }

  TCallbackQueryObj = class(TTelegramObj)
  private
    FChatInstance: String;
    FData: String;
    FFrom: TTelegramUserObj;
    FID: String;
    FMessage: TTelegramMessageObj;
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property ID: String read FID;
    property From: TTelegramUserObj read FFrom;
    property Message: TTelegramMessageObj read FMessage;
    property ChatInstance: String read FChatInstance;
    property Data: String read FData;  // optional 1-64 bytes!!!
  end;

  { TTelegramInlineQueryObj }

  TTelegramInlineQueryObj = class(TTelegramObj)
  private
    FFrom: TTelegramUserObj;
    FID: String;
    FLocation: TTelegramLocation;
    FOffset: String;
    FQuery: String;
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property ID: String read FID;
    property From: TTelegramUserObj read FFrom;
    property Location: TTelegramLocation read FLocation;
    property Query: String read FQuery;
    property Offset: String read FOffset;
  end;

  { TTelegramChosenInlineResultObj }

  TTelegramChosenInlineResultObj = class(TTelegramObj)
  private
    FFrom: TTelegramUserObj;
    FInlineMessageID: String;
    FLocation: TTelegramLocation;
    FQuery: String;
    FResultID: String;
    procedure SetFrom(AValue: TTelegramUserObj);
    procedure SetInlineMessageID(AValue: String);
    procedure SetLocation(AValue: TTelegramLocation);
    procedure SetQuery(AValue: String);
    procedure SetResultID(AValue: String);
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property ResultID: String read FResultID;
    property From: TTelegramUserObj read FFrom;
    property Location: TTelegramLocation read FLocation;
    property InlineMessageID: String read FInlineMessageID;
    property Query: String read FQuery;
  end;

  { TTelegramUserObj }

  TTelegramUserObj = class(TTelegramObj)
  private
    FFirst_name: String;
    FID: Integer;
    FIs_bot: Boolean;
    FLanguage_code: String;
    FLast_name: String;
    FUsername: String;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property ID: Integer read FID;
    property Is_bot: Boolean read FIs_bot;
    property First_name: String read FFirst_name;
    property Last_name: String read FLast_name;
    property Username: String read FUsername;
    property Language_code: String read FLanguage_code;
  end;

  { TTelegramChatObj }

  TTelegramChatObj = class(TTelegramObj)
  private
    FChatType: TChatType;
    FFirst_name: String;
    FID: Int64;
    FLast_name: String;
    FTitle: String;
    FUsername: String;
    class function StringToChatType(const TypeString: String): TChatType;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property ID: Int64 read FID;
    property First_name: String read FFirst_name;
    property Last_name: String read FLast_name;
    property Username: String read FUsername;
    property ChatType: TChatType read FChatType;
    property Title: String read FTitle;
  end;

  { TTelegramLocation }

  TTelegramLocation = class(TTelegramObj)
  private
    FLatitude: Double;
    FLongitude: Double;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property Longitude: Double read FLongitude write FLongitude;
    property Latitude: Double read FLatitude write FLatitude;
  end;

  { TTelegramPhotoSize }

  TTelegramPhotoSize = class(TTelegramObj)
  private
    FFileID: String;
    FFileSize: Integer;
    FHeight: Integer;
    FWidth: Integer;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property FileID: String read FFileID;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property FileSize: Integer read FFileSize;
  end;

  { TTelegramFile }

  TTelegramFile = class(TTelegramObj)
  private
    FFileID: String;
    FFilePath: String;
    FFileSize: Integer;
  public
    constructor Create(JSONObject: TJSONObject); override;
    class function DownloadLink(const AFilePath, AToken: String): String;
    function DownloadLink(const AToken: String): String; overload;
    property FileID: String read FFileID;
    property FileSize: Integer read FFileSize;
    property FilePath: String read FFilePath; //  https://api.telegram.org/file/bot<token>/<file_path>
  end;



  TTelegramObjClass = class of TTelegramObj;

  const
    TELEGRAM_REQUEST_GETUPDATES = 'getUpdates';
    UpdateTypeAliases: array[TUpdateType] of PChar = ('message', 'edited_message', 'channel_post',
      'edited_channel_post', 'inline_query', 'chosen_inline_result', 'callback_query',
      'shipping_query', 'pre_checkout_query', '');
    UpdateTypeClasses: array[TUpdateType] of TTelegramObjClass = (TTelegramMessageObj,
      TTelegramMessageObj, TTelegramMessageObj, TTelegramMessageObj, TTelegramInlineQueryObj,
      TTelegramChosenInlineResultObj, TCallbackQueryObj, TTelegramObj, TTelegramObj, TTelegramObj);

function AllowedUpdatesToJSON(const AllowedUpdates: TUpdateSet): TJSONArray;

implementation

const
  API_URL_FILE='https://api.telegram.org/file/bot';

function AllowedUpdatesToJSON(const AllowedUpdates: TUpdateSet): TJSONArray;
var
  u: TUpdateType;
begin
  Result:=TJSONArray.Create;
  for u in AllowedUpdates do
    Result.Add(UpdateTypeAliases[u]);
end;

{ TTelegramFile }

constructor TTelegramFile.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FFileID := fJSON.Strings['file_id'];
  FFileSize := fJSON.Get('file_size', 0);
  FFilePath := fJSON.Get('file_path', '');
end;

class function TTelegramFile.DownloadLink(const AFilePath, AToken: String
  ): String;
begin
  Result:=API_URL_FILE+AToken+'/'+AFilePath;
end;

function TTelegramFile.DownloadLink(const AToken: String): String;
begin
  Result:=DownloadLink(FFilePath, AToken);
end;

{ TTelegramPhotoSize }

constructor TTelegramPhotoSize.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FFileID := fJSON.Strings['file_id'];
  FWidth := fJSON.Integers['width'];
  FHeight := fJSON.Integers['height'];
  FFileSize:=fJSON.Get('file_size', 0);
end;

{ TTelegramChatObj }

class function TTelegramChatObj.StringToChatType(const TypeString: String
  ): TChatType;
begin
  if TypeString='private' then
    Exit(ctPrivate);
  if TypeString='group' then
    Exit(ctGroup);
  if TypeString='supergroup' then
    Exit(ctPrivate);
  if TypeString='channel' then
    Exit(ctChannel);
  Result:=ctUnknown;
end;

constructor TTelegramChatObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FID := fJSON.Int64s['id'];
  FChatType:=StringToChatType(fJSON.Strings['type']);
  FFirst_name:=fJSON.Get('first_name', '');
  FLast_name:=fJSON.Get('last_name', '');
  FUsername:=fJSON.Get('username', '');
  FTitle:=fJSON.Get('title', '');
end;

{ TTelegramChosenInlineResultObj }

procedure TTelegramChosenInlineResultObj.SetFrom(AValue: TTelegramUserObj);
begin
  if FFrom=AValue then Exit;
  FFrom:=AValue;
end;

procedure TTelegramChosenInlineResultObj.SetInlineMessageID(AValue: String);
begin
  if FInlineMessageID=AValue then Exit;
  FInlineMessageID:=AValue;
end;

procedure TTelegramChosenInlineResultObj.SetLocation(AValue: TTelegramLocation);
begin
  if FLocation=AValue then Exit;
  FLocation:=AValue;
end;

procedure TTelegramChosenInlineResultObj.SetQuery(AValue: String);
begin
  if FQuery=AValue then Exit;
  FQuery:=AValue;
end;

procedure TTelegramChosenInlineResultObj.SetResultID(AValue: String);
begin
  if FResultID=AValue then Exit;
  FResultID:=AValue;
end;

constructor TTelegramChosenInlineResultObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FResultID := fJSON.Strings['result_id'];
  FFrom:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('from', jtObject) as TJSONObject) as TTelegramUserObj;
  FLocation:=TTelegramLocation.CreateFromJSONObject(fJSON.Find('location', jtObject) as TJSONObject) as TTelegramLocation;
  FInlineMessageID:=fJSON.Get('inline_message_id', '');
  FQuery:=fJSON.Strings['query'];
end;

destructor TTelegramChosenInlineResultObj.Destroy;
begin
  if Assigned(FLocation) then
    FreeAndNil(FLocation);
  if Assigned(FFrom) then
    FreeAndNil(FFrom);
  inherited Destroy;
end;

{ TTelegramLocation }

constructor TTelegramLocation.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FLongitude := fJSON.Floats['longitude'];
  FLatitude :=  fJSON.Floats['latitude'];
end;

{ TCallbackQueryObj }

constructor TCallbackQueryObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FID := fJSON.Strings['id'];
  FFrom:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('from', jtObject) as TJSONObject) as TTelegramUserObj;
  FMessage := TTelegramMessageObj.CreateFromJSONObject(
      fJSON.Find('message', jtObject) as TJSONObject) as TTelegramMessageObj;
  FChatInstance:= fJSON.Strings['chat_instance'];
  FData:=fJSON.Get('data', '');
end;

destructor TCallbackQueryObj.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FFrom) then
    FFrom.Free;
  inherited Destroy;
end;

{ TTelegramUserObj }

constructor TTelegramUserObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FID := fJSON.Integers['id'];
  FIs_bot := fJSON.Booleans['is_bot'];
  FFirst_name:=fJSON.Strings['first_name'];

  FLast_name:=fJSON.Get('last_name', '');
  FUsername:=fJSON.Get('username', '');
  FLanguage_code:=fJSON.Get('language_code', '');
end;

{ TTelegramInlineQueryObj }

constructor TTelegramInlineQueryObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  FID := fJSON.Strings['id'];
  FQuery:=fJSON.Get('query', '');
  FOffset:=fJSON.Get('offset', '');

  FFrom:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('from', jtObject) as TJSONObject) as TTelegramUserObj;
  FLocation:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('location', jtObject) as TJSONObject) as TTelegramLocation;
end;

destructor TTelegramInlineQueryObj.Destroy;
begin
  if Assigned(FLocation) then
    FLocation.Free;
  if Assigned(FFrom) then
    FFrom.Free;
  inherited Destroy;
end;

{ TTelegramObj }

constructor TTelegramObj.Create(JSONObject: TJSONObject);
begin
  fJSON := JSONObject.Clone as TJSONObject;
end;

destructor TTelegramObj.Destroy;
begin
  fJSON.Free;
  inherited Destroy;
end;

class function TTelegramObj.CreateFromJSONObject(JSONObject: TJSONObject): TTelegramObj;
begin
  try
    if Assigned(JSONObject) then
      Result := Create(JSONObject)
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

function TTelegramObj.AsString: String;
begin
  Result:=fJSON.AsJSON;
end;

{ TTelegramUpdateObj }

function TTelegramUpdateObj.ParseUpdateParameter: TUpdateType;
begin
  Result:=Low(TUpdateType);
  while (Result<utUnknown) and not Assigned(FUpdateParameter) do
  begin
    FUpdateParameter := UpdateTypeClasses[Result].CreateFromJSONObject(
      fJSON.Find(UpdateTypeAliases[Result], jtObject) as TJSONObject);
    if not Assigned(FUpdateParameter) then
      Inc(Result)
  end;
end;

function TTelegramUpdateObj.GetMessage: TTelegramMessageObj;
begin
  if FUpdateType=utMessage then
    Result:=TTelegramMessageObj(FUpdateParameter)
  else
    Result:=nil;
end;

function TTelegramUpdateObj.GetCallbackQuery: TCallbackQueryObj;
begin
  if FUpdateType=utCallbackQuery then
    Result:=TCallbackQueryObj(FUpdateParameter)
  else
    Result:=nil;
end;

function TTelegramUpdateObj.GetChannelPost: TTelegramMessageObj;
begin
  if FUpdateType=utChannelPost then
    Result:=TTelegramMessageObj(FUpdateParameter)
  else
    Result:=nil;
end;

function TTelegramUpdateObj.GetChosenInlineResult: TTelegramChosenInlineResultObj;
begin
  if FUpdateType=utChosenInlineResult then
    Result:=TTelegramChosenInlineResultObj(FUpdateParameter)
  else
    Result:=nil;
end;

function TTelegramUpdateObj.GetEditedChannelPost: TTelegramMessageObj;
begin
  if FUpdateType=utEditedChannelPost then
    Result:=TTelegramMessageObj(FUpdateParameter)
  else
    Result:=nil;
end;

function TTelegramUpdateObj.GetEditedMessage: TTelegramMessageObj;
begin
  if FUpdateType=utEditedMessage then
    Result:=TTelegramMessageObj(FUpdateParameter)
  else
    Result:=nil;
end;

function TTelegramUpdateObj.GetInlineQuery: TTelegramInlineQueryObj;
begin
  if FUpdateType=utInlineQuery then
    Result:=TTelegramInlineQueryObj(FUpdateParameter)
  else
    Result:=nil;
end;

constructor TTelegramUpdateObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  fUpdateId := fJSON.Integers['update_id'];
  FUpdateType:=ParseUpdateParameter;
end;

function TTelegramUpdateObj.Clone: TTelegramUpdateObj;
begin
  Result:=TTelegramUpdateObj.Create(fJSON);
end;

destructor TTelegramUpdateObj.Destroy;
begin
  if Assigned(FUpdateParameter) then
    FUpdateParameter.Free;
  inherited Destroy;
end;

{ TTelegramMessageObj }

constructor TTelegramMessageObj.Create(JSONObject: TJSONObject);
var
  lJSONArray: TJSONArray;
  lJSONEnum: TJSONEnum;
begin
  inherited Create(JSONObject);
  fMessageId := fJSON.Integers['message_id'];

  fText := fJSON.Get('text', '');
  fEntities := TTelegramUpdateObjList.Create;
  FPhoto := TTelegramPhotoSizeList.Create;


  FChat:=TTelegramChatObj.CreateFromJSONObject(fJSON.Find('chat', jtObject) as TJSONObject) as TTelegramChatObj;
  fChatId := fJSON.Objects['chat'].Int64s['id']; // deprecated?
  FFrom:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('from', jtObject) as TJSONObject) as TTelegramUserObj;

  FLocation:=TTelegramLocation.CreateFromJSONObject(fJSON.Find('location', jtObject) as TJSONObject) as TTelegramLocation;

  FReplyToMessage:=
    TTelegramMessageObj.CreateFromJSONObject(fJSON.Find('reply_to_message', jtObject) as TJSONObject)
    as TTelegramMessageObj;

  lJSONArray := fJSON.Find('entities', jtArray) as TJSONArray;
  if Assigned(lJSONArray) then
    for lJSONEnum in lJSONArray do
      fEntities.Add(TTelegramMessageEntityObj.CreateFromJSONObject(lJSONEnum.Value as TJSONObject) as TTelegramMessageEntityObj);

  lJSONArray := fJSON.Find('photo', jtArray) as TJSONArray;
  if Assigned(lJSONArray) then
    for lJSONEnum in lJSONArray do
      FPhoto.Add(TTelegramPhotoSize.CreateFromJSONObject(lJSONEnum.Value as TJSONObject) as TTelegramPhotoSize);
end;

destructor TTelegramMessageObj.Destroy;
begin
  if Assigned(FFrom) then
    FFrom.Free;
  if Assigned(FLocation) then
    FLocation.Free;
  if Assigned(FChat) then
    FChat.Free;
  if Assigned(FReplyToMessage) then
    FReplyToMessage.Free;
  FPhoto.Free;
  fEntities.Free;
  inherited Destroy;
end;

{ TTelegramMessageEntityObj }

constructor TTelegramMessageEntityObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  fTypeEntity := fJSON.Strings['type'];
  fOffset := fJSON.Integers['offset'];
  fLength := fJSON.Integers['length'];
end;

end.
