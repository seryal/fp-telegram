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
  TTelegramUserObj = class;
  TCallbackQueryObj = class;
  TTelegramLocation = class;
  TTelegramUpdateObjList = specialize TFPGObjectList<TTelegramMessageEntityObj>;

  TUpdateType = (utMessage, utCallbackQuery);

  { TTelegramObj }

  TTelegramObj = class
  private
    fJSON: TJSONObject;
  public
    constructor Create(JSONObject: TJSONObject); virtual;  // Caution! The JSONObject must be released separately
    destructor Destroy; override;
    class function CreateFromJSONObject(JSONObject: TJSONObject): TTelegramObj;
  end;

  { TTelegramUpdateObj }

  TTelegramUpdateObj = class(TTelegramObj)
  private
    FCallbackQuery: TCallbackQueryObj;
    FInlineQuery: TTelegramInlineQueryObj;
    fUpdateId: Integer;
    fMessage: TTelegramMessageObj;
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property UpdateId: Integer read fUpdateId;
    property Message: TTelegramMessageObj read fMessage;
    property CallbackQuery: TCallbackQueryObj read FCallbackQuery write FCallbackQuery;
    property InlineQuery: TTelegramInlineQueryObj read FInlineQuery;
  end;

  { TTelegramMessageObj }

  TTelegramMessageObj = class(TTelegramObj)
  private
    FFrom: TTelegramUserObj;
    FLocation: TTelegramLocation;
    fMessageId: Integer;
    fChatId: Integer;
    fText: string;
    fEntities: TTelegramUpdateObjList;
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property MessageId: Integer read fMessageId;
    property From: TTelegramUserObj read FFrom;
    property ChatId: Integer read fChatId;
    property Text: string read fText;
    property Entities: TTelegramUpdateObjList read fEntities;
    property Location: TTelegramLocation read FLocation;
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
    property Data: String read FData;  // optional
  end;

  { TTelegramInlineQueryObj }

  TTelegramInlineQueryObj = class(TTelegramObj)
  private
    FFrom: TTelegramUserObj;
    FID: String;
//    FLocation: TTelegramLocation;
    FOffset: String;
    FQuery: String;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property ID: String read FID;
    property From: TTelegramUserObj read FFrom;
//    property Location: TTelegramLocation read FLocation;
    property Query: String read FQuery;
    property Offset: String read FOffset;
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

  { TTelegramLocation }

  TTelegramLocation = class(TTelegramObj)
  private
    FLatitude: Real;
    FLongitude: Real;
    procedure SetLatitude(AValue: Real);
    procedure SetLongitude(AValue: Real);
  public
    constructor Create(JSONObject: TJSONObject); override;
    property Longitude: Real read FLongitude write SetLongitude;
    property Latitude: Real read FLatitude write SetLatitude;
  end;

  const
    TELEGRAM_REQUEST_GETUPDATES = 'getUpdates';

implementation

const

  UpdateTypeAliases: array[TUpdateType] of PChar = ('message', 'callback_query');

{ TTelegramLocation }

procedure TTelegramLocation.SetLatitude(AValue: Real);
begin
  if FLatitude=AValue then Exit;
  FLatitude:=AValue;
end;

procedure TTelegramLocation.SetLongitude(AValue: Real);
begin
  if FLongitude=AValue then Exit;
  FLongitude:=AValue;
end;

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

{ TTelegramUpdateObj }

constructor TTelegramUpdateObj.Create(JSONObject: TJSONObject);
begin
  inherited Create(JSONObject);
  fUpdateId := fJSON.Integers['update_id'];
  // объекты - не нашли?! - nil
  fMessage := TTelegramMessageObj.CreateFromJSONObject(
    fJSON.Find('message', jtObject) as TJSONObject) as TTelegramMessageObj;
  FCallbackQuery:=TCallbackQueryObj.CreateFromJSONObject(
    fJSON.Find('callback_query', jtObject) as TJSONObject) as TCallbackQueryObj;
  FInlineQuery:=TTelegramInlineQueryObj.CreateFromJSONObject(
    fJSON.Find('inline_query', jtObject) as TJSONObject) as TTelegramInlineQueryObj;
end;

destructor TTelegramUpdateObj.Destroy;
begin
  if Assigned(fMessage) then
    fMessage.Free;
  if Assigned(FCallbackQuery) then
    FCallbackQuery.Free;
  if Assigned(FInlineQuery) then
    FInlineQuery.Free;
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
  fChatId := fJSON.Objects['chat'].Integers['id'];
  // простые типы - не нашли?! - дефолтное значение
  fText := fJSON.Get('text', '');
  fEntities := TTelegramUpdateObjList.Create;

  FFrom:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('from', jtObject) as TJSONObject) as TTelegramUserObj;

  FLocation:=TTelegramUserObj.CreateFromJSONObject(fJSON.Find('location', jtObject) as TJSONObject) as TTelegramLocation;

  lJSONArray := fJSON.Find('entities', jtArray) as TJSONArray;
  if Assigned(lJSONArray) then
    for lJSONEnum in lJSONArray do
      fEntities.Add(TTelegramMessageEntityObj.CreateFromJSONObject(lJSONEnum.Value as TJSONObject) as TTelegramMessageEntityObj);
end;

destructor TTelegramMessageObj.Destroy;
begin
  if Assigned(FFrom) then
    FFrom.Free;
  if Assigned(FLocation) then
    FLocation.Free;
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
