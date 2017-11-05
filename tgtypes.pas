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
  TTelegramUpdateObjList = specialize TFPGObjectList<TTelegramMessageEntityObj>;

  { TTelegramObj }

  TTelegramObj = class
  private
    fJSON: TJSONObject;
  public
    constructor Create(JSONObject: TJSONObject); virtual;
    class function CreateFromJSONObject(JSONObject: TJSONObject): TTelegramObj;
  end;

  TTelegramObjClass = class of TTelegramObj;

  { TTelegramUpdateObj }

  TTelegramUpdateObj = class(TTelegramObj)
  private
    FInlineQuery: TTelegramInlineQueryObj;
    fUpdateId: Integer;
    fMessage: TTelegramMessageObj;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property UpdateId: Integer read fUpdateId;
    property Message: TTelegramMessageObj read fMessage;
    property InlineQuery: TTelegramInlineQueryObj read FInlineQuery;
  end;

  { TTelegramMessageObj }

  TTelegramMessageObj = class(TTelegramObj)
  private
    fMessageId: Integer;
    fChatId: Integer;
    fText: string;
    fEntities: TTelegramUpdateObjList;
  public
    constructor Create(JSONObject: TJSONObject); override;
    destructor Destroy; override;
    property MessageId: Integer read fMessageId;
    property ChatId: Integer read fChatId;
    property Text: string read fText;
    property Entities: TTelegramUpdateObjList read fEntities;
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

  const
    TELEGRAM_REQUEST_GETUPDATES = 'getUpdates';

implementation

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

class function TTelegramObj.CreateFromJSONObject(JSONObject: TJSONObject): TTelegramObj;
begin
  try
    if Assigned(JSONObject) then
      Result := Self.Create(JSONObject)
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
  FInlineQuery:=TTelegramInlineQueryObj.CreateFromJSONObject(
    fJSON.Find('inline_query', jtObject) as TJSONObject) as TTelegramInlineQueryObj;
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

  lJSONArray := fJSON.Find('entities', jtArray) as TJSONArray;
  if Assigned(lJSONArray) then
    for lJSONEnum in lJSONArray do
      fEntities.Add(TTelegramMessageEntityObj.CreateFromJSONObject(lJSONEnum.Value as TJSONObject) as TTelegramMessageEntityObj);
end;

destructor TTelegramMessageObj.Destroy;
begin
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
