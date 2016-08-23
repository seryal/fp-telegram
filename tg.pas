unit tg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpJSON, jsonparser, SyncObjs, regexpr, fgl, gqueue,
  fphttpclient, Math, LazLogger, flqueue;

//The API will not allow more than ~30 messages to different users per second
//Also note that your bot will not be able to send more than 20 messages per minute to the same group.
//If you're sending bulk notifications to multiple users, the API will not allow more than 30 messages per second or so.
//error_code

type
  TTelegramUpdateObj = class;
  TTelegramMessageObj = class;
  TTelegramMessageEntityObj = class;
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
    fUpdateId: Integer;
    fMessage: TTelegramMessageObj;
  public
    constructor Create(JSONObject: TJSONObject); override;
    property UpdateId: Integer read fUpdateId;
    property Message: TTelegramMessageObj read fMessage;
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


  { TTGQueueRequestsThread }

  TTGQueneProcessorThread = class(TThread)
  private
    fToken: string;
    fQueue: TFLQueue;
  protected
    procedure Execute; override;
  public
    constructor Create(const aToken: string; aPower : NativeInt = 10);
    destructor Destroy; override;
    procedure AddUpdateObj(UpdateObj: TTelegramUpdateObj);
  end;

  { TTGLongPollThread }

  TTGLongPollThread = class(TThread)
  private
    fToken: string;
    fQueneProcessor: TTGQueneProcessorThread;
  protected
    function StreamToJSONObject(Stream: TMemoryStream): TJSONObject;
    function GetUpdates(HTTPClient: TFPHTTPClient; aOffset: Integer): Integer;
    procedure Execute; override;
  public
    constructor Create(const aToken: string);
    destructor Destroy; override;
  end;

const
  TELEGRAM_REQUEST_GETUPDATES = 'getUpdates';

implementation

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
  fMessage := TTelegramMessageObj.CreateFromJSONObject(fJSON.Find('message', jtObject) as TJSONObject) as TTelegramMessageObj;
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

{ TTGQueneProcessorThread }

procedure TTGQueneProcessorThread.Execute;
var
  lUpdateObj: TTelegramUpdateObj;
  lMessageEntityObj: TTelegramMessageEntityObj;
  lHTTPClient: TFPHTTPClient;
  lCommand: string;
begin
  lHTTPClient := TFPHTTPClient.Create(nil);
  try
    while not Terminated do
      while fQueue.length <> 0 do
      begin
        lUpdateObj := fQueue.pop as TTelegramUpdateObj;
        if Assigned(lUpdateObj.Message) then
          for lMessageEntityObj in lUpdateObj.Message.Entities do
            if (lMessageEntityObj.TypeEntity = 'bot_command') and (lMessageEntityObj.Offset = 0) then
            begin
              lCommand := Copy(lUpdateObj.Message.Text, lMessageEntityObj.Offset, lMessageEntityObj.Length);
              if lCommand = '/help' or lCommand = '/start' then
              begin
                lHTTPClient.Get('https://api.telegram.org/bot' + fToken + '/sendMessage?chat_id=' + IntToStr(lUpdateObj.Message.ChatId) + '&parse_mode=Markdown&text=' +
                EncodeURLElement(
                  '*Привет!' + #$F0#$9F#$98#$81 + 'Я умеею показывать расписание.*') );
              end;
            end;
      end;
  finally
    lHTTPClient.Free;
  end;
end;

constructor TTGQueneProcessorThread.Create(const aToken: string; aPower : NativeInt);
begin
  FreeOnTerminate := False;
  inherited Create(False);
  fToken := aToken;
  fQueue := TFLQueue.Create(10);
end;

destructor TTGQueneProcessorThread.Destroy;
begin
  fQueue.Free;
  inherited Destroy;
end;

procedure TTGQueneProcessorThread.AddUpdateObj(UpdateObj: TTelegramUpdateObj);
begin
  fQueue.push(UpdateObj);
end;

{ TTGLongPollThread }

function TTGLongPollThread.StreamToJSONObject(Stream: TMemoryStream): TJSONObject;
var
  lParser: TJSONParser;
  lJSON: TJSONObject;
begin
  Result := nil;
  if Stream.Size > 0 then
  begin
    Stream.Position := 0;
    lParser := TJSONParser.Create(Stream);
    try
      try
        lJSON := lParser.Parse as TJSONObject;
        if lJSON.Booleans['ok'] then
          Result := lJSON;
      except
      end;
    finally
      lParser.Free;
    end;
  end;
end;

function TTGLongPollThread.GetUpdates(HTTPClient: TFPHTTPClient; aOffset: Integer): Integer;
var
  lData: TMemoryStream;
  lJSON: TJSONObject;
  lJSONArray: TJSONArray;
  lJSONEnum: TJSONEnum;
  lUpdateObj: TTelegramUpdateObj;
begin
  Result := 0;
  lData := TMemoryStream.Create;
  try
  if aOffset > 0 then
    HTTPClient.Get('https://api.telegram.org/bot' + fToken + '/getUpdates?offset=' + IntToStr(aOffset) + '&timeout=30', lData)
  else
    HTTPClient.Get('https://api.telegram.org/bot' + fToken + '/getUpdates?timeout=30', lData);
  lJSON := StreamToJSONObject(lData);
  if Assigned(lJSON) then
    try
      lJSONArray := lJSON.Find('result', jtArray) as TJSONArray;
      if Assigned(lJSONArray) then
        for lJSONEnum in lJSONArray do
        begin
          lUpdateObj := TTelegramUpdateObj.CreateFromJSONObject(lJSONEnum.Value as TJSONObject) as TTelegramUpdateObj;
          fQueneProcessor.AddUpdateObj(lUpdateObj);
          Result := Max(Result, lUpdateObj.UpdateId);
        end;
    except
    end;
  lData.Clear;
  finally
    lData.Free;
  end;
end;

procedure TTGLongPollThread.Execute;
var
  lOffset: Integer;
  lHTTPClient: TFPHTTPClient;
begin
  lHTTPClient := TFPHTTPClient.Create(nil);
  try
    while not Terminated do
    begin
      lOffset := GetUpdates(lHTTPClient, lOffset);
      // next!
      if lOffset > 0 then
        lOffset := lOffset + 1;
    end;
  finally
    lHTTPClient.Free;
  end;
end;

constructor TTGLongPollThread.Create(const aToken: string);
begin
  FreeOnTerminate := False;
  inherited Create(False);
  fToken := aToken;
  fQueneProcessor := TTGQueneProcessorThread.Create(fToken, 10);
end;

destructor TTGLongPollThread.Destroy;
begin
  fQueneProcessor.Terminate;
  fQueneProcessor.WaitFor;
  fQueneProcessor.Free;
  inherited Destroy;
end;

end.
