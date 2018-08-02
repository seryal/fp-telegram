unit longpoll_mthread;

// todo long polling

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpJSON, jsonparser, regexpr,
  Math, flqueue, tgtypes, tgsendertypes;

//The API will not allow more than ~30 messages to different users per second
//Also note that your bot will not be able to send more than 20 messages per minute to the same group.
//If you're sending bulk notifications to multiple users, the API will not allow more than 30 messages per second or so.
//error_code

type

  { TTGQueneProcessorThread }

  TTGQueneProcessorThread = class(TThread)
  private
    fToken: string;
    fQueue: TFLQueue;
    FBot: TTelegramSender;
    procedure SetBot(AValue: TTelegramSender);
  protected
    procedure Execute; override;
    property Bot: TTelegramSender read FBot write SetBot;
  public
    constructor Create(const aToken: string; aPower : NativeInt = 10); virtual;
    destructor Destroy; override;
    procedure AddUpdateObj(UpdateObj: TTelegramUpdateObj);
  end;

  { TTGLongPollThread }

  TTGLongPollThread = class(TThread)
  private
    FBot: TTelegramSender;
    fToken: string;
    fQueneProcessor: TTGQueneProcessorThread;
    FOffset: Integer;
  protected
    procedure QueneAddUpdate({%H-}ASender: TObject; AnUpdate: TTelegramUpdateObj);
    procedure Execute; override;
    property Bot: TTelegramSender read FBot write FBot;
  public
    constructor Create(const aToken: string); virtual;
    destructor Destroy; override;
    property Processor: TTGQueneProcessorThread read fQueneProcessor write fQueneProcessor;
  end;

const
  TELEGRAM_REQUEST_GETUPDATES = 'getUpdates';

implementation

{ TTGQueneProcessorThread }

procedure TTGQueneProcessorThread.SetBot(AValue: TTelegramSender);
begin
  if FBot=AValue then Exit;
  FBot:=AValue;
end;

procedure TTGQueneProcessorThread.Execute;
var
  lUpdateObj: TTelegramUpdateObj;
begin
  while not Terminated do
    while fQueue.length <> 0 do
    begin
      lUpdateObj := fQueue.pop as TTelegramUpdateObj;
      FBot.DoReceiveUpdate(TTelegramUpdateObj(lUpdateObj.Clone));
    end;
end;

constructor TTGQueneProcessorThread.Create(const aToken: string; aPower : NativeInt);
begin
  FreeOnTerminate := False;
  inherited Create(True);
  fToken := aToken;
  fQueue := TFLQueue.Create(aPower);
  FBot := TTelegramSender.Create(fToken);
end;

destructor TTGQueneProcessorThread.Destroy;
begin
  FBot.Free;
  fQueue.Free;
  inherited Destroy;
end;

procedure TTGQueneProcessorThread.AddUpdateObj(UpdateObj: TTelegramUpdateObj);
begin
  fQueue.push(UpdateObj);
end;

{ TTGLongPollThread }

procedure TTGLongPollThread.QueneAddUpdate(ASender: TObject;
  AnUpdate: TTelegramUpdateObj);
begin
  if Assigned(fQueneProcessor) then
    fQueneProcessor.AddUpdateObj(AnUpdate);
  FOffset := Max(FOffset, AnUpdate.UpdateId);
end;

procedure TTGLongPollThread.Execute;
begin
  FOffset:=0;
  while not Terminated do
  begin

//      GetUpdates(ASender);
    FBot.getUpdates(FOffset, 0, 10);

    if FOffset > 0 then
      Inc(FOffset);
  end;
end;

constructor TTGLongPollThread.Create(const aToken: string);
begin
  FreeOnTerminate := False;
  inherited Create(True);
  fToken := aToken;
//  fQueneProcessor := TTGQueneProcessorThread.Create(fToken, 10);
  FBot:=TTelegramSender.Create(fToken);
  FBot.OnReceiveUpdate:=@QueneAddUpdate;
  FBot.ProcessUpdate:=False;
end;

destructor TTGLongPollThread.Destroy;
begin
  FBot.Free;
  inherited Destroy;
end;

end.
