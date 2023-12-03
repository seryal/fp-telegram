unit tgbot_dt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgbot, receiverthread, tgsendertypes
  ;

type

  { TCustomDTTelegramBot }

  TCustomDTTelegramBot = class(TComponent)
  private
    FActive: Boolean;
    FBotUsername: String;
    FHelpStrings: TStringList;
    FLongPollingTime: Integer;
    FOnReceiveCallack: TCallbackEvent;
    FOnReceiveEditedMessage: TMessageEvent;
    FOnReceiveMessageUpdate: TMessageEvent;
    FOnReceiveUpdate: TOnUpdateEvent;
    FReceiver: TLongPollingThread;
    FSenderBot: TTelegramSender;
    FStartStrings: TStringList;
    FToken: String;
    function GetHelpText: TStrings;
    function GetStartText: TStrings;
    procedure SetHelpText(AValue: TStrings);
    procedure SetStartText(AValue: TStrings);
    procedure SetActive(AValue: Boolean);
  public
    procedure BotgetMe;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartReceiver;
    procedure StopReceiver;
    property Active: Boolean read FActive write SetActive;
    { It can be arrived with getMe command for example or specified manually.
      Required for some commands }
    property BotUsername: String read FBotUsername write FBotUsername;
    property Token: String read FToken write FToken;
    property StartText: TStrings read GetStartText write SetStartText;
    property HelpText: TStrings read GetHelpText write SetHelpText;
    property LongPollingTime: Integer read FLongPollingTime write FLongPollingTime;
    property OnReceiveCallack: TCallbackEvent read FOnReceiveCallack write FOnReceiveCallack;
    property OnReceiveMessageUpdate: TMessageEvent read FOnReceiveMessageUpdate write FOnReceiveMessageUpdate; 
    property OnReceiveEditedMessage: TMessageEvent read FOnReceiveEditedMessage write FOnReceiveEditedMessage;
    { This event will be triggered if none of the other events have been handled (UpdateProcessed = True) after
      receiving the update from the telegram server. }
    property OnReceiveUpdate: TOnUpdateEvent read FOnReceiveUpdate write FOnReceiveUpdate;
  end;

implementation

uses
  Dialogs
  ;

type
  EDTTelegramBot = class(Exception);

const
  _StartText = 'Welcome! This is start text for `/start` command.'+LineEnding+
    'You can change this text by the property `TDTLongPolBot.StartText`.'+LineEnding+
    'Caution: it is markdown markup';
  _HelpText = 'This is help text for `/help` command.'+LineEnding+
    'You can change this text by the property `TDTLongPolBot.HelpText`.'+LineEnding+
    'Caution: it is markdown markup';


{ TCustomDTTelegramBot }

procedure TCustomDTTelegramBot.StartReceiver;
begin
  if FActive then
    Exit;
  if FToken.IsEmpty then
    raise EDTTelegramBot.Create('Token for telegram bot is not specified!');
  FActive:=True;
  FReceiver:=TLongPollingThread.Create;
  FReceiver.FreeOnTerminate:=False;
  FReceiver.LongpollingTimeout:=LongPollingTime;
  FReceiver.Bot.LogDebug:=True;
  FReceiver.Bot.Token:=FToken;
  FReceiver.Bot.StartText:=FStartStrings.Text;
  FReceiver.Bot.HelpText:=FHelpStrings.Text;
  FReceiver.OnReceiveMessageUpdate:=FOnReceiveMessageUpdate;
  FReceiver.OnReceiveEditedMessage:=FOnReceiveEditedMessage;
  FReceiver.OnReceiveCallack:=FOnReceiveCallack;
  FReceiver.OnReceiveUpdate:=FOnReceiveUpdate;
  FReceiver.Start;
end;

function TCustomDTTelegramBot.GetHelpText: TStrings;
begin
  Result:=FHelpStrings;
end;

function TCustomDTTelegramBot.GetStartText: TStrings;
begin
  Result:=FStartStrings;
end;

procedure TCustomDTTelegramBot.SetHelpText(AValue: TStrings);
begin
  FHelpStrings.Assign(AValue);
end;

procedure TCustomDTTelegramBot.SetStartText(AValue: TStrings);
begin
  FStartStrings.Assign(AValue);
end;

procedure TCustomDTTelegramBot.StopReceiver;
begin
  if not FActive then
    Exit;
  FReceiver.Terminate;
  FReceiver.WaitFor;
  FActive:=False;
  FreeAndNil(FReceiver);
end;

procedure TCustomDTTelegramBot.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  if AValue then
    StartReceiver
  else
    StopReceiver;
end;

constructor TCustomDTTelegramBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartStrings:=TStringList.Create;
  FHelpStrings:=TStringList.Create;
  FLongPollingTime:=4; // secs! 0 is not recommended
  FStartStrings.Text:=_StartText;
  FHelpStrings.Text:= _HelpText;
end;

destructor TCustomDTTelegramBot.Destroy;
begin
  FHelpStrings.Free;
  FStartStrings.Free;
  StopReceiver;
  inherited Destroy;
end;

procedure TCustomDTTelegramBot.BotgetMe;
begin
  FSenderBot:=TTelegramSender.Create(Token);
  try
    if FSenderBot.getMe then
    begin
      FBotUsername:=FSenderBot.BotUsername;
      if (csDesigning in ComponentState) then
        with FSenderBot.BotUser do
          MessageDlg('Telegram response for getMe',
            Format('Bot username: %s. User_id: %d. First_Name: %s', [Username, ID, First_name]), mtInformation, [mbOK],
            '');
    end
    else
      raise EDTTelegramBot.Create('Unsuccessful request!');
  finally
    FSenderBot.Free;
  end;
end;

end.
