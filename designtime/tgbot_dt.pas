unit tgbot_dt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgbot, receiverthread, tgsendertypes, fpjson
  ;

type

  { TInlineKeyboardButtonItem }

  TInlineKeyboardButtonItem = class(TCollectionItem)
  private
    FCallbackData: String;
    FSwitchInlineQuery: String;
    FText: String;
    FUrl: String;
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Text: String read FText write FText;
    property Url: String read FUrl write FUrl;
    property CallbackData: String read FCallbackData write FCallbackData;
    property SwitchInlineQuery: String read FSwitchInlineQuery write FSwitchInlineQuery;
  end;

  TInlineButtonRowItem = class;

  { TInlineButtonCollection }

  TInlineButtonCollection = class(TOwnedCollection)
  public
    constructor Create(aOwner: TInlineButtonRowItem);
  end;

  { TInlineButtonRowItem }

  TInlineButtonRowItem = class(TCollectionItem)
  private
    FButtonRows: TInlineButtonCollection;
    procedure SetButtonRows(AValue: TInlineButtonCollection);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property ButtonRows: TInlineButtonCollection read FButtonRows write SetButtonRows;
  end;

  TReplyMarkupItem = class;

  { TInlineKeyboardCollection }

  TInlineKeyboardCollection = class(TOwnedCollection)
  public
    constructor Create(aOwner: TReplyMarkupItem);
  end;

  { TReplyMarkupItem }

  TReplyMarkupItem = class(TCollectionItem)
  private
    FForceReply: Boolean;
    FInlineKeyBoard: TInlineKeyboardCollection;
    FInputFieldPlaceholder: String;
    FOneTimeKeyboard: Boolean;
    FRemoveKeyboard: Boolean;
    FResizeKeyboard: Boolean;
    FSelective: Boolean;
    procedure SetInlineKeyBoard(AValue: TInlineKeyboardCollection);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    { Caution: this is an object constructor.
        While there is no way for streaming to JSON nested array (or is there one?)
        we manually create jsonreplymarkup }
    function GetJSONReplyMarkup: TReplyMarkup;
  published
    property ForceReply: Boolean read FForceReply write FForceReply;
    property InlineKeyBoard: TInlineKeyboardCollection read FInlineKeyBoard write SetInlineKeyBoard;
    property RemoveKeyboard: Boolean read FRemoveKeyboard write FRemoveKeyboard;
    property ResizeKeyboard: Boolean read FResizeKeyboard write FResizeKeyboard;
    property OneTimeKeyboard: Boolean read FOneTimeKeyboard write FOneTimeKeyboard;
    property InputFieldPlaceholder: String read FInputFieldPlaceholder write FInputFieldPlaceholder;
    property Selective: Boolean read FSelective write FSelective;
  end;

  TReplyMarkupItemClass = class of TReplyMarkupItem;

  TCustomDTTelegramBot = class;

  { TReplyMarkupCollection }

  TReplyMarkupCollection = class(TOwnedCollection)
  private
    function GetReplyMarkup(aIndex: Integer): TReplyMarkupItem;
  public
    constructor Create(aOwner: TCustomDTTelegramBot);
    property ReplyMarkups[aIndex: Integer]: TReplyMarkupItem read GetReplyMarkup;
  end;


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
    FReplyMarkup: TReplyMarkupCollection;
    FSenderBot: TTelegramSender;
    FStartStrings: TStringList;
    FToken: String;
    function GetHelpText: TStrings;
    function GetSenderBot: TTelegramSender;
    function GetStartText: TStrings;
    procedure RaiseIfNoToken;
    procedure SetHelpText(AValue: TStrings);
    procedure SetReplyMarkups(AValue: TReplyMarkupCollection);
    procedure SetStartText(AValue: TStrings);
    procedure SetActive(AValue: Boolean);
  public
    procedure BotgetMe;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartReceiver;
    procedure StopReceiver;
    property SenderBot: TTelegramSender read GetSenderBot;
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
    property ReplyMarkups: TReplyMarkupCollection read FReplyMarkup write SetReplyMarkups;
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

{ TInlineKeyboardButtonItem }

procedure TInlineKeyboardButtonItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TInlineKeyboardButtonItem then
  begin                                                        
    TInlineKeyboardButtonItem(Dest).Text:=Text;
    TInlineKeyboardButtonItem(Dest).CallbackData:=CallbackData;
    TInlineKeyboardButtonItem(Dest).SwitchInlineQuery:=SwitchInlineQuery;
    TInlineKeyboardButtonItem(Dest).Url:=Url;
  end
  else
    inherited AssignTO(Dest);
end;

{ TInlineButtonRowItem }

procedure TInlineButtonRowItem.SetButtonRows(AValue: TInlineButtonCollection);
begin
  FButtonRows.Assign(AValue);
end;

constructor TInlineButtonRowItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FButtonRows:=TInlineButtonCollection.Create(Self);
end;

destructor TInlineButtonRowItem.Destroy;
begin
  FButtonRows.Free;
  inherited Destroy;
end;

procedure TInlineButtonRowItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TInlineButtonRowItem then
    TInlineButtonRowItem(Dest).FButtonRows.Assign(FButtonRows)
  else
    inherited AssignTO(Dest);
end;

{ TInlineButtonCollection }

constructor TInlineButtonCollection.Create(aOwner: TInlineButtonRowItem);
begin
  inherited Create(aOwner, TInlineKeyboardButtonItem);
end;

{ TInlineKeyboardCollection }

constructor TInlineKeyboardCollection.Create(aOwner: TReplyMarkupItem);
begin
  inherited Create(aOwner, TInlineButtonRowItem);
end;

{ TReplyMarkupItem }

function TReplyMarkupItem.GetJSONReplyMarkup: TReplyMarkup;
var
  aRowItem, aButtonItem: TCollectionItem;
  aInlineKeyboard: TInlineKeyboard;
  aRow: TInlineKeyboardButtons;

  procedure AddKeyboardButton(aButton: TInlineKeyboardButtonItem);
  begin
    if aButton.CallbackData<>EmptyStr then
    begin
      aRow.AddButton(aButton.Text, aButton.CallbackData);
      Exit;
    end;
    if aButton.Url<>EmptyStr then
    begin
      aRow.AddButtonUrl(aButton.Text, aButton.Url);
      Exit;
    end;   
    if aButton.SwitchInlineQuery<>EmptyStr then
      aRow.AddButtonInline(aButton.Text, aButton.SwitchInlineQuery)
  end;

begin
  Result:=TReplyMarkup.Create;
  if FForceReply then
  begin
    Result.ForceReply:=True;
    Result.InputFieldPlaceholder:=FInputFieldPlaceholder;
    Result.Selective:=FSelective;
    Exit;
  end;
  if FInlineKeyBoard.Count>0 then
  begin
    aInlineKeyboard:=Result.CreateInlineKeyBoard;
    for aRowItem in FInlineKeyBoard do
    begin
      aRow:=aInlineKeyboard.Add;
      for aButtonItem in (aRowItem as TInlineButtonRowItem).ButtonRows do
        AddKeyboardButton(aButtonItem as TInlineKeyboardButtonItem);
    end;
  end;
end;

procedure TReplyMarkupItem.SetInlineKeyBoard(AValue: TInlineKeyboardCollection);
begin
  FInlineKeyBoard.Assign(AValue);
end;

constructor TReplyMarkupItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FInlineKeyBoard:=TInlineKeyboardCollection.Create(Self);
end;

destructor TReplyMarkupItem.Destroy;
begin
  FInlineKeyBoard.Free;
  inherited Destroy;
end;

procedure TReplyMarkupItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TReplyMarkupItem then
  begin
    TReplyMarkupItem(Dest).FForceReply:=FForceReply;
    TReplyMarkupItem(Dest).FInlineKeyBoard.Assign(FInlineKeyBoard);
    //TReplyMarkupItem(Dest).ReplyKeyboardMarkup:=ReplyKeyboardMarkup.Clone;
    TReplyMarkupItem(Dest).FOneTimeKeyboard:=FOneTimeKeyboard;
    TReplyMarkupItem(Dest).FRemoveKeyboard:=FRemoveKeyboard;
    TReplyMarkupItem(Dest).FResizeKeyboard:=FResizeKeyboard;
    TReplyMarkupItem(Dest).FInputFieldPlaceholder:=FInputFieldPlaceholder;
    TReplyMarkupItem(Dest).FSelective:=FSelective;
  end
  else
    inherited AssignTO(Dest);
end;

{ TReplyMarkupCollection }

function TReplyMarkupCollection.GetReplyMarkup(aIndex: Integer): TReplyMarkupItem;
begin
  Result:=TReplyMarkupItem(Items[aIndex]);
end;

constructor TReplyMarkupCollection.Create(aOwner: TCustomDTTelegramBot);
begin
  inherited Create(aOwner, TReplyMarkupItem);
end;


{ TCustomDTTelegramBot }

procedure TCustomDTTelegramBot.StartReceiver;
begin
  if FActive then
    Exit;
  RaiseIfNoToken;
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

procedure TCustomDTTelegramBot.RaiseIfNoToken;
begin
  if FToken.IsEmpty then
    raise EDTTelegramBot.Create('Token for telegram bot is not specified!');
end;

function TCustomDTTelegramBot.GetHelpText: TStrings;
begin
  Result:=FHelpStrings;
end;

function TCustomDTTelegramBot.GetSenderBot: TTelegramSender;
begin
  if not Assigned(FSenderBot) then
    FSenderBot:=TTelegramSender.Create(FToken);
  Result:=FSenderBot;
end;

function TCustomDTTelegramBot.GetStartText: TStrings;
begin
  Result:=FStartStrings;
end;

procedure TCustomDTTelegramBot.SetHelpText(AValue: TStrings);
begin
  FHelpStrings.Assign(AValue);
end;

procedure TCustomDTTelegramBot.SetReplyMarkups(AValue: TReplyMarkupCollection);
begin
  FReplyMarkup.Assign(AValue);
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
  FReplyMarkup:=TReplyMarkupCollection.Create(Self);
end;

destructor TCustomDTTelegramBot.Destroy;
begin
  FReplyMarkup.Free;
  FSenderBot.Free;
  FHelpStrings.Free;
  FStartStrings.Free;
  StopReceiver;
  inherited Destroy;
end;

procedure TCustomDTTelegramBot.BotgetMe;
begin
  RaiseIfNoToken;
  if SenderBot.getMe then
  begin
    BotUsername:=FSenderBot.BotUsername;
    if (csDesigning in ComponentState) then
      with FSenderBot.BotUser do
        MessageDlg('Telegram response for getMe',
          Format('Bot username: %s. User_id: %d. First_Name: %s', [Username, ID, First_name]), mtInformation, [mbOK],
          '');
  end
  else
    raise EDTTelegramBot.Create('Unsuccessful request!');
end;

end.
