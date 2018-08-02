unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, tgtypes, IniPropStorage, eventlog, longpoll_mthread;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BtnStart: TButton;
    BtnStop: TButton;
    EdtToken: TLabeledEdit;
    InPrpStrg: TIniPropStorage;
    MmLog: TMemo;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLongPollThread: TTGLongPollThread;
    FProcessorThread: TTGQueneProcessorThread;
    procedure BotLogMessage({%H-}ASender: TObject; LogType: TEventType; const Msg: String);
  public

  end;

  { TProcessorThread }

  TProcessorThread = class(TTGQueneProcessorThread)
  private
    FLogMsg: String;
    procedure SynWriteLog;
    procedure BotStartCommandHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure BotLogMessage({%H-}ASender: TObject; LogType: TEventType; const Msg: String);
  public
    constructor Create(const aToken: string; aPower: NativeInt=10); override;
  end;

  { TReceiverThread }

  TReceiverThread = class(TTGLongPollThread)
  private
    FLogMsg: String;
    procedure SynWriteLog;
    procedure BotLogMessage({%H-}ASender: TObject; LogType: TEventType; const Msg: String);
  public
    constructor Create(const aToken: string); override;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  tgsendertypes;

var
  Logger: TEventLog;

{ TReceiverThread }

procedure TReceiverThread.SynWriteLog;
begin
  FrmMain.BotLogMessage(Bot, etDebug, FLogMsg);
end;

procedure TReceiverThread.BotLogMessage(ASender: TObject; LogType: TEventType;
  const Msg: String);
begin
  Logger.Log(LogType, Msg);
  FLogMsg:=Msg;
  Synchronize(@SynWriteLog);
end;

constructor TReceiverThread.Create(const aToken: string);
begin
  inherited Create(aToken);
  Bot.APIEndPoint:='https://api.telegram.org/bot';
  Bot.OnLogMessage:=@BotLogMessage;
end;

{ TProcessorThread }

procedure TProcessorThread.SynWriteLog;
begin
  FrmMain.BotLogMessage(Bot, etDebug, FLogMsg);
end;

procedure TProcessorThread.BotStartCommandHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.sendMessage('Hi! This is HelloWorld bot (Long Polling) developed in Lazarus');
end;

procedure TProcessorThread.BotLogMessage(ASender: TObject; LogType: TEventType;
  const Msg: String);
begin
  Logger.Log(LogType, Msg);
  FLogMsg:=Msg;
  Synchronize(@SynWriteLog);
end;

constructor TProcessorThread.Create(const aToken: string; aPower: NativeInt);
begin
  inherited Create(aToken, aPower);
  Bot.APIEndPoint:='https://api.telegram.org/bot';
  Bot.CommandHandlers['/start']:=@BotStartCommandHandler;
  Bot.OnLogMessage:=@BotLogMessage;
end;

{ TFrmMain }

procedure TFrmMain.BtnStartClick(Sender: TObject);
begin
  if EdtToken.Text=EmptyStr then
  begin
    ShowMessage('Please, enter telegram token!');
    Exit;
  end;
  FProcessorThread:=TProcessorThread.Create(EdtToken.Text);
  FProcessorThread.Start;

  FLongPollThread:=TReceiverThread.Create(EdtToken.Text);
  FLongPollThread.Processor:=FProcessorThread;
  FLongPollThread.Start;

  BtnStart.Enabled:=False;
  BtnStop.Enabled:=True;
end;

procedure TFrmMain.BtnStopClick(Sender: TObject);
begin
  BtnStop.Enabled:=False;
  FLongPollThread.Terminate;
  FLongPollThread.WaitFor;
  FreeAndNil(FLongPollThread);
  FProcessorThread.Terminate;
  FProcessorThread.WaitFor;
  FreeAndNil(FProcessorThread);
  BtnStart.Enabled:=True;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  Logger:=TEventLog.Create(Self);
  Logger.LogType:=ltFile;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FLongPollThread) then
  begin
    FLongPollThread.Terminate;
    FLongPollThread.WaitFor;
    FreeAndNil(FLongPollThread);
  end;
  if Assigned(FProcessorThread) then
  begin
    FProcessorThread.Terminate;
    FProcessorThread.WaitFor;
    FreeAndNil(FProcessorThread);
  end;
end;

procedure TFrmMain.BotLogMessage(ASender: TObject; LogType: TEventType;
  const Msg: String);
begin
  Logger.Log(LogType, Msg);
  MmLog.Lines.Add(Msg);
end;

end.

