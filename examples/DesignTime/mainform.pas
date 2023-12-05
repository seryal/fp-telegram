unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DTTelegramBot, tgtypes, tgsendertypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnStart: TButton;
    BtnStop: TButton;
    DTLongPolBot1: TDTLongPolBot;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure DTLongPolBot1ReceiveMessageUpdate(ASender: TObject; AMessage: TTelegramMessageObj);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  tgutils
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnStartClick(Sender: TObject);
begin                         
  BtnStart.Enabled:=False;
  DTLongPolBot1.StartReceiver;
  BtnStop.Enabled:=True;
end;

procedure TForm1.BtnStopClick(Sender: TObject);
begin
  BtnStop.Enabled:=False;
  DTLongPolBot1.StopReceiver;
  BtnStart.Enabled:=True;
end;

procedure TForm1.DTLongPolBot1ReceiveMessageUpdate(ASender: TObject; AMessage: TTelegramMessageObj);
var
  aReply: String;
  aReplyMarkup: TReplyMarkup;
begin
  aReplyMarkup:=nil;
  if DTLongPolBot1.ReplyMarkups.Count>0 then
    aReplyMarkup:=DTLongPolBot1.ReplyMarkups.ReplyMarkups[0].GetJSONReplyMarkup;
  try
    if AMessage.Text='/hi' then
      TgBotSendMessage(DTLongPolBot1.Token, TTelegramSender(ASender).CurrentChatId,
        Format('Hi, %s', [CaptionFromUser(AMessage.From)])+'!', aReply, pmDefault, False, aReplyMarkup)
    else
      TgBotSendMessage(DTLongPolBot1.Token, TTelegramSender(ASender).CurrentChatId,
        Format('You sent a command `%s`', [AMessage.Text]), aReply, pmMarkdown);
  finally
    aReplyMarkup.Free;
  end;
  Memo1.Lines.Add(aReply);
  TTelegramSender(ASender).UpdateProcessed:=True;
end;

end.

