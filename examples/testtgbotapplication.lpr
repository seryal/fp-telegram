program testtgbotapplication;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, tg
  { you can add units after this };

type

  { TTestTGBotApplication }

  TTestTGBotApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTestTGBotApplication }

procedure TTestTGBotApplication.DoRun;
var
  ErrorMsg: String;
  lRequestsThread: TTGLongPollThread;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  lRequestsThread := TTGLongPollThread.Create('220259196:AAG40hUUEIZKUYXprHW-B81b15Gw2DHNV_Q');
  lRequestsThread.WaitFor;
  lRequestsThread.Free;
  //Sleep(4000);
  //lRequestsThread.Request;
  //Sleep(4000);
  //lRequestsThread.Request;
  //Sleep(4000);
  //lRequestsThread.Request;
  //lRequestsThread.Request;
  //lRequestsThread.Terminate;
  //lRequestsThread.WaitFor;
  //lRequestsThread.Free;

  { add your program here }


  // stop program loop
  Terminate;
end;

constructor TTestTGBotApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestTGBotApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TTestTGBotApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TTestTGBotApplication;
begin
  Application:=TTestTGBotApplication.Create(nil);
  Application.Title:='TG Bot Application';
  Application.Run;
  Application.Free;
end.

