{****************************************************************************
*                                                                           *
*                          free-lock queue                                  *
*                                                                           *
*                                                                           *
* Language:             FPC Pascal v2.2.0+ / Delphi 6+                      *
*                                                                           *
* Required switches:    none                                                *
*                                                                           *
* Author:               Dariusz Mazur                                       *
* Date:                 20.01.2008                                          *
* Version:              0.7                                                 *
* Licence:              MPL or GPL
*                                                                           *
*        Send bug reports and feedback to  darekm @@ emadar @@ com          *
*   You can always get the latest version/revision of this package from     *
*                                                                           *
*           http://www.emadar.com/fpc/lockfree.htm                          *
*                                                                           *
*                                                                           *
* Description:  Free-lock algotithm to handle queue FIFO                    *
*               Has two implementation queue based on curcular array        *
*               proposed by Dariusz Mazur                                   *
*               use only single CAS                                         *
*               tFlQueue: for queue of tObject (pointer)                    *
*               gFlQueue: generic queue of any record                       *
* caution : if You set too small size of array and store data excess size   *
*           of queue data will be lost                                      *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*                                                                           *
*****************************************************************************
*                      BEGIN LICENSE BLOCK                                  *

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: flqueue.pas, released 20.01.2008.
The Initial Developer of the Original Code is Dariusz Mazur


Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

*                     END LICENSE BLOCK                                     * }

{ changelog
v.0.06 27.01.2008 change implementation of circural array (bug find by Martin Friebe
v.0.07 08.12.2014 make it compilable with FPC 2.6.0 on x86_64
v.0.08 20.07.2016 more compilable on x86_64}



unit flqueue;

interface
uses
  {$IFNDEF FPC}
   windows,
  {$ELSE}
    {$IF ((FPC_VERSION = 2) AND (FPC_RELEASE >= 2)) OR (FPC_VERSION > 2)}
       {$DEFINE USEGENERIC}
    {$ENDIF}
  {$ENDIF}
  classes;



type
  TNodeQueue = TObject;

  TFLQueue = class
  private
      fSize : PtrUInt;
      fMask : PtrUInt;
      tab   : array of TNodeQueue;
      tail,
      head,
      temp  : NativeInt;
      procedure setobject(lp : NativeInt;const aObject : TNodeQueue);
      function getObject(lp : NativeInt) : TNodeQueue;
      function getLength : NativeInt;
  public
      constructor create(aPower : NativeInt =10);  {allocate tab with size equal 2^aPower, for 10 size is equal 1024}
      procedure push(const tm : TNodeQueue);
      function pop: TNodeQueue;
      property length : NativeInt read getLength;
  end;

{$IFDEF USEGENERIC}

  generic TGFLQueue<_R>=class
      tab   : array of _R;
      fSize : PtrUInt;
      fMask : PtrUInt;
      tail,
      head,
      temp  : NativeInt;
      procedure setobject(lp : NativeInt;const aObject : _R);
      function getObject(lp : NativeInt):  _R;
      function getLength : NativeInt;
  public
     constructor create(aPower : NativeInt);   {allocate tab with size equal 2^aPower}
     procedure push(const tm : _R);
     function pop(var tm : _R) : boolean;
     property length : NativeInt read getLength;
  end;

{$ENDIF}

implementation

constructor TFLQueue.create(aPower : NativeInt);
begin
  fMask:=not(high(fMask) shl aPower);
  fSize:=1 shl aPower;
  setLength(tab,fSize);
  temp:=0;
  tail:=0;
  head:=0;
end;

procedure TFLQueue.setobject(lp : NativeInt; const aObject : TNodeQueue);
begin
  tab[lp and fMask]:=aObject;
end;

function TFLQueue.getObject(lp : NativeInt) : TNodeQueue;
begin
  result:=tab[lp and fMask];
end;

function TFLQueue.getLength : NativeInt;
begin
  result:=tail-head;
end;

procedure TFLQueue.push(const tm : TNodeQueue);
var
  newTemp,
  lastTail,
  newTail : NativeInt;
begin
  {$IFDEF CPU64}
  newTemp:=interlockedIncrement64(temp);
  {$ELSE}
  newTemp:=interlockedIncrement(temp);
  {$ENDIF CPU64}
  lastTail:=newTemp-1;
  setObject(lastTail,tm);
  repeat
    {$IFDEF CPU64}
    newTail:=interlockedCompareExchange64(tail,newTemp,lastTail);
    {$ELSE}
    newTail:=interlockedCompareExchange(tail,newTemp,lastTail);
    {$ENDIF CPU64}
  until (newTail=lastTail);

end;

function TFLQueue.pop : TNodeQueue;
var
  newhead,
  lastHead : NativeInt;
begin
  repeat
    lastHead:=head;
    if tail<>head then begin
      {$IFDEF CPU64}
      newHead:=interlockedCompareExchange64(head,lastHead+1,lasthead);
      {$ELSE}
      newHead:=interlockedCompareExchange(head,lastHead+1,lasthead);
      {$ENDIF CPU64}
      if newHead=lastHead then begin
         result:=getObject(lastHead);
         exit;
      end;
    end else begin
       result:=nil;
       exit;
    end;
  until false;
end;

{$IFDEF USEGENERIC}

constructor TGFLQueue.create(aPower : NativeInt);
begin
  fMask:=not(high(fMask) shl aPower);
  fSize:=1 shl aPower;
  setLength(tab,fSize);
  tail:=0;
  head:=0;
  temp:=0;
end;

procedure TGFLQueue.setobject(lp : NativeInt; const aObject : _R);
begin
  tab[lp and fMask]:=aObject;
end;

function TGFLQueue.getObject(lp : NativeInt) : _R;
begin
  result:=tab[lp and fMask];
end;

function TGFLQueue.getLength : NativeInt;
begin
  result:=tail-head;
end;

procedure TGFLQueue.push(const tm : _R);
var
  newTemp,
  lastTail,
  newTail : NativeInt;
begin
  {$IFDEF CPU64}
  newTemp:=interlockedIncrement64(temp);
  {$ELSE}
  newTemp:=interlockedIncrement(temp);
  {$ENDIF CPU64}
  lastTail:=newTemp-1;
  setObject(lastTail,tm);
  repeat
    {$IFDEF CPU64}
    newTail:=interlockedCompareExchange64(tail,newTemp,lastTail);
    {$ELSE}
    newTail:=interlockedCompareExchange(tail,newTemp,lastTail);
    {$ENDIF CPU64}
  until (newTail=lastTail);

end;

function TGFLQueue.pop(var tm : _R) : boolean;
var
  newhead,
  lastHead : NativeInt;
begin
  repeat
    lastHead:=head;
    if tail<>head then begin
      {$IFDEF CPU64}
      newHead:=interlockedCompareExchange64(head,lastHead+1,lasthead);
      {$ELSE}
      newHead:=interlockedCompareExchange(head,lastHead+1,lasthead);
      {$ENDIF CPU64}
      if newHead=lastHead then begin
         tm:=getObject(lastHead);
         result:=true;
         exit;
      end;
    end else begin
       result:=false;
       exit;
    end;
  until false;
end;

{$ENDIF}

end.
