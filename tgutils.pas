unit tgutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

const
  mdCode='`';

function MarkdownEscape(const S: String): String;

implementation

const
  MarkdownSpChars: array[0..3] of AnsiChar = ('\', '_', '*', '`');

function MarkdownEscape(const S: String): String;
var
  a: AnsiChar;
begin
  Result:=S;
  for a in MarkdownSpChars do
    Result:=StringReplace(Result, a, '\'+a, [rfReplaceAll]);
end;

end.

