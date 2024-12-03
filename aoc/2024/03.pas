program Aoc3;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Classes,
  RegExpr,
  SysUtils;

var
  EnvVar: string;
  FileName: string;

function CalculateSumOfProducts(InputString: string): integer;
var
  MulExpression: TRegExpr;
  SumOfProducts: integer;
begin
  SumOfProducts := 0;
  MulExpression := TRegExpr.Create('mul\((\d{1,3}),(\d{1,3})\)');
  try
    if MulExpression.Exec(InputString) then
    begin
      SumOfProducts := SumOfProducts + StrToInt(MulExpression.Match[1]) * StrToInt(MulExpression.Match[2]);
      while MulExpression.ExecNext do
      begin
        SumOfProducts := SumOfProducts + StrToInt(MulExpression.Match[1]) * StrToInt(MulExpression.Match[2]);
      end;
    end;
  finally
    MulExpression.Free;
  end;

  Result := SumOfProducts;
end;


var
  Lines: TStringList;
  InputString: string;
  SumOfProducts: integer;
begin
  EnvVar := GetEnvironmentVariable('AOC_TEST');
  if EnvVar <> '' then
    FileName := '03.txt.test'
  else
    FileName := '03.txt';

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
  except
    on E: EInOutError do
      WriteLn('File handling error occurred. Reason: ', E.Message)
  end;

  InputString := Lines.Text;

  SumOfProducts := CalculateSumOfProducts(InputString);
  WriteLn(IntToStr(SumOfProducts));

  InputString := ReplaceRegExpr('don''t\(\)(.*?)do\(\)', InputString, '', TRUE);
  InputString := ReplaceRegExpr('don''t\(\)(.*?)$', InputString, '', TRUE);
  SumOfProducts := CalculateSumOfProducts(InputString);
  WriteLn(IntToStr(SumOfProducts));

  Lines.Free;
end.
