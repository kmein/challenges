with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.IO_Exceptions;
with Ada.Containers.Indefinite_Vectors, Ada.Strings.Fixed, Ada.Strings.Maps;
use Ada.Containers, Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Maps;


procedure aoc02 is
  package Row is new Ada.Containers.Vectors (
    Index_Type   => Positive,
    Element_Type => Integer
  );

  package Grid is new Ada.Containers.Vectors (
    Index_Type   => Positive,
    Element_Type => Row.Vector,
    "="          => Row."="
  );

  function Parse_Row(Input : String) return Row.Vector is
    Pos : Natural := 1;
    Output : Row.Vector := Row.Empty_Vector;
    Start : Positive := Input'First;
    Finish : Natural := 0;
  begin
    while Start <= Input'Last loop
      Find_Token(Input, To_Set (' '), Start, Outside, Start, Finish);
      exit when Start > Finish;
      Output.Append(Integer'Value(Input(Start .. Finish)));
      Start := Finish + 1;
    end loop;
    return Output;
  end Parse_Row;

  function Read_Data(File_Name: String) return Grid.Vector is
    Data : Grid.Vector;
    File : File_Type;
    Input : File_Type;
  begin
    Open(File => Input, Mode => In_File, Name => File_Name);
    loop
      declare
        Line : String := Get_Line(Input);
        Current_Row : Row.Vector;
      begin
        Current_Row := Parse_Row(Line);
        Data.Append(Current_Row);
      end;
    end loop;
  exception
    when Ada.IO_Exceptions.Name_Error =>
      Put_Line("File not found.");
      return Grid.Empty_Vector;
    when Ada.IO_Exceptions.End_Error =>
      if Is_Open(Input) then
        Close (Input);
      end if;
      return Data;
  end Read_Data;

  function Sign(Value: Integer) return Integer is
  begin
    if Value < 0 then
      return -1;
    elsif Value > 0 then
      return 1;
    else
      return 0;
    end if;
  end Sign;

  function Is_Safe(Report : Row.Vector) return Boolean is
    Report_Sign : Integer := 0;
    Current_Sign : Integer := 0;
    Result_A : Integer;
    Result_B : Integer;
    Difference : Integer;
  begin
    for I in Report.First_Index .. Report.Last_Index - 1 loop
      Result_A := Report(I);
      Result_B := Report(I + 1);
      Difference := Result_B - Result_A;

      if abs(Difference) < 1 or abs(Difference) > 3 then
        Put_Line("Report " & Integer'Image(I) & " unsafe because of large difference: " & Integer'Image(Result_A) & Integer'Image(Result_B));
        return False;
      else
        Current_Sign := Sign(Difference);

        if Report_Sign = 0 then
          Report_Sign := Current_Sign;
        end if;

        if Current_Sign /= Report_Sign then
          return False;
        end if;
      end if;
    end loop;

    return True;
  end Is_Safe;

  function Remove_Element_At(Index : Positive; Report : Row.Vector) return Row.Vector is
    Result : Row.Vector := Row.Empty_Vector;
  begin
    for I in Report.First_Index .. Report.Last_Index loop
      if I /= Index then
        Result.Append(Report(I));
      end if;
    end loop;
    return Result;
  end Remove_Element_At;

  function Is_Safe_Dampened(Report : Row.Vector) return Boolean is
    Dampened_Report : Row.Vector;
  begin
    if Is_Safe(Report) then
      return True;
    else
      for I in Report.First_Index .. Report.Last_Index loop
        Dampened_Report := Remove_Element_At(I, Report);
        if Is_Safe(Dampened_Report) then
          Put_Line("Unsafe report could be saved by removing element" & Integer'Image(I));
          return True;
        end if;
      end loop;

      return False;
    end if;
  end Is_Safe_Dampened;

  procedure Main is
    Data : Grid.Vector;
    Safe_Rows : Natural := 0;
    Safe_Rows_Dampened : Natural := 0;
  begin
    if Ada.Environment_Variables.Exists(Name => "AOC_TEST") then
      Data := Read_Data("02.txt.test");
    else
      Data := Read_Data("02.txt");
    end if;

    for Report of Data loop
      if Is_Safe(Report) then
        Safe_Rows := Safe_Rows + 1;
      end if;
      if Is_Safe_Dampened(Report) then
        Safe_Rows_Dampened := Safe_Rows_Dampened + 1;
      end if;
    end loop;

    Put_Line("Safe rows" & Integer'Image(Safe_Rows));
    Put_Line("Safe rows dampened" & Integer'Image(Safe_Rows_Dampened));
  end Main;

begin
  Main;
end aoc02;
