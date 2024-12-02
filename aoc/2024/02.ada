with Ada.Containers.Vectors;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed, Ada.Strings.Maps;
with Ada.Text_IO;

procedure AOC02 is
  package Row is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Integer);
  package Grid is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Row.Vector, "=" => Row."=");

  function Read_Data(File_Name: String) return Grid.Vector is
    function Parse_Row(Input : String) return Row.Vector is
      Pos : Natural := 1;
      Output : Row.Vector := Row.Empty_Vector;
      Start : Positive := Input'First;
      Finish : Natural := 0;
    begin
      while Start <= Input'Last loop
        Ada.Strings.Fixed.Find_Token(
          Input,
          Ada.Strings.Maps.To_Set(' '),
          Start,
          Ada.Strings.Outside,
          Start,
          Finish
        );
        exit when Start > Finish;
        Output.Append(Integer'Value(Input(Start .. Finish)));
        Start := Finish + 1;
      end loop;
      return Output;
    end Parse_Row;
    Data : Grid.Vector;
    File : Ada.Text_IO.File_Type;
    Input : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Open(File => Input, Mode => Ada.Text_IO.In_File, Name => File_Name);
    loop
      Data.Append(Parse_Row(Ada.Text_IO.Get_Line(Input)));
    end loop;
  exception
    when Ada.IO_Exceptions.Name_Error =>
      Ada.Text_IO.Put_Line("File not found.");
      return Grid.Empty_Vector;
    when Ada.IO_Exceptions.End_Error =>
      if Ada.Text_IO.Is_Open(Input) then
        Ada.Text_IO.Close (Input);
      end if;
      return Data;
  end Read_Data;

  function Is_Safe(Report : Row.Vector) return Boolean is
    type Sign_Type is range -1 .. 1;
    function Sign(Value: Integer) return Sign_Type is
    begin
      if Value < 0 then
        return -1;
      elsif Value > 0 then
        return 1;
      else
        return 0;
      end if;
    end Sign;
    Report_Sign : Sign_Type := 0;
  begin
    for I in Report.First_Index .. Report.Last_Index - 1 loop
      declare
        Difference : Integer := Report(I + 1) - Report(I);
        Current_Sign : Sign_Type := Sign(Difference);
      begin
        if abs(Difference) < 1 or abs(Difference) > 3 then
          return False;
        else
          if Report_Sign = 0 then
            Report_Sign := Current_Sign;
          end if;

          if Current_Sign /= Report_Sign then
            return False;
          end if;
        end if;
      end;
    end loop;

    return True;
  end Is_Safe;

  function Is_Safe_Dampened(Report : Row.Vector) return Boolean is
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
  begin
    if Is_Safe(Report) then
      return True;
    else
      for I in Report.First_Index .. Report.Last_Index loop
        declare
          Dampened_Report : Row.Vector := Remove_Element_At(I, Report);
        begin
          if Is_Safe(Dampened_Report) then
            return True;
          end if;
        end;
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

    Ada.Text_IO.Put_Line("Safe rows" & Integer'Image(Safe_Rows));
    Ada.Text_IO.Put_Line("Safe rows dampened" & Integer'Image(Safe_Rows_Dampened));
  end Main;

begin
  Main;
end AOC02;
