open System
open System.IO

type CPUState = {
    A: uint64
    B: uint64
    C: uint64
}

let parseCpu (lines: string[]) : CPUState =
    let parseLine (line: string) =
        let parts = line.Split(':')
        let registerName = parts.[0].Trim()
        let registerValue = uint64 (parts.[1].Trim())
        registerName, registerValue

    let registerMap =
        lines
        |> Array.takeWhile (fun line -> line.StartsWith("Register"))
        |> Array.map parseLine
        |> Map.ofArray

    {
        A = Map.find "Register A" registerMap
        B = Map.find "Register B" registerMap
        C = Map.find "Register C" registerMap
    }

let parseProgram (lines: string[]) : int[] =
    let programLine = lines |> Array.tryFind (fun line -> line.StartsWith("Program:"))
    match programLine with
    | Some line ->
        line.Substring("Program:".Length).Split(',')
        |> Array.map (fun s -> int (s.Trim()))
    | None -> [||] // Return an empty array if no program line is found

let getComboOperand (operand : int) (cpu : CPUState) : uint64 =
  match operand with
  | 4 -> cpu.A
  | 5 -> cpu.B
  | 6 -> cpu.C
  | n when n >= 0 && n <= 3 -> uint64 n

[<TailCall>]
let rec next (ip : int) (output : int list) (program : int[]) (cpu : CPUState) : string =
  if ip >= program.Length then
    output
      |> List.rev
      |> List.map string
      |> String.concat ","
  else
    let
      ip_ = ip + 2
    in
    match (program[ip], program[ip + 1]) with
    | (0, operand) -> next ip_ output program {cpu with A = cpu.A >>> int32 (getComboOperand operand cpu)}
    | (1, operand) -> next ip_ output program {cpu with B = cpu.B ^^^ uint64 operand}
    | (2, operand) -> next ip_ output program {cpu with B = getComboOperand operand cpu % 8UL}
    | (3, operand) ->
      if cpu.A = 0UL then
        next ip_ output program cpu
      else
        next operand output program cpu
    | (4, _) -> next ip_ output program {cpu with B = cpu.B ^^^ cpu.C}
    | (5, operand) ->
      next ip_ (int32 (getComboOperand operand cpu % 8UL) :: output) program cpu
    | (6, operand) -> next ip_ output program {cpu with B = cpu.A >>> int32 (getComboOperand operand cpu)}
    | (7, operand) -> next ip_ output program {cpu with C = cpu.A >>> int32 (getComboOperand operand cpu)}

let printCpu (cpu : CPUState) =
    printfn "Registers: A=%d, B=%d, C=%d" cpu.A cpu.B cpu.C

[<TailCall>]
let rec findQuine (program : int[]) (cpu : CPUState) : uint64 =
  let programString = program |> Array.map string |> Array.toList |> String.concat "," in
  let mutable aValue = 1UL
  let mutable output = next 0 [] program {cpu with A = aValue} in
  while aValue >= 0UL && output <> programString do
    printfn "%d: %s" aValue output
    if String.length output = String.length programString then
      aValue <- aValue + 1UL
    else
      aValue <- aValue <<< 1
    try
      output <- next 0 [] program {cpu with A = aValue}
    with
      | :? System.DivideByZeroException -> ()
  aValue

let main () =
    let filePath =
        if Environment.GetEnvironmentVariable("AOC_TEST") <> null then
            "17.txt.test"
        else
            "17.txt"

    let lines = File.ReadAllLines(filePath)

    let cpuState = parseCpu lines
    let program = parseProgram lines

    printfn "%s" <| next 0 [] program cpuState

    printfn "%d" <| findQuine program cpuState

main ()
