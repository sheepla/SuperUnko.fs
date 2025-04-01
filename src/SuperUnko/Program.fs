namespace SuperUnko

module Program =

    open System

    [<Literal>]
    let Usage =
        @"Usage: 
    SuperUnko [-h|--help]
    SuperUnko king <HEIGHT>
"

    type ExitStatus =
        | Ok
        | InvalidArgs of string

        member self.ToInt() : int =
            match self with
            | Ok -> 0
            | InvalidArgs(args) ->
                eprintfn "invalid arguments: %A" args
                1

    let tryParseInt (s: string) : int option =
        match System.Int32.TryParse(s) with
        | (true, n) -> Some n
        | _ -> None

    let run (args: string array) : ExitStatus =
        match args with
        | args when args.Length = 2 ->
            match args[0] with
            | command when command = "king" ->
                match tryParseInt args[1] with
                | Some(height) ->
                    KingUnko.generate height
                    ExitStatus.Ok
                | None ->
                    Console.WriteLine(Usage)
                    ExitStatus.InvalidArgs <| sprintf "invalid number: %s" args[0]
            | command when (Array.contains command [| "-h"; "--help" |]) ->
                Console.WriteLine(Usage)
                ExitStatus.Ok
            | command ->
                Console.WriteLine(Usage)
                ExitStatus.InvalidArgs command
        | args ->
            Console.WriteLine(Usage)
            ExitStatus.InvalidArgs <| String.concat " " args

    [<EntryPoint>]
    let main (argv: string array) : int =
        let exitStatus = run argv
        exitStatus.ToInt()
