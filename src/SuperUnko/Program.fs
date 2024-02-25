namespace SuperUnko

module Program =

    type ExitStatus =
        | Ok
        | InvalidArgs of string

        member self.ToInt() : int =
            match self with
            | Ok -> 0
            | InvalidArgs(args) ->
                eprintfn "invalid arguments: %A" args
                1

    let run (args: string array) : ExitStatus =
        match args with
        | args when args.Length = 1 ->
            let tryParseInt (s: string) : int option =
                match System.Int32.TryParse(s) with
                | (true, n) -> Some n
                | _ -> None
            match tryParseInt args[0] with
            | Some(height) ->

                KingUnko.generate height
                ExitStatus.Ok
            | None -> ExitStatus.InvalidArgs <| sprintf "invalid number: %s" args[0]

        | args -> ExitStatus.InvalidArgs <| String.concat " " args


    [<EntryPoint>]
    let main (argv: string array) : int =
        let exitStatus = run argv
        exitStatus.ToInt()