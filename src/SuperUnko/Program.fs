namespace SuperUnko

module Program =

    open System

    [<Literal>]
    let Usage =
        @"Usage: 
    SuperUnko [-h|--help]
    SuperUnko king <HEIGHT>
    SuperUnko fizzbuzz <COUNT>
    SuperUnko bigunko
"

    type ExitStatus =
        | Ok
        | InvalidArgs of string array * string

        member self.ToInt() : int =
            match self with
            | Ok -> 0
            | InvalidArgs(args, message) ->
                eprintfn "Invalid arguments: %A\n%s" args message
                1

    let tryParseInt (s: string) : int option =
        match System.Int32.TryParse(s) with
        | (true, n) -> Some n
        | _ -> None

    let run (args: string array) : ExitStatus =
        match Array.tryItem 0 args with
        | None ->
            Console.WriteLine(Usage)
            ExitStatus.InvalidArgs(args, "No command specified")
        | Some command ->
            match command with
            | "king" ->
                match Array.tryItem 1 args with
                | None ->
                    Console.WriteLine(Usage)
                    ExitStatus.InvalidArgs(args, "Requires height")
                | Some heightString ->
                    match tryParseInt heightString with
                    | None ->
                        Console.WriteLine(Usage)
                        ExitStatus.InvalidArgs(args, "Invalid number")
                    | Some height ->
                        KingUnko.generate height |> Seq.iter (fun step -> Console.WriteLine(step))
                        ExitStatus.Ok
            | "bigunko" ->
                SuperUnko.BigUnko.printBigUnko ()
                ExitStatus.Ok
            | "fizzbuzz" ->
                match Array.tryItem 1 args with
                | None ->
                    Console.WriteLine(Usage)
                    ExitStatus.InvalidArgs(args, "Requires count")

                | Some countString ->
                    match tryParseInt countString with
                    | None ->
                        Console.WriteLine(Usage)
                        ExitStatus.InvalidArgs(args, "Invalid number")
                    | Some count ->
                        FizzBuzz.generate count
                        |> Seq.iter (fun pattern -> Console.WriteLine(pattern.ToEmoji()))

                        ExitStatus.Ok
            | _ ->
                Console.WriteLine(Usage)
                ExitStatus.InvalidArgs(args, "Unknown command")

    //match args[0] with
    //| command when command = "king" ->
    //    match tryParseInt args[1] with
    //    | Some height ->
    //        KingUnko.generate height |> Seq.iter (fun step -> Console.WriteLine(step))
    //        ExitStatus.Ok
    //    | None ->
    //        Console.WriteLine(Usage)
    //        ExitStatus.InvalidArgs <| sprintf "invalid number: %s" args[0]
    //| command when command = "bigunko" ->
    //    SuperUnko.BigUnko.printBigUnko ()
    //    ExitStatus.Ok
    //| command when command = "fizzbuzz" ->
    //    match tryParseInt args[1] with
    //    | Some count ->
    //        FizzBuzz.generate count
    //        |> Seq.iter (fun pattern -> Console.WriteLine(pattern.ToEmoji()))

    //        ExitStatus.Ok
    //    | None ->
    //        Console.WriteLine(Usage)
    //        ExitStatus.InvalidArgs <| sprintf "invalid number: %s" args[0]
    //| command when (Array.contains command [| "-h"; "--help" |]) ->
    //    Console.WriteLine(Usage)
    //    ExitStatus.Ok
    //| command ->
    //    Console.WriteLine(Usage)
    //    ExitStatus.InvalidArgs command

    [<EntryPoint>]
    let main (argv: string array) : int =
        let exitStatus = run argv
        exitStatus.ToInt()
