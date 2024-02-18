module KingUnko =
    type Element =
        | Unko
        | Crown
        | Eye
        | Nose
        | Mouth
        | Space

        member self.ToEmoji() : string =
            match self with
            | Unko -> "💩"
            | Crown -> "👑"
            | Eye -> "👁"
            | Nose -> "👃"
            | Mouth -> "👄"
            | Space -> "　"

    type Pattern =
        {
            Element: Element
            Count: int
        }

        member self.Replicate() = List.replicate self.Count self.Element

    let private generate (height: int) (n: int) : Pattern list =
        let padding =
            {
                Element = Space
                Count = (height - (n - 1))
            }

        match n with
        | 1 -> [ padding; { Element = Crown; Count = 1 }; padding ]
        | 3 ->
            [
                padding
                { Element = Unko; Count = 1 }
                { Element = Eye; Count = 1 }
                { Element = Unko; Count = 1 }
                { Element = Eye; Count = 1 }
                { Element = Unko; Count = 1 }
                padding
            ]
        | 4 ->
            [
                padding
                { Element = Unko; Count = 3 }
                { Element = Nose; Count = 1 }
                { Element = Unko; Count = 3 }
                padding
            ]
        | 5 ->
            [
                padding
                { Element = Unko; Count = 4 }
                { Element = Mouth; Count = 1 }
                { Element = Unko; Count = 4 }
                padding
            ]
        | _ -> [ padding; { Element = Unko; Count = (2 * n - 1) }; padding ]

    let private generateStep (height: int) (n: int) =
        generate height n
        |> List.map (fun pattern -> pattern.Replicate())
        |> List.concat

    let generateAll (height: int) =
        for n in [ 1..height ] do
            generateStep height n
            |> List.map (fun step -> step.ToEmoji())
            |> String.concat ""
            |> printfn "%s"

type ExitStatus =
    | Ok
    | InvalidArgs of string

    member self.ToInt() : int =
        match self with
        | Ok -> 0
        | InvalidArgs(args) -> 
            eprintfn "invalid arguments: %A" args
            1

let run (args:string array) : ExitStatus =
    match args with
    | args when args.Length = 1 ->
        let tryParseInt (s: string) : int option =
            match System.Int32.TryParse(s) with
            | (true, n) -> Some n
            | _ -> None

        match tryParseInt args[0] with
        | Some(height) -> KingUnko.generateAll height
        | None -> eprintfn "invalid number: %s" args[0]

        ExitStatus.Ok
    | args ->
        ExitStatus.InvalidArgs <| String.concat " " args
    
[<EntryPoint>]
let main args =
    let exitStatus = run args
    exitStatus.ToInt()

