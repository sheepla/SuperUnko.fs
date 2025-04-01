namespace SuperUnko

module FizzBuzz =

    type Pattern =
        | Fizz
        | Buzz
        | FizzBuzz
        | Number of int

        member self.ToEmoji() : string =
            match self with
            | Fizz -> "💩 Fizz"
            | Buzz -> "🍺 Buzz"
            | FizzBuzz -> "💩🍺 FizzBuzz"
            | Number n -> n.ToString()

    let generate (count: int) : Pattern seq =
        seq {
            for n in 1..count do
                match n % 3, n % 5 with
                | 0, 0 -> yield FizzBuzz
                | 0, _ -> yield Fizz
                | _, 0 -> yield Buzz
                | _ -> yield Number n
        }
