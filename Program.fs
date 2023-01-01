namespace Melon

module Main =
    open AST

    [<EntryPointAttribute>]
    let main _ =
        logicExpression
        |> eval (Map.empty |> Map.add "print" (ENative cheatyPrint))
        |> printfn "result: %A"
        0
