namespace Melon

module Main =
    open AST

    [<EntryPointAttribute>]
    let main _ =
        printExpr
        |> interpret (Map.empty |> Map.add "print" (ENative cheatyPrint))
        |> ignore
        0
