namespace Melon

module AST =
    (*
    expression :=
        literal := integer
        variable := label
        abstraction := λvariable.expression
        application := expression expression
    *)
    type Expression =
        | ELiteral of Value: int
        | EVariable of Label: string
        | EAbstraction of Variable: string * Body: Expression
        | EApplication of Abstraction: Expression * Argument: Expression
        | ENative of Func: (Expression -> Expression)

    let cheatyPrint (elem: Expression): Expression = 
        printfn "%A" elem
        elem

    let expr =
        EApplication 
            (EAbstraction ("X", EVariable "X"), ELiteral 2)

    let badExpr =
        EApplication 
            (ELiteral 10, ELiteral 2)

    let printExpr =
        EApplication
            (EVariable "print", ELiteral 123)
    
    //val interpret : Map<string, Expression> -> Expression -> Expression
    let rec interpret context = function
        | ELiteral v -> ELiteral v
        | EVariable label -> 
            Map.tryFind label context
            |> function
                Some value -> value
              | None -> failwith $"variable not found in context: {label}"
        | EAbstraction (var, body) as abs -> 
            //Closure
            abs
        | ENative _ as native ->
            native
        | EApplication (abstraction, argument) -> 
            let evaluatedAbs = interpret context abstraction
            match evaluatedAbs with
            | EAbstraction (var, body) ->
                let evaluatedArg = interpret context argument
                let updatedContext = Map.add var evaluatedArg context
                interpret updatedContext body
            | ENative func ->
                let evaluatedArg = interpret context argument
                func evaluatedArg
            | _ -> failwith "Could not apply argument to something that is not a function."

    let x (y : #System.Collections.Generic.IEnumerable<int>) = 
        0

    let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) =
        value1 + value2
    