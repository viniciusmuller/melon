namespace Melon

module AST =
    type BinaryOperator = 
        | Add
        | Sub
        | Mult
        | Div
        | And
        | Or

    type UnaryOperator = 
        | Not
        | Plus
        | Minus

    type Literal = 
        | Int of int
        | Str of string
        | Bool of bool

    (*
    expression :=
        // Custom
        literal := integer
        
        // Core LC
        variable := label
        abstraction := λvariable.expression
        application := expression expression
    *)
    type Expression =
        | ELiteral of Value: Literal
        | EBinaryExpression of BinaryOperator * LHS: Expression * RHS: Expression
        | EUnaryExpression of UnaryOperator * Expression
        | EVariable of Label: string
        | EAbstraction of Variable: string * Body: Expression
        | EApplication of Abstraction: Expression * Argument: Expression
        | ENative of Func: (Expression -> Expression)

    let asInt expr = 
        match expr with 
            | ELiteral (Int v) -> v
            | _ -> failwithf "expecting integer: got %A" expr

    let asBool expr = 
        match expr with 
            | ELiteral (Bool b) -> b
            | _ -> failwithf "expecting boolean: got %A" expr

    let cheatyPrint (elem: Expression): Expression = 
        printfn "%A" elem
        elem

    let logicExpression =
        EApplication
            (EAbstraction  (
                "X",
                EBinaryExpression (
                    Or,
                    (EVariable "X"),
                    (EUnaryExpression (Not, (EVariable "X")))
                )),
            ELiteral (Bool false))

    let arithmeticExpression =
        EApplication
            (EAbstraction  (
                "X",
                EBinaryExpression (
                    Add, 
                    (EVariable "X"),
                    (ELiteral (Int 1))
                )),
            ELiteral (Int 10))

    let expr =
        EApplication 
            (EAbstraction ("X", EVariable "X"), ELiteral (Int 2))

    let badExpr =
        EApplication 
            (ELiteral (Int 10), ELiteral (Int 2))

    let printExpr =
        EApplication
            (EVariable "print", ELiteral (Int 123))
    
    // val eval : Map<string, Expression> -> Expression -> Expression
    // - Our goal with this is to reduce it the max that we can, until we get an abstraction on the
    // left hand side and another expression on the right hand side. If we recurse it all over and 
    // we don't have an abstraction on the left side, it means that there's a syntax error (such as "x y")
    // - We use a map to take care of variable bounding, and it also works recursively without clashing names 
    // due to the immutability of the language, we have different versions of the context map scattered across the call stack.
    // - When we find the reference to a value, we try to find it on the map, if we find nothing it means that it is not bound.
    // - If we find it, we are effectively substituing variables with expressions, the core of lambda calculus.
    // - In order to bind variables, when we find an abstraction (function) we evaluate its body and then add it to the context,
    // using the variable name of the application as the key. This works since the arity of functions in LC can only be 1.

    // # Step by step evaluation - Id function with literal
    // 
    // 1. EApplication (EAbstraction ("X", EVariable "X"), ELiteral 2)
    // 2. EAbstraction ("X", EVariable "X")
    // 3. ELiteral 2
    // 4. EVariable "X"
    // 5. result: ELiteral 2
    // 
    // Notice that when we find an application, we recurse on the abstraction in order to get it, so that we can continue evaluating it.
    // Then, we see that the abstraction takes a parameter called X and its block is just a reference to the variable X.
    // Then we evaluate the right hand side, which simplifies to a literal and *bind it* to the variable X in our context.
    // Now we evaluate the body of the function with our updated context, which in turn refers to the variable X, which is now in the context, and just fetch it from there.
    let rec eval context = function
        | ELiteral v -> ELiteral v
        | EVariable label ->
            Map.tryFind label context
            |> function
                Some value -> value
              | None -> failwith $"variable not found in context: {label}"
        | EAbstraction (var, body) as abs -> 
            //Closure
            abs
        | EApplication (abstraction, argument) -> 
            let evaluatedAbs = eval context abstraction

            match evaluatedAbs with
            | EAbstraction (var, body) ->
                let evaluatedArg = eval context argument
                let updatedContext = Map.add var evaluatedArg context
                eval updatedContext body
            | ENative func ->
                let evaluatedArg = eval context argument
                func evaluatedArg
            | _ -> failwith "Could not apply argument to something that is not a function."
        | ENative _ as native ->
            native
        | EBinaryExpression (operator, lhs, rhs) ->
            let lhs = eval context lhs
            let rhs = eval context rhs

            match operator with 
            | Sub -> 
                ELiteral (Int ((asInt lhs) - (asInt rhs)))
            | Add -> 
                ELiteral (Int ((asInt lhs) + (asInt rhs)))
            | Mult -> 
                ELiteral (Int ((asInt lhs) * (asInt rhs)))
            | Div ->
                ELiteral (Int ((asInt lhs) / (asInt rhs)))
            | And ->
                ELiteral (Bool ((asBool lhs) && (asBool rhs)))
            | Or ->
                ELiteral (Bool ((asBool lhs) || (asBool rhs)))
        | EUnaryExpression (operator, expr) ->
            let expr = eval context expr

            match operator with 
            | Not ->
                ELiteral (Bool (not (asBool expr)))
            | Plus ->
                ELiteral (Int (+ asInt expr))
            | Minus ->
                ELiteral (Int (- asInt expr))