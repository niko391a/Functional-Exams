module ReExam2023

    open System

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module ReExam2023 = 
 *)

(* 1: Arithmetic *)
    
    type arith =
    | Num of int
    | Add of arith * arith
    
    let p1 = Num 42
    let p2 = Add(Num 5, Num 3)
    let p3 = Add(Add(Num 5, Num 3), Add(Num 7, Num (-9)))
    
(* Question 1.1: Evaluation *)
    let rec eval (a : arith) : int =
        match a with
        | Num x -> x
        | Add (a, b) -> eval a + eval b
    
(* Question 1.2: Negation and subtraction *)
    let rec negate (a : arith) : arith =
        match a with
        | Num x -> (Num -x)
        | Add(a, b) -> Add(negate a, negate b)
        
    let subtract (a : arith) (b : arith) : arith = Add(a, negate b)

(* Question 1.3: Multiplication *)
        
    let rec multiply (a : arith) (b : arith) : arith =
        match a, b with
        | Num x, Num y -> x * y |> Num
        | Num x, Add(a, b) -> Add(multiply (Num x) a, multiply (Num x) b)
        | Add(a, b), c -> Add(multiply a c, multiply b c)
    
(* Question 1.4: Exponents *)

    let pow (a : arith) (b : arith) =
        let rec powHelper (a : arith) (b : arith) (acc : arith) : arith = 
            match eval b with
            | 1 -> acc 
            | _ ->
                let b' = subtract b (Num 1)
                powHelper a b' (multiply acc a)
        powHelper a b a
        
(* Question 1.5: Iteration *)

    let rec iterate (f : 'a -> 'a) (acc : 'a) (a : arith) : 'a =
        match eval a with
        | 0 -> acc
        | _ ->
            let newAcc = f acc
            let a' = subtract a (Num 1)
            iterate f newAcc a'

    let pow2 (a : arith) (b : arith) =
        let powerFunc acc = multiply acc a
        iterate powerFunc (Num 1) b 
    
(* 2: Code Comprehension *)
 
    let rec foo =
        function
        | 0            -> true
        | x when x > 0 -> bar (x - 1)
        | x            -> bar (x + 1)
        
    and bar =
        function
        | 0            -> false
        | x when x > 0 -> foo (x - 1)
        | x            -> foo (x + 1)
        
    let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)
        

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
    foo has type: int -> bool
    bar has type: int -> bool
    baz has type: int list -> int list * int list


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
    foo returns true if the input is even
    bar returns true if the input is uneven
    baz groups even and uneven integers together, with duplicates, such that fst is even and snd is uneven
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:
    foo would be isIntEven()
    bar would be isIntUneven()
    baz would be sortEvenAndUneven()
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `baz xs`
    * B: `bar x`
    * C: `(ys, x::zs)`

    Q: In the context of the baz function, i.e. assuming that `x`, `xs`, `ys`, and `zs` all have the correct types,
       what are the types of snippets A, B, and C, expressed using the F# syntax for types, and what are they -- 
       focus on what they do rather than how they do it.
    
    A: 
    baz xs has the type: int list * int list
    bar x has the type: bool
    (ys, x::zs) has the type: int list * int list
    
    baz xs is the partition of the even and odd elements where the
    even elements are the first element of the tuple, and the odd elements
    is the second.

    bar x is true if x is odd and false otherwise.

    (ys, x :: zs) is the tuple containing the lists ys as its first element
    and the number x concatinated with zs as its second element.
    
    Q: * Explain the use of the `and`-operator that connect the `foo` and the `bar` functions.
       * Argue if the program would work if you replaced `and` with `let rec`.

    A: 
      The and operator allows for mutual recursion, i.e. functions that call each other.
      It would therefore not work with let rec as the code would no longer compile as foo would not be able to call a function
      before it has been initiated/declared yet.
    *)

(* Question 2.3: No recursion *) 
    let foo2 (input : int) : bool =
        if input % 2 = 0 then
            true
        else
            false
    let bar2 (input : int) : bool =
        if input % 2 = 1 then
            true
        else
            false

(* Question 2.4: Tail Recursion *)

    (*

    Q: The function `baz` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar-functions. You are allowed to evaluate 
       those function immediately.
       
    let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)
            
    A: <Your answer goes here>
    
    *)
(* Question 2.5: Continuations *)

    let bazTail (lst : int list) : int list * int list =
        let rec bazTailHelper (lst : int list) cont : int list * int list =
            match lst with
            | [] -> cont ([], [])
            | x::ys when foo x ->bazTailHelper ys (fun (even, uneven) -> cont(x::even, uneven))
            | x::ys -> bazTailHelper ys (fun (even, uneven) -> cont(even, x::uneven))
        bazTailHelper lst id

(* 3: Balanced brackets *)

      
    let explode (str : string) = [for c in str -> c]
    let implode (lst : char list) = lst |> List.toArray |> System.String
    
(* Question 3.1: Balanced brackets *)
    
    let balanced (str : string) : bool =
        let pairMapping = Map [ (')', '('); ('}', '{'); (']', '[') ]
        let rec recBalanceHelper (list : char list) (stack : char list) : bool =
            match list with
            | ch::lstTail when ch = '(' || ch = '{' || ch ='[' ->
                recBalanceHelper lstTail (ch :: stack)
            | ch::lstTail when ch = ')' || ch = ']' || ch = '}' ->
                match stack with
                | [] -> false
                | stackHead::stackTail ->
                    if stackHead = pairMapping[ch] then
                        recBalanceHelper lstTail stackTail
                    else false
            | [] -> if stack.IsEmpty then true
                    else false
                    
        recBalanceHelper (explode str) List.Empty
        
(* Question 3.2: Arbitrary delimiters *)
    
    let balanced2 (m: Map<char, char>) (str: string) : bool =
    // Helper function to explode string to char list
        let explode (s: string) = [for c in s -> c]
        
        // Define a recursive helper function to process the input
        let rec helperBalance2 (stack: char list) (chars: char list) : bool =
            match chars with
            | [] -> 
                // String is balanced if we've processed all characters and stack is empty
                stack = []
            | c :: rest ->
                // Case 1: Try to use c as an opener
                let openCharResult = 
                    // If c is an opener, add it to the stack and continue
                    if Map.containsKey c m then
                        helperBalance2 (c :: stack) rest
                    else false
                
                // Case 2: Try to use c as a closer
                let closeCharResult =
                    match stack with
                    | [] -> false  // Can't close if stack is empty
                    | top :: stackRest ->
                        // If top of stack maps to c, remove top from stack and continue
                        if Map.tryFind top m = Some c then
                            helperBalance2 stackRest rest
                        else false
                
                // Return true if either approach succeeds
                openCharResult || closeCharResult
               
        helperBalance2 [] (explode str)
    
(* Question 3.3: Matching brackets and palindromes *)    
    
    let balanced3 (str : string) = balanced2 (Map [ ('(', ')'); ('{', '}'); ('[', ']') ]) str
    
    let symmetric (s : string) : bool =
        // Solution without balanced2
        // let splitAndFiltered = explode s |> List.filter System.Char.IsLetter |> List.map System.Char.ToLower
        // let v1, v2 = List.splitAt (splitAndFiltered.Length/2) splitAndFiltered
        // let reV2 = List.rev v2
        //        
        // let rec symmetricHelper (input : char list * char list) : bool = 
        //     // split into two lists and continuously check each other
        //     match input with
        //     | [], [] -> true
        //     | x::xs, y::ys ->
        //         if x = y then
        //             symmetricHelper (xs, ys)
        //         else false
        // symmetricHelper (v1, reV2)
        // With balanced2
        
        let splitAndFiltered = explode s |> List.filter System.Char.IsLetter |> List.map System.Char.ToLower |> implode
        let map = ['a'..'z'] |> List.map (fun x -> (x, x)) |> Map.ofList
        balanced2 map splitAndFiltered

(* Question 3.4: Parsing balanced brackets *)    
               
    open JParsec.TextParser
        
    let ParseBalanced, bref = createParserForwardedToRef<unit>()
    
    let parseBalancedAux : Parser<unit> = many (pchar '(' <|> pchar ')' <|> pchar '[' <|> pchar ']' <|> pchar '{' <|> pchar '}')
                                          |>> (fun chars -> if balanced (implode chars) then () else failwith "Unbalanced string" )
                                               
    // uncomment after you have done parseBalancedAUX
    
    let parseBalanced = parseBalancedAux .>> pstring "**END**"
    do bref := parseBalancedAux
            
(* Question 3.5: Parallel counting *)

    let countBalanced (lst : string list) (x : int) : int =
        // Calculate chunk size such that we get x chunks (allowing last one to be smaller
        let chunkAmount = (lst.Length + x - 1) / x
        let chunks = List.chunkBySize chunkAmount lst
        
        // Define an async computation that checks a chunk of strings for balance
        let processChunk (chunk: string list) : Async<int> =
            async {
                return chunk |> List.filter balanced |> List.length
            }
            
        // Launch all async computations in parallel    
        chunks
        |> List.map processChunk
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.sum
        

(* 4: BASIC *)
    
    
    type var = string

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of var              // Variable lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
    
    type stmnt =
    | If of expr * uint32       // Conditional statement (if-then (no else)).
    | Let of var * expr        // Variable update/declaration
    | Goto of uint32           // Goto
    | End                      // Terminate program
      
    type prog = (uint32 * stmnt) list  // Programs are sequences of commands with their own line numbers 

    
    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    
    let fibProg xarg =  
        [(10u, Let("x",    Num xarg))                         // x = xarg
         (20u, Let("acc1", Num 1))                            // acc1 = 1
         (30u, Let("acc2", Num 0))                            // acc2 = 0
         
         (40u, If(Lookup "x", 60u))                           // if x > 0 then goto 60 (start loop)
         
         (50u, Goto 110u)                                     // Goto 110 (x = 0, terminate program)
         
         (60u, Let ("x", Lookup "x" .-. Num 1))               // x = x - 1
         (70u, Let ("result", Lookup "acc1"))                 // result = acc1
         (80u, Let ("acc1", Lookup "acc1" .+. Lookup "acc2")) // acc1 = acc1 + acc2
         (90u, Let ("acc2", Lookup "result"))                 // acc2 = result
         (100u, Goto 40u)                                     // Goto 40u (go to top of loop)
         
         (110u, End)                                          // Terminate program
                                                              // the variable result contains the
                                                              // fibonacci number of xarg
         ]

(* Question 4.1: Basic programs *)

    type basicProgram = Map<uint32, stmnt>

    let mkBasicProgram (p : prog) : basicProgram = Map.ofList p
    let getStmnt (l : uint32) (p : basicProgram) : stmnt = p[l]
    
    let nextLine (l : uint32) (p : basicProgram) : UInt32 = p |> Map.findKey (fun k _ -> k > l)
    
    let firstLine (p : basicProgram) : UInt32 = p |> Map.minKeyValue |> fun (k, _) -> k
                                                
    
(* Question 4.2: State *)

    type state = { lineNumber : UInt32
                   environment : Map<string, int> }
            // type prog = (uint32 * stmnt) list  // Programs are sequences of commands with their own line numbers 

    let emptyState (p : basicProgram) : state = { lineNumber = firstLine p
                                                  environment = Map.empty }
    
    
    let goto (l : UInt32) (st : state) : state = { st with lineNumber = l }

    let getCurrentStmnt (p : basicProgram) (st : state) : stmnt = getStmnt st.lineNumber p
    
    let update (v : var) (a : int) (st : state) : state = { st with environment = st.environment |> Map.add v a }
    
    let lookup (v : var) (st : state) : int = st.environment[v]
    
    
(* Question 4.3: Evaluation *)
    
    let rec evalExpr (e : expr) (st : state) : int =
        match e with
        | Num x -> x
        | Lookup v -> lookup v st
        | Plus (e1, e2) -> (evalExpr e1 st) + (evalExpr e2 st)
        | Minus (e1, e2) -> (evalExpr e1 st) - (evalExpr e2 st)
    
    
    let step (p : basicProgram) (st : state) : state = {st with lineNumber = nextLine st.lineNumber p}
  
        
    let evalProg (p : basicProgram) : state =
        let rec evalProgHelper (st : state) =
            match p[st.lineNumber] with
            | If (e, l) ->
                    if evalExpr e st <> 0
                        then evalProgHelper (goto l st) // or { st with lineNumber = l }
                    else evalProgHelper (step p st)
            | Let (v, e) ->
                    let e' = evalExpr e st
                    let st' = update v e' st // or { st with environment = st.environment |> Map.add v e' }
                    evalProgHelper (step p st')
            | Goto l -> evalProgHelper (goto l st)
            | End -> st
        evalProgHelper (emptyState p)
        
(* Question 4.4: State monad *)
    type StateMonad<'a> = SM of (basicProgram -> state -> 'a * state)  
      
    let ret x = SM (fun _ s -> (x, s))
    
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun p s ->
            let x, s' = a p s
            let (SM g) = f x
            g p s')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM p (SM f) = f p (emptyState p)
    let goto2 (l : UInt32) : StateMonad<unit> = SM (fun _ st -> ((), goto l st)) // goto can be substituted for { st with lineNumber = l }
    
    let getCurrentStmnt2 : StateMonad<stmnt> = SM (fun p st -> (getCurrentStmnt p st, st)) 
    
    let lookup2 (v : var) : StateMonad<int> = SM (fun _ st -> (lookup v st, st))
    let update2 (v : var) (a : int) : StateMonad<unit> = SM (fun _ st -> ((), update v a st))
    
    let step2 : StateMonad<unit> = SM(fun p st -> ((), step p st))

(* Question 4.5: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let rec evalExpr2 (e : expr) : StateMonad<int> =
        match e with
        | Num x -> ret x
        | Lookup v -> lookup2 v
        | Plus (e1, e2) ->
            state {
                let! e1Val = evalExpr2 e1
                let! e2Val = evalExpr2 e2
                return! ret (e1Val + e2Val)
            }
        | Minus (e1, e2) ->
            state {
                let! e1Val = evalExpr2 e1
                let! e2Val = evalExpr2 e2
                return! ret (e1Val - e2Val)
            }
    
    let rec evalProg2 : StateMonad<unit> =
        state {
            let! s' = getCurrentStmnt2
            match s' with
            | If (e, l) ->
                let! e' = evalExpr2 e
                do! (if e' <> 0 then goto2 l else step2)
                do! evalProg2
            | Let (v, e) ->
                let! e' = evalExpr2 e
                do! update2 v e'
                do! step2
                do! evalProg2
            | Goto l ->
                do! goto2 l
                do! evalProg2 
            | End -> return ()
        }