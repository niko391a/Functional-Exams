module Exam2023
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
 module Exam2023 = 
 *)

(* 1: Logic *)

    type prop =  
    | TT  
    | FF  
    | And of prop * prop  
    | Or of prop * prop
    
    let p1 = And(TT, FF)  
    let p2 = Or(TT, FF)  
    let p3 = And(Or(TT, And(TT, FF)), TT)  
    let p4 = And(Or(TT, And(TT, FF)), Or(FF, And(TT, FF)))
    
(* Question 1.1: Evaluation *)
    let rec eval (p : prop) : bool =
        match p with
        | TT -> true
        | FF -> false
        | And(p, q) -> eval p && eval q
        | Or(p, q) -> eval p || eval q
    
(* Question 1.2: Negation and implication *)
    let rec negate (p : prop) : prop =
        match p with
        | TT -> FF
        | FF -> TT
        | And(p, q) -> Or(negate p, negate q)
        | Or(p, q) -> And(negate p, negate q)
    let implies (p : prop) (q : prop) : prop = Or(negate p, q)

(* Question 1.3: Bounded universal quantifiers *)
    let forall (f : 'a -> prop) (lst : 'a list) : prop =
        let rec forallHelper (f : 'a -> prop) (lst : 'a list) (acc : prop) : prop =
            match lst with
            | [] -> acc
            | listHead::listTail ->
                forallHelper f listTail (And(acc, f listHead))
        forallHelper f lst TT // TT as this is the base case
    

(* Question 1.4: Bounded existential quantifiers *)

    let exists (f : 'a -> prop) (lst : 'a list) : prop =
        match lst with
        | [] -> FF
        | lst ->
            let result = List.map f lst
            if List.exists (fun existence -> existence = TT) result then TT else FF
    
(* Question 1.5: Bounded unique existential quantifiers *)

    let existsOne (f : 'a -> prop) (lst : 'a list) : prop =
        match lst with
        | [] -> FF
        | lst ->
            let result = List.map f lst
            let filteredResults = List.filter (fun existence -> existence = TT) result
            if filteredResults.Length > 1 then FF else TT
    
(* 2: Code Comprehension *)
 
    let rec foo xs ys =  
        match xs, ys with  
        | _       , []                  -> Some xs   
        | x :: xs', y :: ys' when x = y -> foo xs' ys'   
        | _       , _                   -> None  
          
    let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys
        | None -> match xs with
                  | [] -> []
                  | x :: xs' -> x :: (bar xs' ys)  

    let baz (a : string) (b : string) =  
        bar [for c in a -> c] [for c in b -> c] |>  
        List.fold (fun acc c -> acc + string c) ""

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
    foo has type 'a list -> 'a list -> 'a list option
    bar has type 'a list -> 'a list -> 'a list
    baz has type string -> string -> string


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
    foo: removes repeated occurrences in a list when comparing two identical lists where the first list is longer than the 2nd
        if the 2nd list is longer than the 1st then None is returned
    bar: also removes repeated occurrences in a list when comparing two identical lists where the first list is longer than the 2nd
        but if the end list is inconsistent instead of returning None it will return the whole 1st list
    baz: Removes identical parts of the first string when comparing it to the 2nd string if the 2nd string
        is longer than the 1st then the 1st string is simply returned
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
    foo would be called: SubtractOrDiscard
    bar would be called: SubtractOrReturn
    baz would be called: SubtractStringOrReturn
    
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `[for c in a -> c]`
    * B: `[for c in b -> c]`
    * C: `List.fold (fun acc c -> acc + string c) ""`

    Q: In the context of the baz function, i.e. assuming that `a` and `b` are strings, 
       what are the types of snippets A, B, and C and what are they -- 
       focus on what they do rather than how they do it.
    
    A: 
    snippet A: Type string -> char list, it converts the string a, into a list of chars
    snippet B: Type string -> char list, it converts the string c, into a list of chars
    snippet C: Type char list -> string -> string, it takes the list of chars and slowly accumulates a string by appending the chars to acc
    
    Q: Explain the use of the `|>`-operator in the baz function.

    A: The piping operator is used to insert the result of bar into the List.fold higher order function.
        It is syntactic sugar as Jesper would call it, as it helps us make the code look cleaner.

    *)

(* Question 2.3: No recursion *) 
    let foo2 (xs : 'a list) (ys : 'a list) =
        if xs.Length < ys.Length then None
        else
            let front, _ = List.splitAt ys.Length xs
            let indexPredicate = (List.forall2 (fun x y -> x = y) front ys) 
            if indexPredicate then
                let _, back = List.splitAt ys.Length xs
                Some (back)
            else None
        

(* Question 2.4 *)
    // let rec bar xs ys =
    //     match foo xs ys with
    //     | Some zs -> bar zs ys
    //     | None -> match xs with
    //               | [] -> []
    //               | x :: xs' -> x :: (bar xs' ys) 
    (*

    Q: The function `bar` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: <Your answer goes here>
        The reason that <x> is not tail recursive is because the recursive call is appended to x before being evaluated
        This means that x :: (bar xs' ys) won't be evaluated until the collection is empty.

    *)
(* Question 2.5 *)
    // let rec bar xs ys =
    //     match foo xs ys with
    //     | Some zs -> bar zs ys
    //     | None -> match xs with
    //               | [] -> []
    //               | x :: xs' -> x :: (bar xs' ys) 
    let barTail (xs : 'a list) (ys : 'a list) : 'a list =
        let rec barTailHelper (xs : 'a list) (ys : 'a list) cont : 'a list =
            match foo xs ys with
            | Some zs -> barTailHelper zs ys cont
            | None -> match xs with
                      | [] -> cont []
                      | x :: xs' -> barTailHelper xs' ys (fun acc -> cont (x :: acc))
        barTailHelper xs ys id

(* 3: Collatz Conjecture *)

(* Question 3.1: Collatz sequences *)

    let collatz (x : int) : int list =
        let rec collatzHelper (x : int) (acc : int list) : int list =
            match x with
            | x when x < 1 -> failwith $"Non positive number: <{x}>"
            | 1 -> List.rev (1 :: acc)
            | x when x % 2 = 0 ->
                collatzHelper (x/2) (x :: acc)
            | x when x % 2 = 1 ->
                collatzHelper ((3*x)+1) (x :: acc)
            
        collatzHelper x List.Empty

(* Question 3.2: Even and odd Collatz sequence elements *)

    let evenOddCollatz (x : int) : int * int =
        let fullSeq = collatz x
        let evenSeq = List.filter (fun y -> y % 2 = 0) fullSeq
        (evenSeq.Length, (fullSeq.Length - evenSeq.Length))

(* Question 3.3: Maximum length Collatz Sequence *)
  // returns (number of longest, length of number)
    let maxCollatz (x : int) (y : int) : int * int =
        let rec maxCollatzHelper index length currentHighest : int * int =
            match index with
            | index when index > y -> (currentHighest, length)
            | index ->
                let currentCollatz = collatz index
                if currentCollatz.Length > length then
                    maxCollatzHelper (index+1) currentCollatz.Length index
                else
                    maxCollatzHelper (index+1) length currentHighest
        maxCollatzHelper x 0 0

(* Question 3.4: Collecting by length *)
    
    // description Add itself to the key with the corresponding length and set the current index as the value for that key
    let collect (x : int) (y : int) : Map<int, Set<int>> =
        let rec collectHelper index (acc : Map<int, Set<int>>) : Map<int, Set<int>> =
            match index with
            | index when index > y -> acc
            | index ->
                let currentCollatzLength = (collatz index).Length
                if acc.ContainsKey currentCollatzLength then
                    let newSet = (acc.Item currentCollatzLength).Add index
                    let newAcc = acc.Add (currentCollatzLength, newSet)
                    collectHelper (index+1) newAcc
                else
                    
                    let newAcc = acc.Add (currentCollatzLength, set [index])
                    collectHelper (index+1) newAcc
        collectHelper x Map.empty
                
    
(* Question 3.5: Parallel maximum Collatz sequence *)
    // Objective: Find the max collatz sequence length between x and y inclusive, by spltting processing up into n amount of threads
    // maxCollatz returns (number of longest, length of number)
    let maxCollatzHelper (xy : int * int) : Async<int * int> =
        async {
            let x, y = xy
            return maxCollatz x y
        }
    let parallelMaxCollatz (x : int) (y : int) (n : int) : int =
        let chunkSize = (y-(x+1))/n
        
        let rec listOfSubranges chunkIndex (acc : (int * int) list ) =
            if chunkIndex >= n then acc
            else
                let start = x + chunkIndex * chunkSize
                let end' = if chunkIndex = n-1 then y else start + chunkSize - 1
                listOfSubranges (chunkIndex+1) (acc @ [(start, end')])
        let ranges = listOfSubranges 0 []
            
        let results = ranges
                      |> List.map maxCollatzHelper
                      |> Async.Parallel
                      |> Async.RunSynchronously
                      |> Array.toList
                      
        let maxResult = results
                        |> List.maxBy snd
                        
        fst maxResult

(* 4: Memory machines *)

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of expr             // Memory lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
          
    type stmnt =  
    | Assign of expr * expr      // Assign value to memory location
    | While  of expr * prog      // While loop
      
    and prog = stmnt list        // Programs are sequences of statements

    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    // Starting from memory {0, 0, 2, 0}
    let fibProg x =  
        [Num 0 .<-. Num x       // {x, 0, 2, 0}
         Num 1 .<-. Num 1       // {x, 1, 2, 0}
         Num 2 .<-. Num 0       // {x, 1, 0, 0}
         While (Lookup (Num 0), 
                [Num 0 .<-. Lookup (Num 0) .-. Num 1  
                 Num 3 .<-. Lookup (Num 1)  
                 Num 1 .<-. Lookup (Num 1) .+. Lookup (Num 2)  
                 Num 2 .<-. Lookup (Num 3)  
                ]) // after loop {0, fib (x + 1), fib x, fib x}
         ]

(* Question 4.1: Memory blocks *)

    type mem = { mutable memory : int[] }
    let emptyMem (x : int) : mem = { memory = Array.create x 0 }
    let lookup (m : mem) (i : int) : int = m.memory[i]
    let assign (m : mem) (i : int) (v : int) : mem =
        m.memory.[i] <- v
        m

(* Question 4.2: Evaluation *)

    let rec evalExpr (m : mem) (e : expr) : int =
        match e with
        | Num x -> x
        | Lookup e' -> lookup m (evalExpr m e')
        | Plus (e1, e2) -> evalExpr m e1 + evalExpr m e2
        | Minus (e1, e2) -> evalExpr m e1 - evalExpr m e2
    let rec evalStmnt (m : mem) (s : stmnt) : mem =
        match s with
        | Assign (e1, e2) -> assign m (evalExpr m e1) (evalExpr m e2)
        | While (e, p) ->
            match evalExpr m e with
            | 0 -> m
            | _ ->
                let m' = evalProg m p
                evalStmnt m' (While (e, p))
    and evalProg (m : mem) (p : prog) : mem =
        match p with
        | [] -> m
        | s1::sn ->
            let m' = evalStmnt m s1
            evalProg m' sn
    
(* Question 4.3: State monad *)
    type StateMonad<'a> = SM of (mem -> ('a * mem) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)  
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM m (SM f) = f m

    let lookup2 (i : int) : StateMonad<int> =
        SM (fun m ->
            if i >= 0 && i < m.memory.Length then
                let v = lookup m i
                Some (v, m)
            else None
        )
        
    let assign2 (i : int) (v : int) : StateMonad<unit> =
        SM (fun m ->
           if i >= 0 && i < m.memory.Length then
               let newMem = assign m i v
               Some ((), newMem)
           else None
        )

(* Question 4.4: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let rec evalExpr2 (e : expr) : StateMonad<int> =
        match e with
        | Num x -> ret x
        | Lookup e' ->
            state {
                let! x = evalExpr2 e'
                return! lookup2 x
            }
        | Plus (e1, e2) ->
            state {
                let! v1 = evalExpr2 e1
                let! v2 = evalExpr2 e2
                return! ret (v1+v2)
            }
        | Minus (e1, e2) ->
            state {
                let! v1 = evalExpr2 e1
                let! v2 = evalExpr2 e2
                return! ret (v1-v2)
            }
        // With monads
        // match e with
        // | Num x -> ret x
        // | Lookup e' -> evalExpr2 e' >>= lookup2
        // | Plus (e1, e2) ->
        //     evalExpr2 e1 >>= fun v1 ->
        //         evalExpr2 e2 >>= fun v2 -> ret (v1+v2)
        // | Minus (e1, e2) ->
        //     evalExpr2 e1 >>= fun v1 ->
        //         evalExpr2 e2 >>= fun v2 -> ret (v1-v2)
        
    let evalStmnt2 (s : stmnt) : StateMonad<unit> =
        match s with
        | Assign (e1, e2) ->
            state {
                let! v1 = evalExpr2 e1
                let! v2 = evalExpr2 e2
                return! assign2 v1 v2
            }
            
        | While (e, p) ->
            state {
                let! val1 = evalExpr2 e
                match val1 with
                | 0 -> return ()
                | _ ->
                    let! m' = evalProg2 p
                    return! m' (While (e, p))
                }

    and evalProg2 (p : prog) : StateMonad<unit> = 
        match p with
        | [] -> state { return () }
        | s1::sn ->
            state {
                do! evalStmnt2 s1
                return! evalProg2 sn 
            }
            
    
(* Question 4.5: Parsing *)
    
    open JParsec.TextParser
      
    let ParseExpr, eref = createParserForwardedToRef<expr>()  
    let ParseAtom, aref = createParserForwardedToRef<expr>()  
      
    let parseExpr _ = failwith "not implemented" // Parse addition and minus
          
    let parseAtom _ = failwith "not implemented" // Parse numbers and lookups

//    Uncomment the following two lines once you