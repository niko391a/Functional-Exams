module Exam2022

    open JParsec

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
 module Exam2022 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let countWhite (img : grayscale) : int =
        let rec aux img : int =
            match img with
            | Square 255uy -> 1
            | Quad(grayscale, grayscale1, grayscale2, grayscale3) -> aux grayscale + aux grayscale1 + aux grayscale2 + aux grayscale3
            | _ -> 0
        aux img
    
(* Question 1.2 *)
    let rotateRight (img : grayscale) : grayscale =
        let rec aux img : grayscale =
            match img with
            | Square x -> Square x
            | Quad(grayscale, grayscale1, grayscale2, grayscale3) -> Quad(aux grayscale3, aux grayscale, aux grayscale1, aux grayscale2)
        aux img
        

(* Question 1.3 *)
    let map (mapper : uint8 -> grayscale) (img : grayscale) : grayscale  =
        let rec aux (mapper : uint8 -> grayscale) (img : grayscale) : grayscale =
            match img with
            | Square x -> x |> mapper
            | Quad(grayscale, grayscale1, grayscale2, grayscale3) -> Quad(aux mapper grayscale, aux mapper grayscale1, aux mapper grayscale2, aux mapper grayscale3)
        aux mapper img
    
    let bitmap (img : grayscale) : grayscale = map (fun x -> if x <= 127uy then Square 0uy else Square 255uy) img

(* Question 1.4 *)

    let rec fold (folder : 'a -> uint8 -> 'a) (acc : 'a) (img : grayscale) : 'a =
        match img with
        | Square x -> folder acc x
        | Quad(g1, g2, g3, g4) ->
                                let acc1 = fold folder acc g1
                                let acc2 = fold folder acc1 g2
                                let acc3 = fold folder acc2 g3
                                let acc4 = fold folder acc3 g4
                                acc4
    // let countWhite (img : grayscale) : int =
    //     let rec aux img : int =
    //         match img with
    //         | Square 255uy -> 1
    //         | Quad(grayscale, grayscale1, grayscale2, grayscale3) -> aux grayscale + aux grayscale1 + aux grayscale2 + aux grayscale3
    //         | _ -> 0
    //     aux img
    let countWhite2 (img : grayscale) : int = int (fold (fun acc x -> if x = 255uy then acc + 1uy else acc) 0uy img)

(* 2: Code Comprehension *)
    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
    foo has type int -> string
    bar has type int list -> string list


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: 
    foo converts an int to binary 
    bar converts a list of integers to their binary counterparts
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: 
    foo would be toBinary()
    bar would be listToBinary()
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A:
    the input must be a non negative number and larger than 0
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: 
    the warning is:
    stdin(4,9): warning FS0025: Incomplete pattern matches on this expression. 
    For example, the value '1' may indicate a case not covered by the pattern(s). 
    However, a pattern rule with a 'when' clause might successfully match this value.
    
    This is due to the function not having an explicit pattern for negative integers

    *)

    let rec foo2 (i : int) =
        match i with
        | x when x < 0 -> failwith $"{x} mut be larger or equal to 0"
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"
        | _ -> failwith "Not a valid input"


(* Question 2.3 *) 
    let bar2 (lst : int list) : string list = lst |> List.map foo

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: 
       foo 5
       foo (5 / 2) + "1"
       foo (foo (2 / 2) + "0") + "1"
       foo (foo (foo (1 / 2) + "1") + "0") + "1"
       foo (foo ("" + "1") + "0") + "1"
       foo ("1" + "0") + "1"
       "10" + "1"
       "101"
    
    The reason that foo is not tail recursive is because the recursive call to foo is not the last operation performed.
    After each recursive call returns, the function still needs to append either "0" or "1" to the result.
    This means that the function must save each call frame on the stack to finish the string concatenation after returning from the recursive call.
    In a tail-recursive function, there would be no additional work to do after the recursive call.
        
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A:
    foo does not run the risk due to the limitation of what is allowed in a 32bit signed integer

    *)

(* Question 2.5 *)

    let fooTail (x : int) : string =
        let rec aux x acc : string =
            match x with
            | 0 -> acc
            | x when x % 2 = 0 -> aux (x / 2) (acc + "0")
            | x when x % 2 = 1 -> aux (x / 2) (acc + "1")
        aux x ""

(* Question 2.6 *)
    let barTail (lst : int list) : string list =
        let rec aux (lst : int list) (acc : string list) : string list =
            match lst with
            | [] -> acc
            | x :: xs ->
                let x' = foo x
                aux xs (acc @ [x'])
        aux lst List.Empty

(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)

    let failDimensions (m1 : matrix) (m2 : matrix) : 'a = failwith $"Invalid matrix dimensions: m1 rows = {numRows m1}, m1 columns = {numCols m1},
    m2 roms = {numRows m2}, m2 columns = {numCols m2}"

(* Question 3.2 *)

    let add  (m1 : matrix) (m2 : matrix) : matrix =
        if numCols m1 <> numCols m2 || numRows m1 <> numRows m2 then failDimensions m1 m2
        else
            let rows = numRows m1
            let columns = numCols m1
            init (fun r c -> get m1 r c + get m2 r c) rows columns

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct (m1 : matrix) (m2 : matrix) (row : int) (col : int) : int =
        let numColsM1 = numCols m1
        let numRowsM2 = numRows m2

        let rec aux index sum =
            match index with
            | i when i = numColsM1 -> sum // Or numRowsM2, they are equal
            | i ->
                let elementM1 = get m1 row i
                let elementM2 = get m2 i col
                aux (i + 1) (sum + elementM1 * elementM2)
        aux 0 0
        
    let mult (m1 : matrix) (m2 : matrix) : matrix =
        let m1Cols = numCols m1
        let m2Rows = numRows m2
        let m1Rows = numRows m1
        let m2Cols = numCols m2

        if m1Cols <> m2Rows then failDimensions m1 m2
        else
            init (fun r c -> dotProduct m1 m2 r c) m1Rows m2Cols

(* Question 3.4 *)
    let parInit (f : int -> int -> int) (rows : int) (cols : int) : matrix =
        // Initialize an empty matrix with the correct dimensions
        let matrix = init (fun _ _ -> 0) rows cols
        
        // Generate list of coordinates
        let coordinates = [ for i in 0 .. rows - 1 do for j in 0 .. cols - 1 -> (i, j) ]
        
        // Run all async operations in parallel and wait for them to complete
        coordinates
        |> List.map (fun (i, j) -> async { set matrix i j (f i j)})
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        
        // Return the initialized matrix
        matrix

(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = int list
    let emptyStack (unit : unit) : stack = List.Empty

(* Question 4.2 *)

    let runStackProgram (prog : stackProgram) : int =
        let rec aux (prog : stackProgram) (acc : stack) =
            match prog with
            | [] -> acc
            | progHead::progTail ->
                match progHead with
                | Push x -> aux progTail (x :: acc)
                | Add ->
                    let p1' = acc[0]
                    let p2' = acc[1]
                    let evaluation = p1' + p2' 
                    let _, newAcc = List.splitAt 2 acc
                    aux progTail (evaluation :: newAcc)
                | Mult ->
                    let p1' = acc[0]
                    let p2' = acc[1]
                    let evaluation = p1' * p2' 
                    let _, newAcc = List.splitAt 2 acc
                    aux progTail (evaluation :: newAcc)
        (aux prog (emptyStack())).Head

(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
    let bind f (SM a) : StateMonad<'b> = 
        SM (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (SM g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    let push (x : int) : StateMonad<unit> = SM (fun (st : stack) -> Some ((), x::st))
    let pop : StateMonad<int> = SM (fun st ->
        if st.IsEmpty then
            None
        else
            let _, st' = List.splitAt 1 st
            Some (st.Head, st'))

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()
    let runStackProg2 (prog : stackProgram) : StateMonad<int> =
        let rec aux (prog : stackProgram) : StateMonad<int> =
            state {
                match prog with
                | [] ->  return! pop
                | progHead::progTail ->
                    match progHead with
                    | Push x ->
                        do! push x
                        return! aux progTail
                    | Add ->
                        let! p1 = pop
                        let! p2 = pop
                        do! push (p1 + p2)
                        return! aux progTail
                    | Mult ->
                        let! p1 = pop
                        let! p2 = pop
                        do! push (p1 * p2)
                        return! aux progTail
            }
        aux prog

    
(* Question 4.5 *)
    
    open JParsec.TextParser
    let spaces : Parser<char list> = many (pchar ' ')
    let parseStackProg : Parser<'a> = 
        let pPush = pstring "PUSH" .>> spaces >>. pint32 |>> Push
        let pAdd =  pstring "ADD" |>> (fun _ -> Add)
        let pMult =  pstring "MULT" |>> (fun _ -> Mult)
        let pCmd = (pPush <|> pAdd <|> pMult)
        many pCmd .>> spaces



         