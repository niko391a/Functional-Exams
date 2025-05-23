﻿module ReExam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022_2 = 
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
    let rec maxDepth (img : grayscale) : int =
        match img with
        | Square _ -> 0
        | Quad(grayscale, grayscale1, grayscale2, grayscale3) -> 1 + (maxDepth grayscale) + (maxDepth grayscale1) + (maxDepth grayscale2) + (maxDepth grayscale3)
    
(* Question 1.2 *)
    let rec mirror (img : grayscale) : grayscale =
        match img with
        | Square x -> Square x
        | Quad(grayscale, grayscale1, grayscale2, grayscale3) -> Quad(mirror grayscale1, mirror grayscale, mirror grayscale3, mirror grayscale2)

(* Question 1.3 *)
    let rec operate (f : grayscale -> grayscale -> grayscale -> grayscale -> grayscale) (img : grayscale) : grayscale =
        match img with
        | Square v -> Square v
        | Quad (a, b, c, d) -> f (operate f a) (operate f b) (operate f c) (operate f d)
    
    let mirror2 (img : grayscale) : grayscale = operate (fun g1 g2 g3 g4 -> Quad(g2, g1, g4, g3)) img

(* Question 1.4 *)

    let compress (img : grayscale) : grayscale =
        let rec aux img =
            match img with
            | Square x -> Square x
            | Quad (g1, g2, g3, g4) ->
                let c1 = aux g1
                let c2 = aux g2
                let c3 = aux g3
                let c4 = aux g4
                if c1 = c2 && c2 = c3 && c3 = c4 then c1
                else Quad(c1, c2, c3, c4)
        aux img

(* 2: Code Comprehension *)
    let rec foo f =
        function
        | []               -> []
        | x :: xs when f x -> x :: (foo f xs)
        | _ :: xs          -> foo f xs
            
    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
    foo has type ('a -> bool) -> 'a list -> 'a list
    bar has type ('a -> bool) list -> 'a list -> 'a list

    Q: What do the functions foo and  bar do. 
       Focus on what it does rather than how it does it.

    A: 
    foo filters the given list based on the specified predicate
    ex:
    foo (fun x -> x % 2 = 0) [1; 2; 3; 4; 5;]
    val it: int list = [2; 4]

    bar filters the given collection based on one or more given predicates
    bar [(fun x -> x % 2 = 0); (fun x -> x > 3)] [1; 2; 3; 4; 5; 6]
    val it: int list = [4; 6]

    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: 
    foo would be called FilterList
    bar would be called MultiFilterList
        
    Q: The function foo uses an underscore `_` in its third case. 
       Is this good coding practice, if so why, and if not why not?
    
    A: In general if you do not need the variable that you've matched, then you simply replace it with an x, this also means that any value for x would be acceptable,
    given that it is not used.
    *)
        

(* Question 2.2 *)
    let bar2 fs xs = fs |> List.map (fun predicate -> List.filter predicate xs) 

(* Question 2.3 *) 

    let baz fs xs =
        let rec aux fs acc =
            match fs with
            | [] -> acc
            | p::ps ->
                let newAcc = foo p acc
                aux ps newAcc
        aux fs xs

(* Question 2.4 *)

    (*

    Q: Only one of the functions `foo` and `bar` is tail recursive. Which one? 
       Demonstrate why the other one is not tail recursive.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).


    A: 
    bar is tail recursive and foo is not.
    The reason that foo is not tail recursive is because the recursive call to foo is not the last operation performed.     
    After each recursive call returns, the function still needs to append the x which satisfied the predicate to the result.     
    This means that the function must save each call frame on the stack to finish the string concatenation after returning from the recursive call.     
    In a tail-recursive function, there would be no additional work to do after the recursive call.

    This can also be seen for the evaluation:
    foo (fun x -> x % 2 = 0) [1; 2; 3; 4]
    foo f [2; 3; 4]
    2 :: (foo f [3; 4])
    2 :: (foo f [4])
    2 :: (4 :: (foo f []))
    2 :: (4 :: ([]))
    2 :: (4 :: [])
    2 :: 4
    [2; 4]
     *)

(* Question 2.5 *)  
    // only implement the one that is NOT already tail recursive
    let fooTail f xs =
        let rec aux f xs acc =
            match xs with
            | []               -> []
            | x :: xs when f x -> aux f xs (x :: acc)
            | _ :: xs          -> foo f xs
        aux f xs []
    let barTail _ = failwith "not implemented"


(* 3: Guess a number *)

    type guessResult = Lower | Higher | Equal
    type oracle =
        { max : int
          f : int -> guessResult }

(* Question 3.1 *)

    let validOracle _ = failwith "not implemented"

(* Question 3.2 *)

    let randomOracle _ = failwith "not implemented"

(* Question 3.3 *)
    
    let findNumber _ = failwith "not implemented"

(* Question 3.4 *)
    let evilOracle _ = failwith "not implemented"
    
(* Question 3.5 *)
    let parFindNumber _ = failwith "not implemented"

(* 4: Assembly *)

    type register = R1 | R2 | R3
    type address = uint

    type assembly =
    | MOVI of register * int
    | MULT of register * register * register
    | SUB of register * register * register
    | JGTZ of register * address
    
     
    let factorial x =           // Address
        [MOVI (R1, 1)           // 0
         MOVI (R2, x)           // 1
         MOVI (R3, 1)           // 2
         MULT (R1, R1, R2)      // 3 (Loop starts here)
         SUB  (R2, R2, R3)      // 4
         JGTZ (R2, 3u)]         // 5 (Loop ends here)
    
(* Question 4.1 *)

    type program = unit (* replace this entire type with your own *)
    let assemblyToProgram _ = failwith "not implemented"

(* Question 4.2 *)

    type state = unit (* replace this entire type with your own *)
    let emptyState _ = failwith "not implemented"
    

(* Question 4.3 *)

    let setRegister _ = failwith "not implemented"
    
    let getRegister _ = failwith "not implemented"
    
    let setProgramCounter _ = failwith "not implemented"
    
    let getProgramCounter _ = failwith "not implemented"
    
    let getProgram _ = failwith "not implemented"
    
(* Question 4.4 *)
    
    type StateMonad<'a> = SM of (state -> 'a * state)

    let ret x = SM (fun s -> x, s)
    let bind f (SM a) : StateMonad<'b> = 
      SM (fun s -> 
      let x, s' = a s
      let (SM g) = f x
      g s')

    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM prog (SM f) = f (emptyState prog)

    let setReg _ = failwith "not implemented"
    
    let getReg _ = failwith "not implemented"
    
    let setPC _ = failwith "not implemented"
    
    let incPC _ = failwith "not implemented"
    
    let lookupCmd _ = failwith "not implemented"
    


(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runProgram _ = failwith "not implemented"
    