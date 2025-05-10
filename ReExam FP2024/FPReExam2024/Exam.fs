module Exam2024

open System
open System.Xml.Xsl    
    
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find when switching back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2024 = 
 *)

(* 1: Transactions *)
    
    type shape =
        | Rectangle of float * float
        | Circle of float
        | Triangle of float * float
        
    type shapeList =
        | Empty
        | AddShape of shape * shape * shapeList
        
    let area (s : shape) : float =
        match s with
        | Rectangle (w, h) -> w*h 
        | Circle r -> Math.PI * Math.Pow(r, 2.0)
        | Triangle (b, h) -> (b*h) / 2.0
    
    let circumference (s : shape) : float =
        match s with
        | Rectangle (w, h) -> (2.0*w)+(2.0*h) 
        | Circle r -> 2.0 * Math.PI * r
        | Triangle (b, h) -> b + h + Math.Sqrt(Math.Pow(b, 2.0) + Math.Pow(h, 2.0))
         
    let rec totalArea (sl : shapeList) : float =
        match sl with
        | Empty -> 0.0
        | AddShape (x, y, tail) -> area x + area y + totalArea tail 
        
    let totalCircumference (sl : shapeList) : float =
        let rec tailHelper (sl : shapeList) acc : float =
            match sl with
            | Empty -> acc
            | AddShape (x, y, tail) -> tailHelper tail (acc + (circumference x + circumference y))
        tailHelper sl 0.0
        
    let shapeListFold  (f : 'a -> shape -> 'a) (acc : 'a) (sl : shapeList) : 'a =  // self-note check these if time
        let rec tailHelper accumulator sl =
            match sl with
            | Empty -> accumulator
            | AddShape (x, y, tail) ->
                let acc' = f acc x
                let acc'' = f acc' y
                tailHelper acc'' tail
        tailHelper acc sl
        
    let isCircle =
        function
        | Circle _ -> true
        | _        -> false

    let containsCircle trs = 
        shapeListFold (fun acc c -> acc || isCircle c) false trs
    
    let totalAreaHelper acc shape = acc + area shape  // self-note check these if time
    let totalArea2 (sl : shapeList) : float = shapeListFold totalAreaHelper 0.0 sl  // self-note check these if time
    
    let totalCircumferenceHelper acc shape = acc + circumference shape  // self-note check these if time
    let totalCircumference2 (sl : shapeList) : float = shapeListFold totalCircumferenceHelper 0.0 sl  // self-note check these if time
    
(* 2: Code Comprehension *)
        
    let foo =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when c > 'w'             -> char (int c - 23)
        | c when c < 'x'             -> char (int c + 3)
        
    let bar (str : string) = [for c in str -> c]
    
    let baz str =
        let rec aux = 
            function
            | [] -> ""
            | c :: cs -> string (foo c) + (aux cs)
            
        aux (bar str)
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: foo (char -> char)
       bar (string -> char list)
       baz (string -> string)

    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo: evaluates the ascii value of the char and changes the calculation of what ascii to return based on if the ascii value is larger than 'w' or less than 'x'
       If a white space is given it is simply returned. Caesar cipher, it returns a char with the input chars ascii value shifted 3 times down the alphabet.
        
       bar: takes a string and splits the string up into a list of chars.
       If the string is empty an empty char list is returned
       
       baz: Takes a string and scrambles it using foo recursively, by splitting the string into its head and tail and inputting the head to foo
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo -> charScrambler
       bar -> toCharList
       baz -> stringScrambler
*)
    
(* Question 2.2 *)

    let foo2 =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when c > 'w'             -> char (int c - 23)
        | c when c < 'x'             -> char (int c + 3)
        | _ -> failwith "Unhandled Char"

(* Question 2.3 *)
    
    let baz2 (str : String ) =
        bar str |>  List.fold (fun acc c -> acc + string (foo c)) ""
    
(* Question 2.4 *)

    (*

    Q: The function `baz` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo- or the bar functions. 
       You are allowed to evaluate these function immediately.
       
    A: It is not tail recursive because instead of doing computations right away in the accumulator, we wait for the recursive loop to finish and then unwind it
    So baz looks like this:
    
    let baz str =
        let rec aux = 
            function
            | [] -> ""
            | c :: cs -> string (foo c) + (aux cs)
        aux (bar str)
        
    baz "hello" ->
        foo 'h' + aux "ello" ->
        foo 'h' + (foo 'e' + (aux "llo")) ->
        foo 'h' + (foo 'e' + (foo 'l' + (aux "lo"))) ->
        foo 'h' + (foo 'e' + (foo 'l' + (foo 'l' + (aux "o")))) ->
        foo 'h' + (foo 'e' + (foo 'l' + ((foo 'l' + (foo '0' + (aux ""))))) ->
     
     Iterative functions have all recursive calls as their last operation, which this does not in the evaluation ´´string (foo c) + (aux cs)´´
    *)
    
(* Question 2.5 *)
    
    let bazTail _ = failwith "not implemented"
        
(* 3: Atbash Ciphers *)

(* Question 3.1 *)
    let encryptCharHelper =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when  c >= 'a' && c <= 'z' ->
            let alphabetStart  = int 'a'
            let alphabetEnd = int 'z'
            let charCode = int c
            let reversedChar = alphabetEnd - (charCode - alphabetStart)
            char reversedChar
        | c -> c
    let toCharArray (str : string) = [for c in str -> c]
    let encrypt str =
        let rec encryptHelper = 
            function
            | [] -> ""
            | c :: cs -> string (encryptCharHelper c) + (encryptHelper cs)
            
        encryptHelper (toCharArray str)
    
(* Question 3.2 *)

    let decrypt str = encrypt str
    
(* Question 3.3 *)

    let splitAt (i : int) (str : string) : string list =
        let rec tailHelper (i : int) (str : string) (acc : string list) : string list =
            match str with
            | str when str.Length = 0 -> acc
            | _ ->
                if str.Length > i then
                    let currentChunk = [str[..(i-1)]]
                    let remainingString = str[i..]
                    tailHelper i remainingString (acc @ currentChunk)
                else
                    let currentChunk = [str[..i]]
                    tailHelper i "" (acc @ currentChunk)
        tailHelper i str []
    
(* Question 3.4 *)
    let asyncEncryptCharHelper =
        function
        | c when Char.IsWhiteSpace c -> c 
        | c when  c >= 'a' && c <= 'z' ->
            let alphabetStart  = int 'a'
            let alphabetEnd = int 'z'
            let charCode = int c
            let reversedChar = alphabetEnd - (charCode - alphabetStart)
            char reversedChar
        | c -> c
        
    let asyncEncrypt str =
        async {
        let rec encryptHelper = 
            function
            | [] -> ""
            | c::cs -> string (encryptCharHelper c) + (encryptHelper cs)
        return encryptHelper (toCharArray str) }
    let asyncToCharArray (str : string) = [for c in str -> c]
    
    let parEncrypt str i  =
        splitAt i str |> List.map asyncEncrypt |> Async.Parallel |> Async.RunSynchronously
        
    
(* Question 3.5 *)
        
    open JParsec.TextParser

    let parseEncrypt : Parser<string> =
        let encryptChar c =
            match c with
            | c when Char.IsWhiteSpace c -> c 
            | c when Char.IsLetter c ->
                let alphabetStart  = int 'a'
                let alphabetEnd = int 'z'
                
                let charCode = int (Char.ToLower c)
                let reversedChar = alphabetEnd - (charCode - alphabetStart)
                char reversedChar
            | _ -> failwith "invalid char encountered"
                    
        // Parse a single valid character and encrypt it
        let charParser = 
            satisfy (fun c -> Char.IsLetter c || Char.IsWhiteSpace c)
            |>> encryptChar
        
        // Parse many valid characters and join them into a string
        many charParser 
        |>> (List.map string >> String.concat "")

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    // as lists have a guarenteed order in F# i will use them
    type clicker = { wheelCharacters : char array
                     wheelPositions : int array }
    
    let newClicker (wheel : char list) (numWeel : int) : clicker = { wheelCharacters = List.toArray wheel
                                                                     wheelPositions =  Array.init numWeel (fun _ -> 0)}

(* Question 4.2 *)
    
    let click (cl : clicker) : clicker =
        let wheelOptions = cl.wheelCharacters.Length-1
        let wheelAmount = cl.wheelPositions.Length-1
        
        let incrementWheel (index : int) (c : clicker) : clicker = 
            let newPositions =
                c.wheelPositions
                |> Array.mapi (fun i x -> if i = index then x + 1 else x)
            { c with wheelPositions = newPositions }
            
        let rec checkIncrements (index : int) (c : clicker) : clicker =
            if c.wheelPositions[index] > wheelOptions then
                if index = 0 then { c with wheelPositions = Array.init (wheelAmount+1) (fun _ -> 0) }
                else
                    let resettedWheel = c.wheelPositions
                                     |> Array.mapi (fun i x -> if i = index then 0 else x)
                    let newWheel = { c with wheelPositions = resettedWheel }
                    checkIncrements (index-1) (incrementWheel (index-1) newWheel)
            else c
            
        incrementWheel wheelAmount cl |> checkIncrements wheelAmount
        
    let read (cl : clicker ) : string =
        cl.wheelPositions
        |> Array.map (fun pos -> Array.item pos cl.wheelCharacters)
        |> String 
            
    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (clicker -> 'a * clicker)  
      
    let ret x = SM (fun cl -> (x, cl))
    
    let bind f (SM a) : StateMonad<'b> =
        SM (fun cl ->
               let x, cl'  = a cl
               let (SM g) = f x
               g cl')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM cl (SM f) = f cl
    
    let click2 : StateMonad<unit> =
        SM (fun currenClicker ->
            let newClicker = click currenClicker
            ((), newClicker))
    
    let read2 : StateMonad<string> =
        SM (fun currentClicker -> ((read currentClicker), currentClicker))

(* Question 4.4 *)
    
    let multipleClicks (x : int) : StateMonad<string list> =
        let rec clickHelper x (acc : StateMonad<string list>) : StateMonad<string list> =
            if x > 0 then
                click2 >>= fun () ->
                read2 >>= fun currentState ->
                acc >>= fun currentAcc ->
                clickHelper (x - 1) (ret (currentAcc @ [currentState]))
            else
                acc
        read2 >>= fun initialState -> clickHelper (x - 1) (ret [initialState])
            
        
(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()
    
    let multipleClicks2 _ = failwith "not implemented"