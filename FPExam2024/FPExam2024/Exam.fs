module Exam2024

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

    type transactions =
        | Empty
        | Pay     of string * int * transactions
        | Receive of string * int * transactions
        
    let rec balance (trs : transactions) : int =
        match trs with
        | Empty -> 0
        | Pay (_, amount, trs) -> (-amount) + balance trs
        | Receive (_, amount, trs) -> amount + balance trs
        
    let balanceAcc (trs : transactions) : int =
        let rec balanceHelper (trs : transactions) (acc : int) : int =
            match trs with
            | Empty -> acc
            | Pay (_, amount, trs) -> balanceHelper trs (acc - amount)
            | Receive (_, amount, trs) -> balanceHelper trs (acc + amount)
        balanceHelper trs 0
        
    let participants (trs : transactions) : Set<string> * Set<string> =
        let rec participantHelper (trs : transactions) (acc : Set<string> * Set<string>) : Set<string> * Set<string> =
            match trs with
            | Empty -> acc
            | Pay (name, _, trs) ->
                match acc with
                | (paid, received) -> participantHelper trs (paid.Add name, received)
            | Receive (name, _, trs) ->
                match acc with
                | (paid, received) -> participantHelper trs (paid, received.Add name)
        participantHelper trs (Set.empty, Set.empty)
    
    
    let rec balanceFold (payFolder : 'a -> string -> int -> 'a) (receiveFolder : 'a -> string -> int -> 'a) acc (trs : transactions) : 'a =
        match trs with
            | Empty -> acc
            | Pay (name, amount, trs) ->
                  let acc' = payFolder acc name amount
                  balanceFold payFolder receiveFolder acc' trs
            | Receive (name, amount, trs) ->
                  let acc' = receiveFolder acc name amount
                  balanceFold payFolder receiveFolder acc' trs
                
    let collect (trs: transactions) : Map<string, int> =
        let rec collectHelper (trs : transactions) (acc : Map<string, int>) : Map<string, int> =
            match trs with
            | Empty -> acc
            | Pay (name, amount, trs) ->
                if acc.ContainsKey name then
                    let currentAmount = acc.Item name 
                    let acc' =  acc.Add (name, (currentAmount-amount))
                    collectHelper trs acc'
                else 
                    let acc' =  acc.Add (name, -amount)
                    collectHelper trs acc'
            | Receive(name, amount, trs) ->
                if acc.ContainsKey name then
                    let currentAmount = acc.Item name
                    let acc' =  acc.Add (name, currentAmount+amount)
                    collectHelper trs acc'
                else
                    let acc' =  acc.Add (name, amount)
                    collectHelper trs acc'
        collectHelper trs Map.empty
    
    
(* 2: Code Comprehension *)
        
    let foo (x : char) = x |> int |> fun y -> y - (int '0')
    
    let bar (x : string) = [for c in x -> c]
            
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
    
(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
    foo (char -> int)
    bar (string -> char list)
    baz (int list -> int)


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
    foo returns the inserted chars ascii value subtracted with the ascii value of '0'
    bar returns the string split up into a list of chars
    baz reverses the inputted list and combines them into a single int
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:
    foo -> shiftAscii48()
    bar -> stringToCharList()
    baz -> ReverseAndCombineIntList()
    
    
    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: The input must be a char so no inputs longer than 1, the output also doesn't make sense if a char with ascii value below 0 is inserted, 
    as this would return a negative number which cannot be translated
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: If some or more of the inputted values in the list are above 9 the outputted int will not be reversed perfectly
    so [10; 20; 30] wont become 302010 but instead 3210
    *)
    
(* Question 2.2 *)
    
    let stringToInt = bar >> List.map foo >> List.rev >> baz
      

(* Question 2.3 *)
        // let rec baz =
        // function
        // | [] -> 0
        // | x :: xs -> x + 10 * baz xs
    let baz2 (x : int list) : int = List.foldBack (fun head acc -> head + 10 * acc) x 0
    
(* Question 2.4 *)

    (*
    let bar (x : string) = [for c in x -> c]

    Q: The function `bar` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: <Your answer goes here>
    bar "abc"
    --> [for c in "abc" -> c]
    --> 'a' :: [for c in "bc" -> c]
    --> 'a' :: ('b' :: [for c in "c" -> c])
    --> 'a' :: ('b' :: ('c' :: [for c in "" -> c]))
    --> 'a' :: ('b' :: ('c' :: []))
    --> 'a' :: ('b' :: ['c'])
    --> 'a' :: ['b'; 'c']
    --> ['a'; 'b'; 'c']
    *)
    
(* Question 2.5 *)
    let bazTail (input : int list) : int =
        let rec bazHelper (input : int list) (acc : int) : int = 
            match input with
            | [] -> acc
            | x::xs -> bazHelper xs (x + 10 * acc)
        bazHelper (List.rev input) 0
(* 3: Caesar Ciphers *)

(* Question 3.1 *)
    let encrypt (text : string) (offset : int) : string =
        let alphabetStart = 'a'
        let alphabetEnd = 'z'
        List.ofSeq text |> List.map (fun ch ->
            if ch = ' ' then ch
            else
                let offsetWithRotations = offset % 26
                let lettersLeft = int alphabetEnd - int ch
                // if there are not enough letters left we must circle back
                if int lettersLeft < offsetWithRotations then
                    let newOffset = offsetWithRotations - lettersLeft
                    char (int alphabetStart + (newOffset-1))
                else 
                    char (int ch + offsetWithRotations)
        ) |> Array.ofList |> System.String
    
(* Question 3.2 *)
    let decrypt (text : string) (offset : int) : string =
        let alphabetStart = 'a'
        let alphabetEnd = 'z'
        List.ofSeq text |> List.map (fun ch ->
            if ch = ' ' then ch
            else
                let offsetWithRotations = offset % 26
                let lettersLeft = int ch - int alphabetStart
                // if there are not enough letters left we must circle back
                if int lettersLeft < offsetWithRotations then
                    let newOffset = offsetWithRotations - lettersLeft
                    char (int alphabetEnd - (newOffset-1))
                else 
                    char (int ch - offsetWithRotations)
        ) |> Array.ofList |> System.String
    
(* Question 3.3 *)
    let decode (plainText : string) (encryptedText : string) : int option =
        let rec decryptHelper (plainText : string) (encryptedText : string) offset : int option =
            match offset with
            | 26 -> None // possible offsets range from 0-25 so if it hits 26 then there is no offest that would be valid
            | correctOffset when encrypt plainText correctOffset = encryptedText -> Some correctOffset
            | _ -> decryptHelper plainText encryptedText (offset+1)
        decryptHelper plainText encryptedText 0
    
(* Question 3.4 *)
    let asyncEncrypt (text : string) (offset : int) =
        async {
            let alphabetStart = 'a'
            let alphabetEnd = 'z'
            let result =
                    List.ofSeq text |> List.map (fun ch ->
                        if ch = ' ' then ch
                        else
                            let offsetWithRotations = offset % 26
                            let lettersLeft = int alphabetEnd - int ch
                            // if there are not enough letters left we must circle back
                            if int lettersLeft < offsetWithRotations then
                                let newOffset = offsetWithRotations - lettersLeft
                                char (int alphabetStart + (newOffset-1))
                            else 
                                char (int ch + offsetWithRotations)
                    ) |> Array.ofList |> System.String
            return result
        }
        
    let parEncrypt (text : string) (offset : int) : string =
        let splitText = text.Split " "
        splitText
        |> Array.map (fun word -> asyncEncrypt word offset)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> String.concat " "
    
(* Question 3.5 *)
        
    open JParsec.TextParser
    // > run (parseEncrypt 0) "hello world"
    // val it: ParserResult<string> =
    // Success "hello world"
    let rec parseEncrypt (offset : int) : Parser<string> =
        // Parse a single valid character and encrypt it
        let textParser = asciiLetter <|> pchar ' ' |>> string |>> (fun ch -> encrypt ch offset)
        
        // Parse many valid characters and join them into a string
        many textParser
        |>> (List.map string
        >> String.concat "")
        

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = { letterMap : Map<string, string list> }
    
    let empty () : letterbox = { letterMap = Map.empty }

(* Question 4.2 *)

    let post (sender : string) (message : string) (mb : letterbox) : letterbox =
        if mb.letterMap.ContainsKey sender then 
            let messageList = mb.letterMap.Item sender
            let newMessagelist = messageList @ [message]
            let finalMap = mb.letterMap.Add(sender, newMessagelist)
            { mb with letterMap = finalMap }
        else
            let finalMap = mb.letterMap.Add(sender, [message])
            { mb with letterMap = finalMap }
    
    let read (sender : string) (mb : letterbox) : (string * letterbox) option =
        if mb.letterMap.ContainsKey sender then
            let messageList = mb.letterMap.Item sender
            let desiredMessage = messageList[0]
            let finalList = messageList.Tail
            let finalMap = mb.letterMap.Add(sender, finalList)
            Some (desiredMessage , { mb with letterMap = finalMap })
        else None

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (letterbox -> ('a * letterbox) option)  
      
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
      
    let evalSM (SM f) = f (empty ())
    
    let post2 (sender : string) (message : string) : StateMonad<unit> =
        SM ( fun currentLetterbox ->
            let mb = post sender message currentLetterbox
            Some ((), mb)
        )
        
    let read2 (sender : string) : StateMonad<string> =
        SM ( fun currentLetterbox ->
            let letterMessage = read sender currentLetterbox
            match letterMessage with
            | None -> None
            | Some (message, mb) -> Some(message, mb)
        ) 
        

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    type MType =
        | Post of string * string
        | Read of string
    type log = MType list
    
    let trace _ = failwith "not implemented"