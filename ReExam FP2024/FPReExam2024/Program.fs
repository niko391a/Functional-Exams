﻿open System
open Exam2024

let testQ1 () =
    let rect = Rectangle(2., 3.)
    let circ = Circle 2.
    let trig = Triangle(2., 3.)
    
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"
    
    printfn "%A" (area rect)
    printfn "%A" (circumference rect)
    

    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    ()

let testQ3 () =
    printfn "Testing Question 3"
    // place debug prints for Q3 here
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ1 ()
    0 // return an integer exit code
