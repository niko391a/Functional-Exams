(* Load this file into the interacive envorinment
   (select all and Alt-Enter in VS).
   
   Cut-and pasting these lines will typically not work unless you provide
   the entire path in the #load command. 

   Some IDEs may still complain about the path, place the full path here if that is the case.
*)

#load "JParsec.fs"
#load "Exam.fs"
open ReExam2023;;
let fp = 10 |> fibProg |> mkBasicProgram
let st = emptyState fp
let st' = update "x" 42 st
let smallProg = [(10u, Let ("x", Num 42)); (20u, End)] |> mkBasicProgram