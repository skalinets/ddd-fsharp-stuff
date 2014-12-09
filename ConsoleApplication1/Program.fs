﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open DDDFsharpSample.Types
open DDDFsharpSample
open Akka.Fsharp

[<EntryPoint>]
let main argv = 
  printfn "%A" argv

  let inline defaultClock () = System.DateTime.Now

  let handleErrors = function
    | Ok _ -> ()
    | Error data -> printfn "User error: %A" data

  let system = Akka.Actor.ActorSystem.Create "system"

  let userHandler =
    handle
      <| defaultClock
      <| flip findInUserStore userStore
      <| flip saveInUserstore userStore
  
  let subscriber = subscribe system <| printfn "User event: %A"
  let userActor = spawn system "users" <| actsAs (userHandler >=> (publish system.EventStream) >> handleErrors)

  userActor <! RegisterUser ("foo", "p")
  userActor <! RegisterUser ("foo", "p")

  0 // return an integer exit code