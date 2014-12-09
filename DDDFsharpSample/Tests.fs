module Tests
// open Xunit
open NUnit.Framework
open FsUnit
open Swensen.Unquote
open DDDFsharpSample 
open Types 
open Akka.FSharp

[<Test>]
let test_1 () =
  let l = [1..10]
  test <@ 1 = 1 @> 
  test <@ l |> List.sum = (l |> List.fold (+) 0) @> 

[<Test>]
let test_types () =
  let s (email: Email) =  email

  let a = s "yahoo@som.com"
  test <@ a = "yahoo@som.com" @>

[<Test>]
let test_empty_user () =
  test <@ User.empty = {email = ""; phash = [||]; salt = [||]} @>

[<Test>]
let test_commands () =
  let u = RegisterUser ("john", "111")
  let name, p = match u with
                  | RegisterUser (name, p) -> name, p

  test <@ name = "john" @>

//[<Test>]
//let ``test how errors are handled`` () =
//  let h1 = function
//  | "" -> Error ("I don't like empty strings")
//  | s -> Ok (s)
//
//  let h2 s = sprintf "I've got '%A'!!" s
//
//  let r = "foo" |> h1 >=> h2
//
//  test <@ r1 = 8 @>

[<Test>]
let ``show real world akka`` () =
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
