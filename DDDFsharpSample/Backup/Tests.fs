module Tests
// open Xunit
open NUnit.Framework
open FsUnit
open Swensen.Unquote
open DDDFsharpSample 
open Types 

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