namespace DDDFsharpSample
open System

module Types =
  type Email = string
  type Password = string
  type PasswordHash = byte array
  type Salt = byte array

  type User =
    { email: Email
      phash : PasswordHash
      salt : Salt }
    static member empty = { email = ""; phash = [||]; salt = [||]}

  type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float

  type UserCommand = 
    | RegisterUser of email : Email * Password
    | LoginUser of Email * Password

  type UserEvent =
    | UserRegistered of Email * PasswordHash * Salt * DateTime
    | UserLogged of Email * DateTime

