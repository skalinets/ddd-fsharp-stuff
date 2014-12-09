namespace DDDFsharpSample
open System
open Akka.Actor
open Akka.FSharp
open System.Collections.Concurrent

module Types =
  type Email = string
  type PasswordHash = byte array
  type Salt = byte array

  type User =
    { email: Email
      phash : PasswordHash
      salt : Salt }
    static member empty = { email = ""; phash = [||]; salt = [||]}

  type UserCommand = 
    | RegisterUser of email : Email * password : string
    | LoginUser of email : Email * password : string 

  type UserEvent =
    | UserRegistered of email : Email * salt : byte array * phash : byte array * timestamp : DateTime
    | UserLogged of email : Email * timestamp : DateTime

  type Result<'ok, 'err> =
    | Ok of 'ok
    | Error of 'err

  let (>=>) f1 f2 arg =
    match f1 arg with
    | Ok data -> f2 data
    | Error e -> Error e

  type UserError =
    | UserAlreadyExists of userEmail : Email
    | UserNotFound of userEmail : Email
    | WrongPassword of userEmail : Email * hashedInput : PasswordHash

  let private saltWith salt (p : string)  =
    let passBytes = System.Text.Encoding.UTF8.GetBytes p
    Array.append passBytes salt

  let sha512 (b : byte array) =
    use sha = System.Security.Cryptography.SHA512.Create ()
    sha.ComputeHash b

  let hashPassword hashFn salt pwd = hashFn (saltWith salt pwd)

  let generatePassword hashFn saltSize password =
    use saltGet = System.Security.Cryptography.RandomNumberGenerator.Create ()
    let salt = Array.zeroCreate saltSize
    saltGet.GetBytes salt
    (salt, hashPassword hashFn salt password)

  let inline defaultGeneratePassword pass = generatePassword sha512 64 pass
  let inline defaultHashPassword salt pass = hashPassword sha512 salt pass

  let handle clock findUser saveUser =
    function
    | RegisterUser (email, password) ->
      match findUser email with
      | Some _ -> Error(UserAlreadyExists email)
      | None -> 
        let salt, phash = defaultGeneratePassword password
        let user = 
          { email = email
            salt = salt
            phash = phash
          }
        saveUser user
        Ok (UserRegistered(email, salt, phash, clock()))

    | LoginUser (email, password) ->
      match findUser email with
      | None -> Error (UserNotFound email)
      | Some user ->
        let computedHash = defaultHashPassword (user.salt) password
        if computedHash = user.phash then
          Ok (UserLogged(user.email, clock()))
        else Error(WrongPassword(user.email, computedHash))

  let actsAs f (mailbox : Actor<'a>) =
    let rec loop' () =
      actor {
        let! msg = mailbox.Receive ()
        f msg
        return! loop' ()
      }
    loop'()

  type InMemoryStore = ConcurrentDictionary<string, User>
  
  let userStore = InMemoryStore()

  let inline flip f x y  = f y x

  let findInUserStore email (store : ConcurrentDictionary<string, User>) =
    match store.TryGetValue email with
      | (false, _) -> None
      | (true, user) -> Some user

  let saveInUserstore user (store : InMemoryStore) =
    store.AddOrUpdate(user.email, user, (fun _ _ -> user)) |> ignore

  let mutable subscriptionNr = 0
  let inline getSubNr () = System.Threading.Interlocked.Increment(&subscriptionNr)

  let subscribe system (f : 'a -> unit) =
    let subId = getSubNr ()
    let subscriber = spawn system ("subscriber-" + (string subId)) <| actsAs f
    system.EventStream.Subscribe (subscriber, typeof<'a>) |> ignore
    subscriber

  let publish (bus : Akka.Event.EventStream) evt =
    bus.Publish evt
    Ok evt





