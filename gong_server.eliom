[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

[%%server
  open Batteries
  open Lwt
]

module type GONG =
sig
   type t

   val connect : string -> t Lwt.t
   val reconnect : t -> t Lwt.t

   val ring : t -> unit Lwt.t
end

(**
 * Allows treating a file as a gong. The real
 * gong is accessed through the device file for the
 * arduino, which on my system is usually /dev/ttyACM0
 *)
module FileGong : GONG =
struct
   (* the channel is stored in a ref to simplify reconnecting
    * after the arduino is unplugged or the test file is deleted *)
   type t = { filename: string
            ; file: Lwt_io.output Lwt_io.channel }

   let open_file filename =
     Lwt_io.open_file Lwt_io.Output filename

   let connect filename = 
     open_file filename >|=
        (fun file -> { filename = filename; file = file })

   let reconnect gong =
      Lwt_io.close gong.file >>=
      (fun _ -> open_file gong.filename) >|=
      (fun file -> { filename = gong.filename; file = file })

   let ring gong : unit Lwt.t =
     Lwt_io.write_char gong.file 'g' >>=
     (fun _ -> Lwt_io.flush gong.file)

end

(**
 * Given a gong, creates a new gong that behaves the same
 * as the original gong, but also plays the system bell when
 * the ring function is called
 *)
module WithTerminalBell(Gong : GONG) : GONG =
struct
   type t = Gong.t

   let connect = Gong.connect
   let reconnect = Gong.reconnect
   let ring gong =
      Gong.ring gong >>=
      (fun _ -> Lwt_io.print "\007")

end

module GongServer(Gong : GONG) =
struct
   type request = Ring
                | Connect of string

   type t = { gong: Gong.t option
            ; mailbox: request Lwt_mvar.t }

   let rec wait_for_connection (connect_fn: string -> 'a Lwt.t) filename () =
     Lwt.catch
       (fun () ->
          connect_fn filename >>=
          (fun gong ->
            Lwt_io.printl "Connected to gong!" >|=
            (fun _ -> Some gong)))
       (fun _ ->
         Lwt_io.printl "Could not connect to gong, aborting..." >|=
         (fun _ -> None))

   let handle_ring server : unit Lwt.t =
     match server.gong with
     | Some gong ->
         Lwt_io.printl "Processing gong request" >>=
         (fun _ -> Gong.ring gong)
     | None ->
         Lwt_io.printl "No gong available for ring request"

     (* This function is responsible for pulling
      * requests out of the mailbox and actually
      * ringing the gong. It is intended to be run in
      * a separate asynchronous thread *)
   let rec process_requests server =
      Lwt_mvar.take server.mailbox >>=
       (function Ring ->
                  handle_ring server >|=
                  (fun _ -> server)
               | Connect filename ->
                  Lwt_io.printl "Connecting to new gong..." >>=
                  wait_for_connection Gong.connect filename >|=
                  (fun gong ->
                    { gong = gong; mailbox = server.mailbox })) >>=
       process_requests

   let create filename : (unit -> unit Lwt.t) * request Lwt_mvar.t  =
      let mailbox = Lwt_mvar.create_empty () in
      let server_task () =
         wait_for_connection Gong.connect filename () >|=
         (fun gong ->
           let server = { gong = gong;
              mailbox = mailbox } in
           (* launch the request handling thread *)
           Lwt.async (fun () -> process_requests server)) in
      (server_task, mailbox)

   (* this is the function called in response to 
    * http requests to the gong endpoint *)
   let request_gong server =
      Lwt_mvar.put server.mailbox Ring

end

module GongWithTerminalBell = WithTerminalBell(FileGong)
module Server = GongServer(FileGong)

module Server_app =
  Eliom_registration.App (
    struct
      let application_name = "gong"
      let global_data_path = None
    end)

let gong_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["gong"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let connect_service =
   Eliom_service.create
   ~path:(Eliom_service.Path ["connect"])
   ~meth:(Eliom_service.Get (Eliom_parameter.string "filename"))
   ()

let list_service =
   Eliom_service.create
   ~path:(Eliom_service.Path ["list"])
   ~meth:(Eliom_service.Get Eliom_parameter.unit)
   ()

let device_regex = Str.regexp "ttyACM.*"
let is_device filename =
   try
      let _ = Str.search_forward device_regex filename 0 in
      true
   with
   | _ -> false

let full_path arduino = "/dev/" ^ arduino

let list_arduinos () : string list Lwt.t =
   let dir = Lwt_unix.opendir "/dev" in
   let entry_array = dir >>= (fun d -> Lwt_unix.readdir_n d 500) in
   let entries = entry_array >|= (fun es -> Array.enum es |> List.of_enum) in
   let arduinos  = entries >|= (List.filter is_device) in
   arduinos

let serve mailbox =
  Server_app.register
    ~service:gong_service
    (fun () () ->
      Lwt_mvar.put mailbox Server.Ring >|=
      (fun _ ->
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata "gong!"];
           ]))));

  Eliom_registration.Action.register
    ~service:connect_service
    (fun filename () ->
       Lwt_io.printlf "Attempting to connect to %s" filename >>=
       (fun _ -> Lwt_mvar.put mailbox (Server.Connect filename)));

  Server_app.register
     ~service:list_service
     (fun () () ->
       Lwt_io.printl "Listing arduinos" >>=
       list_arduinos >|=
       (fun arduinos ->
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata "Available Arduinos:"];
             ul (List.map
                   (fun arduino ->
                      li [a ~service:connect_service [pcdata arduino] (full_path arduino)] )
                   arduinos)
           ]))
     ))

let () =
   let task = list_arduinos () >|=
      (fun arduinos ->
         let arduino = List.Exceptionless.hd arduinos |> Option.default "ttyACM0" in
         let gong_server, mailbox = Server.create (full_path arduino) in
         Lwt.async gong_server;
         serve mailbox) in
   Lwt_main.run task

