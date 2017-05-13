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
                | Reconnect
                | Connect of string

   type t = { gong: Gong.t
            ; mailbox: request Lwt_mvar.t }

   let rec wait_for_connection (connect_fn: string -> 'a Lwt.t) filename () =
     Lwt.catch
       (fun () ->
          connect_fn filename >>=
          (fun gong ->
            Lwt_io.printl "Connected to gong!" >|=
            (fun _ -> gong)))
       (fun _ ->
         Lwt_io.printl "Could not connect to gong, retrying..." >>=
         (fun _ -> Lwt_unix.sleep 5.0) >>=
         wait_for_connection connect_fn filename)

   let gong server : unit Lwt.t =
     Lwt_io.printl "Processing gong request" >>=
     (fun _ -> Gong.ring server.gong)

     (* This function is responsible for pulling
      * requests out of the mailbox and actually
      * ringing the gong. It is intended to be run in
      * a separate asynchronous thread *)
   let rec process_requests server =
      Lwt_mvar.take server.mailbox >>=
       (function Ring ->
                  gong server >|=
                  (fun _ -> server)
               | Reconnect ->
                  Lwt_io.printl "Reconnecting to gong..." >>=
                  (wait_for_connection (fun _ -> Gong.reconnect server.gong) "") >|=
                  (fun gong ->
                     { gong = gong; mailbox = server.mailbox })
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

   let reconnect server : unit Lwt.t =
      Lwt_mvar.put server.mailbox Reconnect

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

let reconnect_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["reconnect"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

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
  Eliom_registration.Action.register
    ~service:reconnect_service
    (fun () () -> Lwt_mvar.put mailbox Server.Reconnect)

let () =
   let gong_server, mailbox = Server.create "/dev/ttyACM0" in
   Lwt.async gong_server;
   serve mailbox

