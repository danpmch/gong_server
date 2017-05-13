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

   val connect : unit -> t Lwt.t
   val reconnect : t -> unit Lwt.t

   val ring : t -> unit Lwt.t
end

module type FILENAME = sig val filename: string end

(**
 * Allows treating a file as a gong. The real
 * gong is accessed through the device file for the
 * arduino, which on my system is usually /dev/ttyACM0
 *)
module FileGong(Filename : FILENAME) : GONG =
struct
   (* the channel is stored in a ref to simplify reconnecting
    * after the arduino is unplugged or the test file is deleted *)
   type t = Lwt_io.output Lwt_io.channel ref

   let open_file () =
     Lwt_io.open_file Lwt_io.Output Filename.filename

   let connect () = 
     open_file () >|= (fun file -> ref file)

   let reconnect gong =
      let old_file = !gong in
      open_file () >|=
      (* I believe there's a race condition here. If two
       * threads are opening a new file simultaneously
       * then whichever one finishes last will throw away
       * the first thread's file without closing it.
       *
       * Should either use a mutex or figure out if
       * Ocaml/Lwt has a better technique.
       * *)
      (fun file -> gong := file) >>=
      (fun _ -> Lwt_io.close old_file)

   let ring gong : unit Lwt.t =
     Lwt_io.write_char !gong 'g' >>=
     (fun _ -> Lwt_io.flush !gong)

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

   type t = { gong: Gong.t
            ; mailbox: request Lwt_mvar.t }

   let rec wait_for_connection connect_fn () =
     Lwt.catch
       (fun () ->
          connect_fn () >>=
          (fun gong ->
            Lwt_io.printl "Connected to gong!" >|=
            (fun _ -> gong)))
       (fun _ ->
         Lwt_io.printl "Could not connect to gong, retrying..." >>=
         (fun _ -> Lwt_unix.sleep 5.0) >>=
         wait_for_connection connect_fn)

   let gong server : unit Lwt.t =
     Lwt_io.printl "Processing gong request" >>=
     (fun _ -> Gong.ring server.gong)

     (* This function is responsible for pulling
      * requests out of the mailbox and actually
      * ringing the gong. It is intended to be run in
      * a separate asynchronous thread *)
   let rec process_requests server () =
      Lwt_mvar.take server.mailbox >>=
       (function Ring -> gong server
               | Reconnect ->
                  Lwt_io.printl "Reconnecting to gong..." >>=
                  (wait_for_connection (fun () -> Gong.reconnect server.gong))) >>=
       (process_requests server)

   let create () : (unit -> unit Lwt.t) * request Lwt_mvar.t  =
      let mailbox = Lwt_mvar.create_empty () in
      let server_task () =
         wait_for_connection Gong.connect () >|=
         (fun gong ->
           let server = { gong = gong;
              mailbox = mailbox } in
           (* launch the request handling thread *)
           Lwt.async (process_requests server)) in
      (server_task, mailbox)

   (* this is the function called in response to 
    * http requests to the gong endpoint *)
   let request_gong server =
      Lwt_mvar.put server.mailbox Ring

   let reconnect server : unit Lwt.t =
      Lwt_mvar.put server.mailbox Reconnect

end

module ArduinoGong = FileGong(struct let filename = "/dev/ttyACM0" end)
module TestGong = FileGong(struct let filename = "gong.txt" end)
module AudibleTestGong = WithTerminalBell(TestGong)
module Server = GongServer(ArduinoGong)

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
    ~service:reconnect_service
    (fun () () -> Lwt_mvar.put mailbox Server.Reconnect)

let () =
   let gong_server, mailbox = Server.create () in
   Lwt.async gong_server;
   serve mailbox

