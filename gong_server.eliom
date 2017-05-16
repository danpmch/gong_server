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

   module RequestOrder : BatInterfaces.OrderedType with type t = request =
   struct
      type t = request

      let request_to_int r =
         match r with
         | Connect _ -> 0
         | Reconnect -> 1
         | Ring -> 2

      let compare x y =
         compare (request_to_int x) (request_to_int y)

   end

   module RequestQueue =
   struct
      module RequestHeap = BatHeap.Make(RequestOrder)
      type t = unit Lwt_condition.t * RequestHeap.t ref

      let create_empty () : t =
         Lwt_condition.create (), ref RequestHeap.empty

      let put (condition, heap) (x: request) : unit =
         heap := RequestHeap.insert !heap x;
         Lwt_condition.signal condition ()

      let get (condition, heap) : request Lwt.t =
         let lwt = if !heap = RequestHeap.empty
            then Lwt_condition.wait condition
            else Lwt.return () in
         lwt >|= (fun _ -> 
              let min = RequestHeap.find_min !heap in
              heap := RequestHeap.del_min !heap;
              min
            )

      let clear (condition, heap) =
         heap := RequestHeap.empty

   end


   type t = { gong: Gong.t option
            ; mailbox: RequestQueue.t }

   let rec wait_for_connection retries (connect_fn: string -> 'a Lwt.t) filename () : Gong.t option Lwt.t =
     Lwt.catch
       (fun () ->
          if retries > 0 then
             connect_fn filename >>=
             (fun gong ->
               Lwt_io.printl "Connected to gong!" >|=
               (fun _ -> Some gong))
          else
             Lwt_io.printl "Connection attempts exhausted, giving up" >|=
             (fun _ -> None))
       (fun _ ->
         Lwt_io.printl "Could not connect to gong, retrying..." >>=
         (fun _ -> Lwt_unix.sleep 5.0) >>=
         wait_for_connection (retries - 1) connect_fn filename)

   let connect = wait_for_connection 6 Gong.connect
   let reconnect gong = wait_for_connection 6 (fun _ -> Gong.reconnect gong) ""

   let handle_ring server : t Lwt.t =
      let ring_result = match server.gong with
        | Some gong ->
           Lwt_io.printl "Processing gong request" >>=
           (fun _ -> Gong.ring gong)
        | None ->
           Lwt_io.printl "No gong available for Ring request" in
      ring_result >|= (fun _ -> server)

   let handle_reconnect server =
      match server.gong with
      | Some gong ->
          Lwt_io.printl "Reconnecting to gong..." >>=
          reconnect gong >|=
          (fun gong ->
            { gong = gong; mailbox = server.mailbox })
      | None ->
          Lwt_io.printl "No gong available for Reconnect request" >|=
          (fun _ -> server)

   let handle_connect server filename =
      Lwt_io.printl "Connecting to new gong..." >>=
      connect filename >|=
      (fun gong ->
        if Option.is_some gong then RequestQueue.clear server.mailbox
        else ();
        { gong = gong; mailbox = server.mailbox })

     (* This function is responsible for pulling
      * requests out of the mailbox and actually
      * ringing the gong. It is intended to be run in
      * a separate asynchronous thread *)
   let rec process_requests server =
      RequestQueue.get server.mailbox >>=
       (function Ring -> handle_ring server
               | Reconnect -> handle_reconnect server
               | Connect filename -> handle_connect server filename) >>=
       process_requests

   let create filename : (unit -> unit Lwt.t) * RequestQueue.t  =
      let mailbox = RequestQueue.create_empty () in
      let server_task () =
         connect filename () >|=
         (fun gong ->
           let server = { gong = gong;
              mailbox = mailbox } in
           (* launch the request handling thread *)
           Lwt.async (fun () -> process_requests server)) in
      (server_task, mailbox)

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
       Server.RequestQueue.put mailbox Server.Ring;
       Lwt.return
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata "gong!"];
           ])));
  Eliom_registration.Action.register
    ~service:connect_service
    (fun filename () -> Lwt.return
       (Server.RequestQueue.put mailbox (Server.Connect filename)));
  Eliom_registration.Action.register
    ~service:reconnect_service
    (fun () () -> Lwt.return
       (Server.RequestQueue.put mailbox Server.Reconnect))

let () =
   let gong_server, mailbox = Server.create "/dev/ttyACM0" in
   Lwt.async gong_server;
   serve mailbox

