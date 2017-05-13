[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

[%%server
  open Batteries
  open Lwt
]

module type GONG_SERVER =
sig
   type t

   val connect : string -> t Lwt.t
   val reconnect: t -> unit Lwt.t

   val request_gong : t -> unit Lwt.t
end

module Arduino : GONG_SERVER =
struct
   type t = { filename: string
            ; device_file: Lwt_io.output Lwt_io.channel ref
            ; mailbox: unit Lwt_mvar.t }

   let rec open_file filename =
     Lwt.catch
       (fun () ->
          Lwt_io.open_file Lwt_io.Output filename >>=
          (fun file ->
            Lwt_io.printl "Connected to arduino!" >|=
            (fun _ -> file)))
       (fun _ ->
         Lwt_io.printl "Could not connect to arduino, retrying..." >>=
         (fun _ -> Lwt_unix.sleep 5.0) >>=
         (fun _ -> open_file filename))

   let gong arduino () : unit Lwt.t =
     Lwt_io.printl "Processing gong request" >>=
     (fun _ -> Lwt_io.write_char !(arduino.device_file) 'g') >>=
     (fun _ -> Lwt_io.flush !(arduino.device_file))

   let rec process_requests arduino () =
      Lwt_mvar.take arduino.mailbox >>=
       (gong arduino) >>=
       (process_requests arduino)

   let connect filename =
      open_file filename >|=
      (fun file ->
        let arduino = { filename = filename;
           device_file = ref file;
           mailbox = Lwt_mvar.create_empty () } in
        Lwt.async (process_requests arduino);
        arduino)

   let request_gong arduino =
      Lwt_mvar.put arduino.mailbox ()

   let reconnect arduino =
      let old_device = !(arduino.device_file) in
      Lwt_io.printl "Reconnecting to Arduino..." >>=
      (fun _ -> open_file arduino.filename) >|=
      (fun file -> arduino.device_file := file) >>=
      (fun _ -> Lwt_io.close old_device)

end

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

let serve arduino =
  Server_app.register
    ~service:gong_service
    (fun () () ->
      Arduino.request_gong arduino >|=
      (fun _ ->
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata "gong!"];
           ]))));
  Eliom_registration.Action.register
    ~service:reconnect_service
    (fun () () -> Arduino.reconnect arduino)

let () =
  Arduino.connect "gong.txt" >|= serve |> Lwt_main.run

