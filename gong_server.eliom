[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

[%%server
  open Batteries
  open Lwt
]

let rec connect_arduino () =
  Lwt.catch
    (fun () -> Lwt.return (
      let arduino = BatFile.open_out "/dev/ttyACM0" in
      print_endline "Connected to arduino!";
      arduino))
    (fun _ ->
      print_endline "Could not connect to arduino, retrying...";
      Lwt_unix.sleep 5.0 >>= connect_arduino)

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

let gong arduino =
  try
    print_endline "Processing gong request";
    BatIO.write arduino 'g';
    BatIO.flush arduino;
    ()
  with
  | _ -> ()

let request_gong arduino =
   Lwt_mvar.put arduino ()

let serve arduino =
  Server_app.register
    ~service:gong_service
    (fun () () ->
      request_gong arduino >|=
      (fun _ ->
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata "gong!"];
           ]))))

let rec process_requests mailbox arduino =
   Lwt_mvar.take mailbox >|=
    (fun _ -> gong arduino) >>=
    (fun _ -> process_requests mailbox arduino)

let () =
  let mailbox = Lwt_mvar.create_empty () in
  Lwt.async (fun () -> connect_arduino () >>= process_requests mailbox);
  serve mailbox

