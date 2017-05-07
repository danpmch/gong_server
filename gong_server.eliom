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
    BatIO.write arduino 'g';
    BatIO.flush arduino;
    true
  with
  | _ -> false

let serve arduino =
  Server_app.register
    ~service:gong_service
    (fun () () ->
      let message =
        if gong arduino then "gong!"
        else "could not ring gong" in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata message];
           ])))

let () =
  Lwt_main.run (connect_arduino () >|= serve)

