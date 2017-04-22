[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
]

[%%server
  open Batteries
]

let arduino = BatFile.open_out "/dev/ttyACM0"

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

let () =
  Server_app.register
    ~service:gong_service
    (fun () () ->
      BatIO.write arduino 'g';
      BatIO.flush arduino;
      Lwt.return
        (Eliom_tools.F.html
           ~title:"gong"
           ~css:[["css";"server.css"]]
           Html.F.(body [
             h1 [pcdata "gong!"];
           ])))
