open Lwt.Infix
open Cohttp_lwt_unix

module Wm = struct
  module Rd = Webmachine.Rd

  module UnixClock = struct
    let now () = int_of_float (Unix.gettimeofday ())
  end

  include Webmachine.Make (Cohttp_lwt_unix__Io) (UnixClock)
end

module World = struct
  type t = { id : int; randomNumber : int }
end

(* https://github.com/mirage/ocaml-cohttp/issues/328#issuecomment-222583580 *)
let _ = Lwt_io.set_default_buffer_size 0x10000

let pool =
  let connection_url =
    "postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?connect_timeout=3"
  in
  match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err -> failwith (Caqti_error.show err)

type error = Database_error of string

let or_error m =
  match%lwt m with
  | Ok a -> Ok a |> Lwt.return
  | Error e -> Error (Database_error (Caqti_error.show e)) |> Lwt.return

let select_random =
  Caqti_request.find Caqti_type.int
    Caqti_type.(tup2 int int)
    "SELECT id, randomNumber FROM World WHERE id = ?"

class hello =
  object (self)
    inherit [Cohttp_lwt.Body.t] Wm.resource

    method! allowed_methods rd = Wm.continue [ `GET ] rd

    method content_types_provided rd =
      Wm.continue
        [ ("text/plain", self#to_text); ("application/json", self#to_json) ]
        rd

    method content_types_accepted rd = Wm.continue [] rd

    method private to_text rd =
      let text = "Hello, World!" in
      Wm.continue (`String text) rd

    method private to_json rd =
      let json = Ezjsonm.value (`O [ ("message", `String "Hello, World!") ]) in
      Wm.continue (`String (Ezjsonm.value_to_string ~minify:true json)) rd
  end

class db =
  object (self)
    inherit [Cohttp_lwt.Body.t] Wm.resource

    method! allowed_methods rd = Wm.continue [ `GET ] rd

    method content_types_provided rd =
      Wm.continue [ ("application/json", self#read_db) ] rd

    method content_types_accepted rd = Wm.continue [] rd

    method private read_db rd =
      let read_db' (module C : Caqti_lwt.CONNECTION) =
        C.find select_random (Random.int 10000)
      in
      let ret = Caqti_lwt.Pool.use read_db' pool |> or_error in
      let%lwt id, randomNumber =
        match%lwt ret with
        | Ok (x, y) -> (x, y) |> Lwt.return
        | Error _ -> failwith "whoops"
      in
      let json =
        Ezjsonm.value
          (`O
            [
              ("id", `Float (float_of_int id));
              ("randomNumber", `Float (float_of_int randomNumber));
            ])
      in
      Wm.continue (`String (Ezjsonm.value_to_string ~minify:true json)) rd
  end

class queries =
  object (self)
    inherit [Cohttp_lwt.Body.t] Wm.resource

    method private id rd =
      try
        let _id = int_of_string (Wm.Rd.lookup_path_info_exn "id" rd) in
        (* if _id > 500 then 500 else _id *)
        match _id with
        | x when x < 1 -> 1
        | x when x > 500 -> 500
        | x -> x
      with
      | Failure _ -> 1
      | Not_found -> 1

    method! allowed_methods rd = Wm.continue [ `GET ] rd

    method content_types_provided rd =
      Wm.continue [ ("application/json", self#read_query) ] rd

    method content_types_accepted rd = Wm.continue [] rd

    method private read_query rd =
      let query_ids = List.init (self#id rd) (fun _ -> Random.int 10000) in
      let read_query' x (module C : Caqti_lwt.CONNECTION) =
        C.find select_random x
      in
      let response =
        List.map
          (fun id ->
            let ret = Caqti_lwt.Pool.use (read_query' id) pool |> or_error in
            match%lwt ret with
            | Ok (x, y) -> (x, y) |> Lwt.return
            | Error _ -> failwith "whoops")
          query_ids
      in
      let%lwt interim = Lwt.all response in
      let json =
        Ezjsonm.list
          (fun tup ->
            let id, randomNumber = tup in
            Ezjsonm.value
              (`O
                [
                  ("id", `Float (float_of_int id));
                  ("randomNumber", `Float (float_of_int randomNumber));
                ]))
          interim
      in
      Wm.continue (`String (Ezjsonm.value_to_string ~minify:true json)) rd
  end

let main () =
  let port = 8080 in
  let routes =
    [
      ("/plaintext", fun () -> new hello);
      ("/json", fun () -> new hello);
      ("/db", fun () -> new db);
      ("/queries", fun () -> new queries);
      ("/queries/:id", fun () -> new queries);
    ]
  in
  let callback (_ch, _conn) request body =
    let open Cohttp in
    (Wm.dispatch' routes ~body ~request >|= function
     | None -> (`Not_found, Header.init (), `String "Not found", [])
     | Some result -> result)
    >>= fun (status, headers, body, _) ->
    let headers = Header.add headers "Server" "webmachine" in
    let headers =
      Header.add headers "Date" (Ptime.to_rfc3339 (Ptime_clock.now ()))
    in
    Server.respond ~headers ~body ~status ()
  in

  let config = Server.make ~callback () in
  Server.create ~mode:(`TCP (`Port port)) config >|= fun () ->
  Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port

let () = Lwt_main.run (main ())
