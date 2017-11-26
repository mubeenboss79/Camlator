(** Multi-client server example.

    Clients can increment a shared counter or read its current value.

    Build with: ocamlfind ocamlopt -package lwt,lwt.unix -linkpkg -o counter-server ./counter-server.ml
 *)

open Lwt

(* UID of each client *)
let next_uid = ref 0

module Int = struct
  type t = int

  let compare a b =
    if a < b then -1
    else if a > b then 1
    else 0
end

module ChannelMap = Map.Make(Int)

let out_channels = ref ChannelMap.empty

(**************** Weird logging stuff ****************)
let () =
  Lwt_log_core.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(level)] $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ();

  Lwt_log_core.add_rule "*" Lwt_log_core.Info;

  Lwt_main.run begin
    Lwt_log_core.info "Started server..."
  end
(*   let () = Lwt_log.add_rule "*" Lwt_log.Info *)
(******************************************************)

(* [handle_message msg] takes a [msg] and returns some response as a string
 * based on a custom protocol. *)
let handle_message msg =
  match Str.bounded_split (Str.regexp_string " ") msg 3 with
    | ["chat"; cid; msg] -> 
        ChannelMap.map (fun oc -> Lwt_io.write_line oc msg) !out_channels 
        |> ignore;
        "Message sent."
    | ["parts"] -> 
        ChannelMap.fold (fun k _ acc -> acc ^ (string_of_int k) ^ ", ")
          !out_channels "Participants: "
    | _ ->
        "Unknown command"

(* [handle_connection ic oc] is the thread for handling messages coming in from
 * [ic] - some input channel - and returning some data through [oc] - some
 * output channel. *)
let rec handle_connection ic oc uid () =
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
      match msg with
      | Some msg -> 
          let reply = handle_message msg in
          Lwt_io.write_line oc reply >>= handle_connection ic oc uid
      | None -> 
          out_channels := ChannelMap.remove uid !out_channels;
          Lwt_log.info "Connection closed" >>= return)

(* [accept_connection conn] starts a [handle_connection] thread for [conn],
 * which gets passed whenever a connection has been accepted on the server's
 * socket *)
let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  let uid = !next_uid in
  out_channels := ChannelMap.add uid oc !out_channels;
  next_uid := !next_uid + 1;
  Lwt.on_failure 
    (handle_connection ic oc uid ()) 
    (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New connection" >>= return

(* [create_server sock] accepts connections indefinitely from [sock] *)
let create_server sock =
  let rec serv () =
    Lwt_unix.accept sock >>= accept_connection >>= serv
  in serv

(* [create_socket listen_address port backlog] creates a TCP socket on host
 * [listen_address] and port [port]. [backlog] specifies the max number of
 * pending requests (as in Unix.listen). *)
let create_socket listen_address port backlog =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = bind sock @@ ADDR_INET(listen_address, port) in
  listen sock backlog;
  sock

(* Main Function -- Starts up the server *)
let () =
  let sock = create_socket Unix.inet_addr_loopback 9000 10 in
  let serv = create_server sock in
  Lwt_main.run @@ serv ()

