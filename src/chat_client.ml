open Lwt

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
    Lwt_log_core.info "Started client..."
  end
(*   let () = Lwt_log.add_rule "*" Lwt_log.Info *)
(******************************************************)

let rec handle_outgoing_msg oc () =
  Lwt_io.read_line Lwt_io.stdin >>= fun msg ->
  match msg with
  | exception End_of_file -> return ()
  | user_input -> Lwt_io.write_line oc user_input >>= handle_outgoing_msg oc

let rec handle_incoming_msg ic oc () =
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
      match msg with
      | Some msg -> 
          Lwt_log.info ("Got message: " ^ msg) >>=
          handle_incoming_msg ic oc
      | None -> 
          Lwt_log.info "Connection closed" >>= return)

let create_channels () =
  let open Lwt_unix in
  let sockfd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let _ = Lwt_unix.connect sockfd @@ ADDR_INET(Unix.inet_addr_any, 9000) in
  let ic = Lwt_io.of_fd Lwt_io.Input sockfd in
  let oc = Lwt_io.of_fd Lwt_io.Output sockfd in
  (handle_incoming_msg ic oc), (handle_outgoing_msg oc)

(* Run client *)
let () =
  let start_inc, start_out = create_channels () in
  let threads = Lwt.join [start_out (); start_inc ()] in
  Lwt_main.run threads;
