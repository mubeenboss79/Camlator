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

let rec handle_outgoing_msg oc msg () =
  Lwt_io.write_line oc msg >>= return

let rec handle_incoming_msg ic oc messages textbox pref_lang () =
  Lwt_log_core.info "Listening for incoming messages..." >>= fun () ->
  Lwt_io.read_line_opt ic >>= fun msg_opt ->
  match msg_opt with
  | None     -> Lwt_log.info "Connection closed" >>= return
  | Some msg -> 
      Httpclient.translate_msg msg !pref_lang >>= fun tr_msg ->
      let str_utf8 = Glib.Convert.locale_to_utf8 tr_msg in
      messages := !messages ^ "\n" ^ str_utf8;
      let n_buff = GText.buffer ~text:(!messages) () in
      textbox#set_buffer n_buff;
      Lwt_log.info ("Got message: " ^ msg) >>=
      handle_incoming_msg ic oc messages textbox pref_lang

let create_channels () =
  let open Lwt_unix in
  let sockfd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  let _ = Lwt_unix.connect sockfd @@ ADDR_INET(Unix.inet_addr_any, 9000) in
  let ic = Lwt_io.of_fd Lwt_io.Input sockfd in
  let oc = Lwt_io.of_fd Lwt_io.Output sockfd in
  (handle_incoming_msg ic oc), (handle_outgoing_msg oc)

(* Run client *)
(*
let start_client () =
  let start_inc, start_out = create_channels () in
  let threads = Lwt.join [start_out (); start_inc ()] in
  Lwt_main.run threads;
*)
