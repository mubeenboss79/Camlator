open Printf
open GMain

let (>>=) = Lwt.bind
let messages = ref "Welcome to the CS3110 chatroom!"
let pref_lang = ref "es"
let start_recv, start_send = Chatclient.create_channels ()

let enter_callback entry text =
  let entry_text = entry#text in
  printf "Entry contents: %s\n" entry_text;
  entry#set_text "";
  let str_utf8 = Glib.Convert.locale_to_utf8 entry_text in
  messages := !messages ^ "\n" ^ str_utf8;
  let n_buff = GText.buffer ~text:(!messages) () in
  text#set_buffer n_buff;
  flush stdout;
  start_send ("chat 1 " ^ str_utf8) () |> ignore;
  ()
(*
  Httpclient.translate_msg str_utf8 >>= (fun t_msg ->
  start_send ("chat 1 " ^ t_msg) () >>= Lwt.return) |> ignore;
*)
(*   Chatclient.broadcast_msg t_msg *)

let entry_toggle_editable button entry =
  entry#set_editable button#active

let entry_toggle_visibility button entry = 
   entry#set_visibility button#active 

let setup_threads () =
  (* Initializes GTK. *)
  ignore (GMain.init ());

  (* Install Lwt<->Glib integration. *)
  Lwt_glib.install ();

  (* Thread which is wakeup when the main window is closed. *)
  let waiter, wakener = Lwt.wait () in

  let window=GWindow.window ~title: "GTK Entry" ~width: 500 ~height: 350 () in
  window#connect#destroy ~callback:Main.quit |> ignore;

  let vbox = GPack.vbox ~packing: window#add () in
  let scrollwin = GBin.scrolled_window ~packing:vbox#add () in 
  let text = GText.view ~packing: scrollwin#add () in 
  text#buffer#insert "Welcome to the CS3110 chatroom!";

  (* text#misc#set_size_chars ~width:20 ~height:5 (); *)
  let entry = GEdit.entry ~max_length: 50 ~packing: vbox#add () in
  entry#connect#activate ~callback:(fun () -> enter_callback entry text)
   |> ignore;
  entry#set_text "Hello";
  entry#append_text " world";
  entry#select_region ~start:0 ~stop:entry#text_length;

  let hbox = GPack.hbox ~packing: vbox#add () in

  let check = GButton.check_button ~label: "Editable" ~active: true
      ~packing: hbox#add () in
  check#connect#toggled ~callback:(fun () -> entry_toggle_editable check entry);
  |> ignore;

  let check = GButton.check_button 
    ~label:"Visible" ~active:true ~packing:hbox#add ()
  in
  check#connect#toggled 
    ~callback:(fun () -> entry_toggle_visibility check entry);
    |> ignore;

  let button = GButton.button ~label: "Close" ~packing: vbox#add () in
  button#connect#clicked ~callback:window#destroy |> ignore;
  button#grab_default ();

  (* Quit when the window is closed. *)
  ignore (window#connect#destroy (Lwt.wakeup wakener));

  window#show ();

  (* Wait for it to be closed. *)
  (waiter, start_recv messages text pref_lang)

let () =
  let start_gui, handle_incoming_msg = setup_threads () in
  let threads = Lwt.join [
    start_gui;
    handle_incoming_msg ();
  ] in
  Lwt_main.run threads
