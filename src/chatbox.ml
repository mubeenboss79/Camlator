(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open Printf
open GMain

let messages = ref ""

let enter_callback entry text =
  printf "Entry contents: %s\n" entry#text;
  flush stdout;
  let s = Glib.Convert.locale_to_utf8 (entry#text) in
  messages := !messages ^ "\n" ^ s;
  let n_buff = GText.buffer ~text:(!messages) () in
  text#set_buffer n_buff;
  entry#set_text ""

let entry_toggle_editable button entry =
  entry#set_editable button#active

let entry_toggle_visibility button entry = 
   entry#set_visibility button#active 
let main () =

  let window =
    GWindow.window ~title: "GTK Entry" ~width: 500 ~height: 350 () in
    window#connect#destroy ~callback:Main.quit;

  let vbox = GPack.vbox ~packing: window#add () in
  let scrollwin = GBin.scrolled_window ~packing:vbox#add () in 
  let text = GText.view ~packing: scrollwin#add () in 
  text#buffer#insert "こんにちは\n" ; 

  (* text#misc#set_size_chars ~width:20 ~height:5 (); *)
  let entry = GEdit.entry ~max_length: 50 ~packing: vbox#add () in
  entry#connect#activate ~callback:(fun () -> enter_callback entry text);
  entry#set_text "Hello";
  entry#append_text " world";
  entry#select_region ~start:0 ~stop:entry#text_length;

  let hbox = GPack.hbox ~packing: vbox#add () in

  let check = GButton.check_button ~label: "Editable" ~active: true
      ~packing: hbox#add () in
  check#connect#toggled
    ~callback:(fun () -> entry_toggle_editable check entry);

  let check =
    GButton.check_button ~label:"Visible" ~active:true ~packing:hbox#add () in
  check#connect#toggled
    ~callback:(fun () -> entry_toggle_visibility check entry);

  let button = GButton.button ~label: "Close" ~packing: vbox#add () in
  button#connect#clicked ~callback:window#destroy;
  button#grab_default ();

  window#show ();

  Main.main ()

let _ = main ()
