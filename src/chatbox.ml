open Printf
open GMain
open GdkKeysyms
let (>>=) = Lwt.bind
let messages = ref "Welcome to the CS3110 chatroom!"
let pref_lang = ref "en"
let user_name = ref "Camlator User"
let start_recv, start_send = Chatclient.create_channels ()
let emojs_list = [
	"ʘ‿ʘ"; "ಠ_ಠ"; 
	"(╯°□°）╯︵ ┻━┻";
	"┬─┬﻿ ノ( ゜-゜ノ)"; 
	"┬─┬⃰͡ (ᵔᵕᵔ͜ )"; 
	"┻━┻ ︵ヽ(`Д´)ﾉ︵﻿ ┻━┻";
	"ლ(｀ー´ლ)";
	"ʕ•ᴥ•ʔ";
	"ʕᵔᴥᵔʔ";
	"(｡◕‿◕｡)";
	"（　ﾟДﾟ）";
	"¯\\_(ツ)_/¯";
	"¯\\(°_o)/¯";
	"(`･ω･´)"; 
	"(╬ ಠ益ಠ)";
	"☜(⌒▽⌒)☞";
	"ε=ε=ε=┌(;*´Д`)ﾉ";
	"ヽ(´▽`)/"; 
	"ヽ(´ー｀)ノ";
	"ᵒᴥᵒ#";
	"V●ᴥ●V";
	"ฅ^•ﻌ•^ฅ";
	"（ ^_^）o自自o（^_^ ）";
	"ಠ‿ಠ";
	"( ͡° ͜ʖ ͡°)";  
	"ಥ_ಥ";
	"ಥ﹏ಥ";
	"٩◔̯◔۶";
	"ᕙ(⇀‸↼‶)ᕗ";
	"ᕦ(ò_óˇ)ᕤ";
	"⊂(◉‿◉)つ";
	"q(❂‿❂)p";
	"⊙﹏⊙";
	"¯\\_(⊙︿⊙)_/¯";
	"°‿‿°";
	"¿ⓧ_ⓧﮌ";
	"(⊙.☉)7";
	"(´･_･`)";
	"щ（ﾟДﾟщ）";
	"٩(͡๏_๏)۶";
	"ఠ_ఠ";
	"ᕕ( ᐛ )ᕗ";
	"(⊙_◎)";
	"ミ●﹏☉ミ";
	"༼∵༽ ༼⍨༽ ༼⍢༽ ༼⍤༽";
	"ヽ༼ ಠ益ಠ ༽ﾉ";
	"t(-_-t)";
	"(ಥ⌣ಥ)";
	"(づ￣ ³￣)づ";
	"(づ｡◕‿‿◕｡)づ";
	"｡ﾟ( ﾟஇ‸இﾟ)ﾟ｡";
	"༼ ༎ຶ ෴ ༎ຶ༽";
	"┌(ㆆ㉨ㆆ)ʃ";
	"눈_눈";
	"( ఠൠఠ )ﾉ";
	"乁( ◔ ౪◔)「      ┑(￣Д ￣)┍";
	"(๑•́ ₃ •̀๑) ";
	"⁽⁽ଘ( ˊᵕˋ )ଓ⁾⁾";
	"◔_◔";
	"♥‿♥";
	"ԅ(≖‿≖ԅ)";
	"( ˘ ³˘)♥ ";
	"( ˇ෴ˇ )";
	"ヾ(-_- )ゞ ";
	"♪♪ ヽ(ˇ∀ˇ )ゞ";
	"ヾ(´〇`)ﾉ♪♪♪";
	"ʕ •́؈•̀ ₎";
	"ლ(•́•́ლ)";
	"{•̃_•̃}";
	"(ᵔᴥᵔ)";
	"(Ծ‸ Ծ)";
	"(•̀ᴗ•́)و ̑̑";
	"[¬º-°]¬";
	"(☞ﾟヮﾟ)☞";
	] 


let languages = [
    "Automatic";
    "Afrikaans";
    "Albanian";
    "Amharic";
    "Arabic";
    "Armenian";
    "Azerbaijani";
    "Basque";
    "Belarusian";
    "Bengali";
    "Bosnian";
    "Bulgarian";
    "Catalan";
    "Cebuano";
    "Chichewa";
    "Chinese Simplified";
    "Chinese Traditional";
    "Corsican";
    "Croatian";
    "Czech";
    "Danish";
    "Dutch";
    "English";
    "Esperanto";
    "Estonian";
    "Filipino";
    "Finnish";
    "French";
    "Frisian";
    "Galician";
    "Georgian";
    "German";
    "Greek";
    "Gujarati";
    "Haitian Creole";
    "Hausa";
    "Hawaiian";
    "Hebrew";
    "Hindi";
    "Hmong";
    "Hungarian";
    "Icelandic";
    "Igbo";
    "Indonesian";
    "Irish";
    "Italian";
    "Japanese";
    "Javanese";
    "Kannada";
    "Kazakh";
    "Khmer";
    "Korean";
    "Kurdish (Kurmanji)";
    "Kyrgyz";
    "Lao";
    "Latin";
    "Latvian";
    "Lithuanian";
    "Luxembourgish";
    "Macedonian";
    "Malagasy";
    "Malay";
    "Malayalam";
    "Maltese";
    "Maori";
    "Marathi";
    "Mongolian";
    "Myanmar (Burmese)";
    "Nepali";
    "Norwegian";
    "Pashto";
    "Persian";
    "Polish";
    "Portuguese";
    "Punjabi";
    "Romanian";
    "Russian";
    "Samoan";
    "Scots Gaelic";
    "Serbian";
    "Sesotho";
    "Shona";
    "Sindhi";
    "Sinhala";
    "Slovak";
    "Slovenian";
    "Somali";
    "Spanish";
    "Sundanese";
    "Swahili";
    "Swedish";
    "Tajik";
    "Tamil";
    "Telugu";
    "Thai";
    "Turkish";
    "Ukrainian";
    "Urdu";
    "Uzbek";
    "Vietnamese";
    "Welsh";
    "Xhosa";
    "Yiddish";
    "Yoruba";
    "Zulu";
    ]

(* Changes the preferred language once an option is clicked in the 
 * language combo box *)

let changed_and_get_active (combo : #GEdit.combo_box) column cb =
  combo#connect#changed
    (fun () ->
      match combo#active_iter with
      | None -> ()
      | Some row ->  
    let data = combo#model#get ~row ~column in

    let lang = 
      begin
        match data with 
        | "Automatic" -> "auto"; 
        | "Afrikaans" -> "af";
        | "Albanian" -> "sq";
        | "Amharic" -> "am";
        | "Arabic" -> "ar";
        | "Armenian" -> "hy";
        | "Azerbaijani" -> "az";
        | "Basque" -> "eu";
        | "Belarusian" -> "be";
        | "Bengali" -> "bn";
        | "Bosnian" -> "bs";
        | "Bulgarian" -> "bg";
        | "Catalan" -> "ca";
        | "Cebuano" -> "ceb";
        | "Chichewa" -> "ny";
        | "Chinese Simplified" -> "zh-cn";
        | "Chinese Traditional" -> "zh-tw";
        | "Corsican" -> "co";
        | "Croatian" -> "hr";
        | "Czech" -> "cs";
        | "Danish" -> "da";
        | "Dutch" -> "dl";
        | "English" -> "en";
        | "Esperanto" -> "eo";
        | "Estonian" -> "et";
        | "Filipino" -> "tl";
        | "Finnish" -> "fi";
        | "French" -> "fr";
        | "Frisian" -> "fy";
        | "Galician" -> "gl";
        | "Georgian" -> "ka";
        | "German" -> "de";
        | "Greek" -> "el";
        | "Gujarati" -> "gu";
        | "Haitian Creole" -> "ht";
        | "Hausa" -> "ha";
        | "Hawaiian" -> "haw";
        | "Hebrew" -> "iw";
        | "Hindi" -> "hi";
        | "Hmong" -> "hmn";
        | "Hungarian" -> "hu";
        | "Icelandic" -> "is";
        | "Igbo" -> "ig";
        | "Indonesian" -> "id";
        | "Irish" -> "ga";
        | "Italian" -> "it";
        | "Japanese" -> "ja";
        | "Javanese" -> "jw";
        | "Kannada" -> "kn";
        | "Kazakh" -> "kk";
        | "Khmer" -> "km";
        | "Korean" -> "ko";
        | "Kurdish (Kurmanji)" -> "ku";
        | "Kyrgyz" -> "ky";
        | "Lao" -> "lo";
        | "Latin" -> "la";
        | "Latvian" -> "lv";
        | "Lithuanian" -> "lt";
        | "Luxembourgish" -> "lb";
        | "Macedonian" -> "mk";
        | "Malagasy" -> "mg";
        | "Malay" -> "ms";
        | "Malayalam" -> "ml";
        | "Maltese" -> "mt";
        | "Maori" -> "mi";
        | "Marathi" -> "mr";
        | "Mongolian" -> "mn";
        | "Myanmar (Burmese)" -> "my";
        | "Nepali" -> "ne";
        | "Norwegian" -> "no";
        | "Pashto" -> "ps";
        | "Persian" -> "fa";
        | "Polish" -> "pl";
        | "Portuguese" -> "pt";
        | "Punjabi" -> "ma";
        | "Romanian" -> "ro";
        | "Russian" -> "ru";
        | "Samoan" -> "sm";
        | "Scots Gaelic" -> "gd";
        | "Serbian" -> "sr";
        | "Sesotho" -> "st";
        | "Shona" -> "sn";
        | "Sindhi" -> "sd";
        | "Sinhala" -> "si";
        | "Slovak" -> "sk";
        | "Slovenian" -> "sl";
        | "Somali" -> "so";
        | "Spanish" -> "es";
        | "Sundanese" -> "su";
        | "Swahili" -> "sw";
        | "Swedish" -> "sv";
        | "Tajik" -> "tg";
        | "Tamil" -> "ta";
        | "Telugu" -> "te";
        | "Thai" -> "th";
        | "Turkish" -> "tr";
        | "Ukrainian" -> "uk";
        | "Urdu" -> "ur";
        | "Uzbek" -> "uz";
        | "Vietnamese" -> "vi";
        | "Welsh" -> "cy";
        | "Xhosa" -> "xh";
        | "Yiddish" -> "yi";
        | "Yoruba" -> "yo";
        | "Zulu" -> "zu";
        | _ -> "auto";
      end
    in
    pref_lang := lang;

    cb !pref_lang)

(*Setups the combobox for the Language selection drop down *)
let setup_combobox_text packing =  
  let tmp = GBin.frame ~label:"Language" ~packing () in
  let box = GPack.vbox ~border_width:8 ~packing:tmp#add () in
  let (combo, (_, column)) = 
    GEdit.combo_box_text ~packing:box#pack 
      ~strings: languages () in
  combo#set_active 0;
  ignore (changed_and_get_active combo column prerr_endline);
  ()

(* Sends message to textbox after enter is pressed in the entry box *)
let enter_callback entry text =
  let entry_text = entry#text in
  printf "Entry contents: %s\n" entry_text;
  entry#set_text "";
  let str_utf8 = Glib.Convert.locale_to_utf8 entry_text in
  messages := !messages ^ "\n\n" ^ !user_name ^ ": " ^ str_utf8;
  let n_buff = GText.buffer ~text:(!messages) () in
  text#set_buffer n_buff;
  flush stdout;
  start_send ("chat 1 " ^ !user_name ^ ": " ^str_utf8) () |> ignore;
  ()

(* Changes the person's username in the messenger app *) 
let name_callback entry text =
  let entry_text = entry#text in
  printf "Name changed to: %s\n" entry_text;
  entry#set_text entry#text;
  let name_change = entry#text in 
  let str_utf8 = Glib.Convert.locale_to_utf8 (name_change) in 
  messages := !messages ^ "\n\n" ^ !user_name ^ " changed their name to: " ^ str_utf8;
  user_name := str_utf8;
  let n_buff = GText.buffer ~text:(!messages) () in
  text#set_buffer n_buff;
  flush stdout;
  start_send ("chat 1 " ^ !user_name ^ " changed their name to: " ^ str_utf8) () |> ignore;
  ()

let setup_threads () =
  (* Initializes GTK. *)
  ignore (GMain.init ());

  (* Install Lwt<->Glib integration. *)
  Lwt_glib.install ();

  (* Thread which is wakeup when the main window is closed. *)
  let waiter, wakener = Lwt.wait () in

  let window=GWindow.window ~title: "Camalator Messenger" ~width: 680 ~height: 500 
            ~allow_grow:true ~allow_shrink:true () in
  window#connect#destroy ~callback:Main.quit |> ignore;

  let vbox = GPack.vbox ~packing: window#add () in

  (*Menu bar*)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in 
  let file_menu = factory#add_submenu "Menu" in
  ignore(accel_group); 
  ignore(file_menu); 

  (*Text box for messages*)
  let scrollwin = GBin.scrolled_window ~width: 400 ~height: 400  ~packing:vbox#add () in
  let text = GText.view ~packing: scrollwin#add () in 
  text#buffer#insert "Welcome to the CS3110 chatroom!"; 

  (* text#misc#set_size_chars ~width:20 ~height:5 (); *)
  let entry = GEdit.entry ~max_length: 5000 ~packing: vbox#add () in
  entry#connect#activate ~callback:(fun () -> enter_callback entry text)
   |> ignore;
  entry#set_text "Send";
  entry#append_text " Message Here!";
  entry#select_region ~start:0 ~stop:entry#text_length;

  let hbox = GPack.hbox ~packing: vbox#add () in


(* Handles the printing of the emojis to the entry box *)
let changed_and_get_active_emojis (combo : #GEdit.combo_box) column cb entry =
  combo#connect#changed
    (fun () ->
      match combo#active_iter with
      | None -> ()
      | Some row ->  
    let emoji = combo#model#get ~row ~column in
   	let curr_text = entry#text in 
   	entry#set_text curr_text;
   	entry#append_text " ";
   	entry#append_text emoji;
    cb emoji)
in

(*The setup for the combobox for emojis *)
let setup_combobox_emojis packing =  
  let tmp = GBin.frame ~label:"Emojis" ~packing () in
  let box = GPack.vbox ~border_width:8 ~packing:tmp#add () in
  let (combo, (_, column)) = 
    GEdit.combo_box_text ~packing:box#pack 
      ~strings: emojs_list () in
  combo#set_active 0 ;
  changed_and_get_active_emojis combo column prerr_endline entry;
  in 

  setup_combobox_text hbox#pack; 

  (*Make the User name's frames and text box *)
  let namebox = GPack.vbox ~packing: (hbox#pack ~padding: 30) () in
  let tmp = GBin.frame ~label:"Name" ~packing: namebox#add () in
  let box = GPack.vbox ~border_width:8 ~packing:tmp#add () in
  let name = GEdit.entry ~max_length: 50 ~packing:(box#pack )() in
  name#connect#activate ~callback:(fun () -> name_callback name text)
   |> ignore;
  name#set_text "Camlator User";
  entry#select_region ~start:0 ~stop:entry#text_length;

  ignore (setup_combobox_emojis hbox#pack);
  let button = GButton.button ~label: "Close" ~packing:(hbox#pack ~padding:15) () in
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
