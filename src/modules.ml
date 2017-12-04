let require_module s =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "require")
      [|Js.Unsafe.inject (Js.string s)|]

let google_translate_token_ = require_module "google-translate-token"
