(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
(*     let uri = req |> Request.uri |> Uri.to_string in *)
(*     let meth = req |> Request.meth |> Code.string_of_method in *)
(*     let headers = req |> Request.headers |> Header.to_string in *)
    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
      ({|
<!DOCTYPE html>
<html>
<head>

  <title>MINE</title>

  </head>

  <body>
    <h1> My Breakfast Menu</h1>
      <img src="https://c.tribune.com.pk/2016/05/1100455-breaky-1462866009.png"
      width="200" height="200"/>
        
        <ul> 
            <li> 
                  <a href="https://en.wikipedia.org/wiki/Chai"> Chai </a> 
                      </li>
                        
                          <li>
                                <a href="https://en.wikipedia.org/wiki/Paratha">
                                Paratha </a>
                                    </li>
                                      
                                      <li>
                                          <a
                                          href="https://en.wikipedia.org/wiki/Chana_masala">
                                          Chana Masala </a>
                                            </li>
                                             
                                             <p> <h7> To learn more about an
                                             Indian breakfast and how to prepare
                                             it, click <a
                                             href="http://www.vegrecipesofindia.com/recipes/indian-breakfast-recipes/">
                                             here </a>. </h7>
                                              
                                              </p>



                                              </body>




                                              </html>
|}))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
