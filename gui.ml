
#directory "/lib64/ocaml/lablgtk2/"
#load "lablgtk.cma"

let test = ("Klaudia Kozdra", "Na fb masz propozycje hobby. ", "23 lis 2014 15:19:31",
 "1416752371661", true)


let init = GMain.Main.init ()


(* Get the selected filename and print it to the console *)
let file_ok_sel filew () =
  print_endline filew#filename;
  flush stdout

let update = 
    (fun (n,b,rd,_,se)-> "Name: "^n^"\nDate: "^rd^"\nSeen: "^(string_of_bool se) ^"\nBody: "^b)

let file_selection () =
  (* Create a new file selection widget; set default filename *)
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
    ~filename:"penguin.png" () in
  filew#ok_button#connect#clicked ~callback:(file_ok_sel filew);
  filew#cancel_button#connect#clicked ~callback:filew#destroy;
  filew#show ();
  GMain.Main.main ()



  let main () =
  let window = GWindow.window ~title:"Wymyśl nazwę" ~width:300 ~height:300 ~border_width:10 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  let mainbox = GPack.vbox ~packing:window#add () in
    let frame = GBin.frame ~label:"SMS details" ~label_xalign:1.0  ~packing:mainbox#add () in
        let smslabel = GMisc.label ~text:"sometest " ~packing:frame#add () in
        let button = GButton.button ~packing:mainbox#add () in

    button#connect#clicked ~callback: (fun () -> smslabel#set_text (update test));
    window#show ();
    GMain.Main.main ()

(*let _ = Printexc.print main () *)