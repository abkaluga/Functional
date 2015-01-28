
#directory "/lib64/ocaml/xml-light/" 
#load "xml-light.cma"
#load "str.cma"
#directory "/lib64/ocaml/lablgtk2/"
#load "lablgtk.cma" 


module Sms = struct
	type t = (string * string) list

	let cmp_name a b =
		compare (List.assoc "name" a) (List.assoc "name" b)

	let cmp_date a b =
		compare (List.assoc "date" a) (List.assoc "date" b)

	let cmp_r a b =
		compare (List.assoc "r" a) (List.assoc "r" b)

	let cmp_body a b =
		compare (List.assoc "body" a) (List.assoc "body" b)
	let cmp_addr a b = 
		compare (List.assoc "address" a) (List.assoc "address" b)

	let compare (a:t) (b:t) =
		match cmp_date a b with
		| 0 -> cmp_addr a b
		| c -> c
end

module SS = Set.Make(Sms)

module File = struct

	let parse_xml fileName = Xml.parse_file fileName

	let clear_number s =    Str.global_replace (Str.regexp "-")  ""
                        (Str.global_replace (Str.regexp " ") "" s)

	let make_friends parsedFile =
		let friends = []
			in
			Xml.fold (fun aux e -> 
						let name = (Xml.attrib  e "contact_name") in 
						let number = (clear_number ((Xml.attrib  e "address"))) in
						if not (List.mem_assoc name aux) then 
							(name,[number])::aux
                        else 
							if not (List.mem number (List.assoc name aux)) then
								(name,(number::(List.assoc name aux)))::(List.remove_assoc name aux)
                        	else 
                        		aux
            ) friends parsedFile

	let make_smss parsedFile = 
		let smss =  SS.empty in
			Xml.fold (fun aux e ->
				SS.add 
				([("name",(Xml.attrib  e "contact_name"));
				("body",(Xml.attrib  e "body"));
				("rdate",(Xml.attrib  e "readable_date"));
				("date",(Xml.attrib  e "date"));
				("address",(clear_number (Xml.attrib e "address")));
				("r",(Xml.attrib  e "read"))]) smss ) smss (parsedFile)
	


	let default = [(-10,Sms.cmp_name); (5,Sms.cmp_date)]

	let impor li a b = 
		let rec inner aux li' = 
		match li' with
		| [] -> aux
		| (w,f)::t -> inner (aux+w*(f a b)) t 
	in inner 0 li



end



module Gui = struct
  let opened = ref "test.xml"
  let test = SS.min_elt (File.make_smss (File.parse_xml  !opened))


let init = GMain.Main.init ()


(* Get the selected filename and print it to the console *)
let file_ok_sel filew () =
  print_endline filew#filename;
  flush stdout

let toSelect ali = 
	"Name:\t\t"^(List.assoc "name" ali)
	^"\nDate:\t\t"^(List.assoc "rdate" ali)
	^"\nSeen:\t\t"^(List.assoc "r" ali)
	^"\nBody:\t\t"^(List.assoc "body" ali)
let toBox ali =
	(List.assoc "name" ali)
	^"\t"^(List.assoc "rdate" ali)
	^"\t"^(List.assoc "r" ali)
	^"\t"^(List.assoc "body" ali)

let file_selection () =
  (* Create a new file selection widget; set default filename *)
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
    ~filename:"penguin.png" () in
  filew#ok_button#connect#clicked ~callback:(file_ok_sel filew);
  filew#cancel_button#connect#clicked ~callback:filew#destroy;
  filew#show ();
  GMain.Main.main ()
  

  let window = GWindow.window ~title:"Wymyśl nazwę" ~width:300 ~height:300 ~border_width:10 ()

  let mainbox = GPack.vbox ~packing:window#add ()
  let frame = GBin.frame ~label:"SMS details" ~label_xalign:1.0  ~packing:mainbox#add ()
  let smslabel = GMisc.label ~text:"sometest " ~packing:frame#add ()

  let event_box = GBin.event_box ~packing:mainbox#add ()
  let label = GMisc.label ~text: (toBox test) ~line_wrap:true ~justify:`FILL
    ~packing:event_box#add () 

  let main () =
 (* 	button#connect#clicked ~callback: (fun () -> smslabel#set_text (update test)); *)
 	event_box#event#add [`BUTTON_PRESS];
  	event_box#event#connect#button_press ~callback: (fun env -> smslabel#set_text (toSelect test);true );
  	event_box#misc#realize ();
  	Gdk.Window.set_cursor event_box#misc#window (Gdk.Cursor.create `HAND1);
    window#connect#destroy ~callback:GMain.Main.quit;
    window#show ();
    GMain.Main.main ()

end