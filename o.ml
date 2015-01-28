
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

	let compare a b =
		match (cmp_date a b) with
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
			Xml.fold (fun aux e  -> 
				SS.add ([("name",(Xml.attrib  e "contact_name"));
				("body",(Xml.attrib  e "body"));
				("type",(Xml.attrib  e "type"));
				("rdate",(Xml.attrib  e "readable_date"));
				("date",(Xml.attrib  e "date"));
				("address",(clear_number (Xml.attrib e "address")));
				("r",(Xml.attrib  e "read"))]) aux ) SS.empty (parsedFile)
	


	let default = [(-10,Sms.cmp_name); (5,Sms.cmp_date)]

	let important li a b = 
		let rec inner aux li' = 
		match li' with
		| [] -> aux
		| (w,f)::t -> inner (aux+w*(f a b)) t 
	in inner 0 li



end



module Gui = struct
  let opened = ref "test.xml"
  let test = (File.make_smss (File.parse_xml  !opened))


let init = GMain.Main.init ()


(* Get the selected filename and print it to the console *)
let file_ok_sel filew () =
  print_endline filew#filename;
  flush stdout

let toBox sms box =
	let nameLabel =GMisc.label ~text: (List.assoc "name" sms) ~width:100 ~line_wrap:true ~justify:`LEFT
    		~packing:box#add () in 
    let rdateLabel =GMisc.label ~text: (List.assoc "rdate" sms) ~width:150
    		~packing:box#add () in 
    let rLabel =GMisc.label ~text: (List.assoc "r" sms) ~width:10 ~justify:`FILL
    		~packing:box#add () in 
    let wayLabel =GMisc.label ~text: (List.assoc "type" sms) ~width:10 ~justify:`FILL
    		~packing:box#add () in 

    let bodyLabel =GMisc.label ~text: (List.assoc "body" sms) ~width:800 ~line_wrap:true ~justify:`FILL
    		~packing:box#add () in ()

let file_selection () =
  (* Create a new file selection widget; set default filename *)
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10
    ~filename:"penguin.png" () in
  filew#ok_button#connect#clicked ~callback:(file_ok_sel filew);
  filew#cancel_button#connect#clicked ~callback:filew#destroy;
  filew#show ();
  GMain.Main.main ()
  

  let window = GWindow.window ~title:"Wymyśl nazwę" ~width:1070 ~height:300 ~border_width:10 ()

  let mainbox = GPack.vbox ~packing:window#add ()
  let infobox = GPack.hbox ~packing:mainbox#add () 
  let frame = GBin.frame ~label:"SMS details" ~label_xalign:1.0 ~width:300  ~packing:infobox#add ()
  let smsBox = GPack.hbox ~packing: frame#add()
  let smsLabels = 
  		let helpBox = GPack.vbox ~packing:smsBox#add () in
  		List.iter (fun str -> GMisc.label ~text: str ~width:100 ~packing: helpBox#add () ;()) ["Name"; "Date";"Seen";"Recived/Sended(1/2)";"Body"]

  let selected_sms = GPack.vbox ~width:500 ~packing:  smsBox#add ()
  
  let scrolled_window = GBin.scrolled_window ~border_width:10
   	 	~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:mainbox#add ()
  let sms_list = GPack.vbox ~packing: scrolled_window#add_with_viewport() 

  let create_line sms = 
  			let event_box = GBin.event_box ~packing:sms_list#add() in 
  				let line = GPack.hbox ~border_width:5 ~width:1100 ~packing:event_box#add () in
  			toBox sms line;
    		event_box#event#add [`BUTTON_PRESS];
  			event_box#event#connect#button_press ~callback: 
  						(fun env -> List.iter 
  							(fun e -> selected_sms#remove e) 
  								selected_sms#all_children; 
  							List.iter (fun str -> GMisc.label ~text: (List.assoc str sms) ~width:600 ~line_wrap:true ~packing: selected_sms#add () ;()) 
  								["name"; "rdate";"r";"type";"body"];
  					true );
  			event_box#misc#realize ()

  let main () =
  	List.iter (fun e -> sms_list#remove e) sms_list#all_children;
 	SS.iter create_line test;
    window#connect#destroy ~callback:GMain.Main.quit;
    window#show ();
    GMain.Main.main ()

end