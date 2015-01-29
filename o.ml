
#directory "/lib64/ocaml/xml-light/" 
#load "xml-light.cma"
#load "str.cma"
#directory "/lib64/ocaml/lablgtk2/"
#load "lablgtk.cma" 
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

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
  let opened = ref ""
  let sms = ref SS.empty
  (*let test = (File.make_smss (File.parse_xml  !opened)) *)





let init = GMain.Main.init ()

let toBox sms box li=  List.iter 
					(fun (width,height,str) -> 	GMisc.label ~text: (List.assoc str sms) ~width:width  ~height:height ~line_wrap:true ~packing: box#add () ;
  							()) li

  let window = GWindow.window ~title:"Wymyśl nazwę" ~width:1200 ~height:300 ~border_width:10 ()

  let mainbox = GPack.vbox ~packing:window#add ()

  let infobox = GPack.hbox ~packing:mainbox#add () 

  let smsFrame = GBin.frame ~label:"SMS details" ~border_width:10 ~label_xalign:1.0 ~width:600  ~packing:infobox#add ()
  
  let smsBox = GPack.hbox ~packing: smsFrame#add()

  let smsLabels = 
  		let labelsBox = GPack.vbox ~packing:smsBox#add () in
  		List.iter (fun (str,height) -> 	GMisc.label ~text: str ~width:100 ~height:height ~justify:`LEFT ~packing: labelsBox#add () ;
  			()) [("Name",10); ("Date",10);("Seen",10);("R/S(1/2)",10);("Body",0)]

  let selected_sms = GPack.vbox ~width:500 ~packing:  smsBox#add ()
  
  let scrolled_window = GBin.scrolled_window 
   	 	~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:mainbox#add ()
  
  let sms_list = GPack.vbox ~packing: scrolled_window#add_with_viewport() 

  let create_line sms = 
  			let event_box = GBin.event_box ~packing:sms_list#add() in 
  				let line = GPack.hbox  ~width:600 ~packing:event_box#add () in
  			toBox sms line  [(30,0,"name");(110,0,"rdate");(30,0,"r");(30,0,"type");(400,0,"body")];
    		event_box#event#add [`BUTTON_PRESS];
  			event_box#event#connect#button_press ~callback: 
  						(fun env -> 
  							List.iter (fun e -> selected_sms#remove e) selected_sms#all_children; 
  							toBox sms selected_sms [(500,15,"name");(500,15,"rdate");(500,15,"r");(500,15,"type");(500,0,"body")];
  					true );
  			event_box#misc#realize ()



let menuFrame= GBin.frame ~label:"Options" ~border_width:10 ~label_xalign:1.0 ~width:600 ~packing:infobox#add ()
let vbox = GPack.vbox ~packing:menuFrame#add ()
let openButton = GButton.button ~label:"Open file" ~packing:vbox#add ()
let filltreButton = GButton.button ~label: "Filltre" ~packing:vbox#add ()
				  	
let typeFrame = GBin.frame ~label:"Filltre options" ~border_width:10 ~label_xalign:1.0 ~packing:vbox#add ()
let optionsBox = GPack.vbox ~packing:typeFrame# add ()  
let recivedButton = GButton.check_button ~label:"Recived" ~packing:optionsBox#add () 
let sendedButton = GButton.check_button ~label:"Sended" ~packing:optionsBox#add ()
let seLab = 	GMisc.label ~text:"Search in body for" ~line_wrap:true ~packing: optionsBox#add () 
let bodyContent = GEdit.entry ~has_frame:true ~text:"Body contains" ~packing:optionsBox#add ()
let contLab = GMisc.label ~text:"Choose contact" ~line_wrap:true ~packing: optionsBox#add ()
let contactCombo = GEdit.combo  ~allow_empty:true ~enable_arrow_keys:true ~value_in_list:true ~packing:optionsBox#add ()


let file_ok_sel filew () =
	opened:= filew#filename ;
	List.iter (fun e -> sms_list#remove e) sms_list#all_children;
	let parsed = (File.parse_xml !opened) in
	sms:=(File.make_smss parsed);
 	SS.iter create_line !sms;
 	contactCombo#set_popdown_strings ((fun (a,b) -> a) (List.split (File.make_friends parsed)))

let file_selection () =
  (* Create a new file selection widget; set default filename *)
  let filew = GWindow.file_selection ~title:"File selection" ~border_width:10 () in
  filew#ok_button#connect#clicked ~callback:(filew#destroy;file_ok_sel filew);
  filew#cancel_button#connect#clicked ~callback:filew#destroy;
  filew#show ()
let filltre () = 
	List.iter (fun e -> sms_list#remove e) sms_list#all_children;
	SS.iter create_line (
	SS.filter (fun e ->
		((recivedButton#active&& ((List.assoc  "type" e)="1")) ||
		(sendedButton#active && ((List.assoc  "type" e)="2"))) &&
		(contains (List.assoc  "name" e) contactCombo#entry#text) &&
		(contains (List.assoc  "body" e) bodyContent#text)
		) !sms);
	()
  (*GMain.Main.main ()*)
let startGui () =
	filltreButton#connect#clicked ~callback:filltre;
	openButton#connect#clicked ~callback:file_selection;
    window#connect#destroy ~callback:GMain.Main.quit;
    window#show ();
    GMain.Main.main ()

end
