
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

	let important li a b = List.fold_left (fun aux (w,f) -> (aux+w*(f a b))) 0 li



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

end



module Gui = struct
  let opened = ref ""
  let sms = ref SS.empty
  (*let test = (File.make_smss (File.parse_xml  !opened)) *)

let toBox sms box li=  List.iter 
					(fun (width,height,str) -> 	GMisc.label ~text: (List.assoc str sms) ~width:width  ~height:height ~line_wrap:true ~packing: box#add () ;
  							()) li



let init = GMain.Main.init ()



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
let buttonBox = GPack.button_box `HORIZONTAL ~border_width:5 ~packing:vbox#add ()
let openButton = GButton.button ~label:"Open file" ~packing:buttonBox#add ()
let filltreButton = GButton.button  ~label: "Filltre" ~packing:buttonBox#add ()
let sortButton = GButton.button  ~label: "Sort" ~packing:buttonBox#add ()
				  	
let filltreFrame = GBin.frame ~label:"Filltre options" ~border_width:10 ~label_xalign:1.0 ~packing:vbox#add ()
let optionsBox = GPack.vbox ~packing:filltreFrame# add ()  
let recivedButton = GButton.check_button ~label:"Recived" ~active:true ~packing:optionsBox#add () 
let sendedButton = GButton.check_button ~label:"Sended" ~active:true  ~packing:optionsBox#add () 	
let bodyContent = 	GMisc.label ~text:"Search in body for" ~line_wrap:true ~packing: optionsBox#add () ;
					GEdit.entry ~has_frame:true  ~packing:optionsBox#add ()
let contactCombo = 	GMisc.label ~text:"Choose contact" ~line_wrap:true ~packing: optionsBox#add () ;
					GEdit.combo  ~allow_empty:true ~enable_arrow_keys:true ~value_in_list:true ~packing:optionsBox#add ()

let sortFrame = GBin.frame ~label:"Sort priorty" ~border_width:10 ~label_xalign:1.0 ~packing:vbox#add ()
let sortBox = GPack.hbox ~packing:sortFrame#add ()
let adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
let spinner1 =  let frame = GBin.frame ~label:"Name" ~border_width:10 ~label_xalign:1.0 ~width:30 ~packing: sortBox#add () 
				and adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
				in
				GEdit.spin_button ~adjustment:adj ~rate:1.0 ~digits:2 ~width:30 ~packing:frame#add ()
let spinner2 =  let frame = GBin.frame ~label:"Body" ~border_width:10 ~label_xalign:1.0 ~width:30 ~packing: sortBox#add () 
				and adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
				in
				GEdit.spin_button ~adjustment:adj ~rate:1.0 ~digits:2 ~width:30 ~packing:frame#add ()

let spinner3 =  let frame = GBin.frame ~label:"Date" ~border_width:10 ~label_xalign:1.0 ~width:30 ~packing: sortBox#add () 
				and adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
				in
				GEdit.spin_button ~adjustment:adj ~rate:1.0 ~digits:2 ~width:30 ~packing:frame#add ()

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

let filtre smss = SS.filter (fun e -> 
		((recivedButton#active&& ((List.assoc  "type" e)="1")) ||
		(sendedButton#active && ((List.assoc  "type" e)="2")))  &&
		(contains (List.assoc  "name" e) contactCombo#entry#text) &&
		(contains (List.assoc  "body" e) bodyContent#text) 
		) smss
let updateList () = 
	List.iter (fun e -> sms_list#remove e) sms_list#all_children;
	SS.iter create_line (filtre !sms);
	()
let sortList () = 
	let priorty = [(spinner1#value_as_int,Sms.cmp_name);(spinner2#value_as_int,Sms.cmp_body);(spinner3#value_as_int,Sms.cmp_date)] in
	List.iter (fun e -> sms_list#remove e) sms_list#all_children;
	List.iter create_line (List.sort (Sms.important priorty) (SS.elements (filtre !sms)));
	()
let startGui () =
	sortButton#connect#clicked ~callback:sortList;
	filltreButton#connect#clicked ~callback:updateList;
	openButton#connect#clicked ~callback:file_selection;
    window#connect#destroy ~callback:GMain.Main.quit;
    window#show ();
    GMain.Main.main ()

end
