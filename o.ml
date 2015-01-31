(*
#directory "/lib64/ocaml/xml-light/" 
#load "xml-light.cma"
#load "str.cma"
#directory "/lib64/ocaml/lablgtk2/"
#load "lablgtk.cma" *)
open Printf


(*
	Funkcja pomocnicza. Sprawdza czy jeden string zawiera się w drugim.
*)
let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false
(*
	Moduł do reprezentacji sms. Dodaje do modułu xml funkcje pozwalające porównywać.
*)

module Sms = struct
	type t = Xml.xml

	let cmp_name a b =
		compare (Xml.attrib a "contact_name") (Xml.attrib b "contact_name")

	let cmp_date a b =
		compare (Xml.attrib a "date") (Xml.attrib b "date") 

	let cmp_r a b =
		compare (Xml.attrib  a "read") (Xml.attrib  b "read")

	let cmp_body a b =
		let parsedA = (Xml.attrib  a "body") and parsedB = (Xml.attrib  b "body") in 
		 match compare (String.length parsedA) (String.length parsedB) with
		 | 0 -> compare parsedA parsedB
		 | c -> c
	let cmp_addr a b = 
		compare (Xml.attrib  a "address") (Xml.attrib  b "address")

	(* 	
		Zakładam, że nie da się wysłać dwóch różnych smsów do tej samej osoby w tym samym czasie.
		Funkcja używana tylko przez moduł Set do porównywania obiektów.	
	*)

	let compare (a:'a) (b:'a) =
		match (cmp_date a b) with
		| 0 -> cmp_addr a b
		| c -> c

	(*
		Właściwa funkcja porównójąca. Bierzę listę krotek funkcji porównująych z ich wagami.
		Porównuje i sumuje wyniki.
	*)
	let important li a b = List.fold_left (fun aux (w,f) -> (aux+w*(f a b))) 0 li

end

module SS = Set.Make(Sms)
module StS = Set.Make(String)


module File = struct
	(* Wczytanie pliku *)
	let parse_xml fileName = 
		try Xml.parse_file fileName
		with  	Xml.Error e-> prerr_endline (Xml.error_msg ((fun (a,_) -> a) e));
				Xml.Element("smss",[("count","0")],[])

	(* Stworzenie zbiru kontaktów *)
	let make_friends parsedFile =
			Xml.fold (fun aux e -> 
					StS.add (Xml.attrib e "contact_name") aux 
            ) StS.empty parsedFile
	(* Stworzenie zbiru sms *)
	let make_smss parsedFile = 
			Xml.fold (fun aux e  -> 
				if ((Xml.tag e) = "sms") then SS.add e aux else aux) SS.empty (parsedFile)

end

module type GUI=
	sig 
    		val startGui : unit -> unit	
	end


module Gui : GUI =
struct
	(* Referencje do zbiru sms i kontatków *)
	let sms = ref SS.empty
	let contacts = ref StS.empty
	(* Funkcja pomocnicza. Osadza sms w zadanym boxie. Li reprezentuje listę atrybutów sms do osadzenia. lab decyduje czy etykiety mają być osadzone w ramkę *)
	let toBox sms box li (lab:bool) =  List.iter 
					(fun (width,height,str) -> 	
						if lab then 
							let frame = GBin.frame ~label:str ~border_width:10 ~label_xalign:1.0 ~width:width ~height:height ~packing:box#add () in
								GMisc.label ~text: (Xml.attrib sms str) ~width:width  ~height:height ~line_wrap:true ~packing: frame#add ()
						else 						
							GMisc.label ~text: (Xml.attrib sms str) ~width:width  ~height:height ~line_wrap:true ~packing: box#add ()
							; ()) li


	(*Bez tego dzieją się złe rzeczy*)
	let init = GMain.Main.init ()



	let window = GWindow.window ~title:"Wymyśl nazwę" ~width:1200 ~height:300 ~border_width:10 ()
	
	let mainbox = GPack.vbox ~packing:window#add ()
	let infobox = GPack.hbox ~packing:mainbox#add () 
	let smsBox = let smsFrame =GBin.frame ~label:"SMS details" ~border_width:10 ~label_xalign:1.0 ~width:600  ~packing:infobox#add () in
					GPack.vbox ~packing: smsFrame#add()  
	let scrolled_window = GBin.scrolled_window 
		~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:mainbox#add ()
  
	let sms_list = GPack.vbox ~packing: scrolled_window#add_with_viewport() 

	let create_line sms = 
			let event_box = GBin.event_box ~packing:sms_list#add() in 
				let frame = GBin.frame ~packing:event_box#add () in
					let line = GPack.hbox  ~width:600  ~show:true ~packing:frame#add () in
						toBox sms line  [(30,0,"contact_name");(110,0,"readable_date");(30,0,"read");(30,0,"type");(400,0,"body")] false;
						event_box#event#add [`BUTTON_PRESS];
						event_box#event#connect#button_press ~callback: 
  						(fun env -> 
  							List.iter (fun e -> smsBox#remove e) smsBox#all_children; 
  							toBox sms smsBox [(600,20,"contact_name");(600,20,"readable_date");(600,20,"read");(600,20,"type");(500,0,"body")] true;
  						true );
						event_box#misc#realize ()

	(*Dodawanie masy elementów. W tutorialach tak budowali strukturę. Ale trochę mi się to nie podoba*)
	let menuFrame= GBin.frame ~label:"Options" ~border_width:10 ~label_xalign:1.0 ~width:600 ~packing:infobox#add ()
	let vbox = GPack.vbox ~packing:menuFrame#add ()
	let buttonBox = GPack.button_box `HORIZONTAL ~border_width:5 ~packing:vbox#add ()
	let openButton = GButton.button ~label:"Open new file" ~packing:buttonBox#add ()
	let concatButton = GButton.button ~label:"Concat file" ~packing:buttonBox#add ()
	let saveButton = GButton.button ~label:"Save actual" ~packing:buttonBox#add ()
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


	let sortBox = let sortFrame = GBin.frame ~label:"Sort priorty" ~border_width:10 ~label_xalign:1.0 ~packing:vbox#add () in 
			  GPack.hbox ~packing:sortFrame#add ()
	(*Dodawanie kontrolek do wyboru priorytetu poszczególnych pól w sms, żeby móc sortować*)
	let spinner1 =  let frame = GBin.frame ~label:"contact_name" ~border_width:10 ~label_xalign:1.0 ~width:30 ~packing: sortBox#add () 
				and adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
				in
				GEdit.spin_button ~adjustment:adj ~rate:1.0 ~digits:2 ~width:30 ~packing:frame#add ()
	let spinner2 =  let frame = GBin.frame ~label:"Body" ~border_width:10 ~label_xalign:1.0 ~width:30 ~packing: sortBox#add () 
				and adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
				in
				GEdit.spin_button ~adjustment:adj ~rate:1.0 ~digits:2 ~width:30 ~packing:frame#add ()

	let spinner3 =  let frame = GBin.frame ~label:"readable_date" ~border_width:10 ~label_xalign:1.0 ~width:30 ~packing: sortBox#add () 
				and adj = GData.adjustment ~value:1.0 ~lower:(-10.0) ~upper:10.0 ~step_incr:1.0 ~page_incr:5.0 ~page_size:0.0 ()
				in
				GEdit.spin_button ~adjustment:adj ~rate:1.0 ~digits:2 ~width:30 ~packing:frame#add ()
	(*Funkcja czyszcząca aktualną listę sms i kontaktów i dodająca listę z nowego pliku*)
	let new_file filew () =
		let parsed = (File.parse_xml filew#filename) in
			filew#destroy ();
			List.iter (fun e -> sms_list#remove e ; e#destroy ()) sms_list#all_children;
			sms:=(File.make_smss parsed);
			contacts:=(File.make_friends parsed);
 			SS.iter create_line !sms;
 			contactCombo#set_popdown_strings (StS.elements !contacts)
 	(* Dołączenie sms i kontatków z innego pliku*)
	let concat_file filew () =
		let parsed = (File.parse_xml filew#filename) in
			let newsms = (File.make_smss parsed) in
				filew#destroy ();
				SS.iter create_line (SS.diff newsms !sms);
				sms:=SS.union !sms newsms ;
				contacts:=StS.union !contacts (File.make_friends parsed); 
 				contactCombo#set_popdown_strings (StS.elements !contacts)


 	(* Okno do wyboru pliku. Ok podpięty do funkcji func*)
	let file_selection func () =
  		let filew = GWindow.file_selection ~title:"Open file" ~border_width:10 () in
  			filew#ok_button#connect#clicked ~callback:(func filew);
  			filew#cancel_button#connect#clicked ~callback:filew#destroy;
  			filew#show ()
  	(*Filtrowanie sms znajdujących się w zbiorze zgodnie z wybranymi kryteriami*)
	let filtre smss = SS.filter (fun e -> 
			((recivedButton#active&& ((Xml.attrib e "type")="1")) ||
			(sendedButton#active && ((Xml.attrib e "type")="2")))  &&
			(contains (Xml.attrib e "contact_name") contactCombo#entry#text) &&
			(contains (Xml.attrib e "body") bodyContent#text) 
			) smss
	(*Zapisywanie sms które spełniają kryteria*)
	let save_file filew () =
		let output = open_out filew#filename in
			let toSave = filtre !sms in
				let xml = Xml.Element("smss",[("count",(string_of_int (SS.cardinal toSave)))],SS.elements toSave) in
				fprintf output "<?xml version='1.0' encoding='UTF-8' standalone='yes' ?>\n<?xml-stylesheet type=\"text/xsl\" href=\"sms.xsl\"?>\n%s" (Xml.to_string xml);
				close_out output;
				filew#destroy ()
	(*Umieszczenie w liście sms które przeszły filtrowanie*)
	let updateList () = 
		List.iter (fun e -> sms_list#remove e ; e#destroy ()) sms_list#all_children;
		SS.iter create_line (filtre !sms);
		()
	(*Sortowanie wg kryteriów*)	
	let sortList () = 
		let priorty = [(spinner1#value_as_int,Sms.cmp_name);(spinner2#value_as_int,Sms.cmp_body);(spinner3#value_as_int,Sms.cmp_date)] in
			List.iter (fun e -> sms_list#remove e ; e#destroy ()) sms_list#all_children;
			List.iter create_line (List.sort (Sms.important priorty) (SS.elements (filtre !sms)));
		()

	let startGui () =
		saveButton#connect#clicked ~callback:(file_selection save_file);
		concatButton#connect#clicked ~callback:(file_selection concat_file);
		sortButton#connect#clicked ~callback:sortList;
		filltreButton#connect#clicked ~callback:updateList;
		openButton#connect#clicked ~callback:(file_selection new_file);
    	window#connect#destroy ~callback:GMain.Main.quit;
   		window#show ();
    	GMain.Main.main ()

	end


let s = Gui.startGui()
