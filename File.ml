(*#directory "/lib64/ocaml/xml-light/" 
#load "xml-light.cma"
#load "str.cma" *)
module File = struct
let file = "test.xml"
let xml = Xml.parse_file file
let contacts = Hashtbl.create (int_of_string (Xml.attrib xml "count"))
let sms = ref []
let clear_number s =    Str.global_replace (Str.regexp "-")  ""
                        (Str.global_replace (Str.regexp " ") "" s)

let make_contacts = Xml.iter (fun e -> 
                        let name = (Xml.attrib  e "contact_name") in 
                        let number = (clear_number ((Xml.attrib  e "address"))) in
                        
                        if not (Hashtbl.mem contacts name) then 
                            Hashtbl.add contacts name [number]
                        else 
                            if not (List.mem number (Hashtbl.find contacts name)) then
                            Hashtbl.replace contacts name (number::(Hashtbl.find contacts name))
                    )
                    xml

let make_sms   = 
                Xml.iter (fun e->
                        sms:= ((Xml.attrib  e "contact_name"),
                        (Xml.attrib  e "body"),
                        (Xml.attrib  e "readable_date"),
                        (Xml.attrib  e "date"),
                        (((Xml.attrib  e "read"))!="0"))::(!sms)     
                    )
                    xml
    
end



