type sender = {name : string ; lastname : string}
type recipients = sender list
type date = {year : int ; day : int ; month : int}
type time = {hour : int ; minute : int ; second : int}

type email = {sender : sender ; recipients : recipients ; date : date ; 
time : time ; subject : string ; content : string}

let string_of_sender email = 
	"From: " ^ email.sender.name ^ " " ^ email.sender.lastname ^
	"<" ^ email.sender.name ^ "." ^ email.sender.lastname ^ "@" ^
	String.lowercase_ascii email.sender.name ^ ".com>\n"

let string_of_recipient1 recipient = 
	recipient.name ^ " " ^ recipient.lastname ^
	"<" ^ recipient.name ^ "." ^ recipient.lastname ^ "@" ^
	String.lowercase_ascii recipient.name ^ ".com>, "


let string_of_date_time email = 
	"Date: " ^ (string_of_int email.date.year) ^ "-" ^
	(string_of_int email.date.day) ^ "-" ^ (string_of_int email.date.month) ^
	" " ^ (string_of_int email.time.hour) ^ ":" ^ (string_of_int email.time.minute) ^
	":" ^ (string_of_int email.time.second) ^ "\n"

let string_of_subject email = 
	"Subject: " ^
	email.subject ^ "\n" ^
	email.content

(* let rec string_of_recipients recipients = 
	match recipients with
		| head::body -> begin "helo";
			string_of_recipients body
		end
		| [] -> "\n"
		;;  *)

let string_of_rec recipients = 
	List.iter (string_of_recipient1) recipients 

let string_of_email email = 
	string_of_sender email ^
	"To: " ^
	(* string_of_recipients email.recipients ^  *)
	string_of_date_time email ^ 
	string_of_subject email



let testEmail = {
	sender = {name = "Marko" ; lastname = "Mekjavic"};
	recipients = [{name = "Lan" ; lastname = "Strlic"}; {name = "Miha" ; lastname = "Bercic"}];
	date = {year = 2020 ; day = 28 ; month = 5};
	time = {hour = 22 ; minute = 50 ; second = 21};
	subject = "kesi mesi";
	content = "Milions of lines of well structured code VS One curly boy wihtout a friend"

} 








