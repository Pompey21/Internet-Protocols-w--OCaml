(* -------------------------------------------------
	DESCRIBING INTERNET PROTOCOLS WITH OCAML TYPES |
   -------------------------------------------------
*)



(*1.*)
(* Defining all the necessery types for the internet protocol to be
	described properly.
*)
type status = { version : string ; code : int }
type transferEncoding = Chunked | Compress | Deflate | Gzip | Identity
type date = {day : string ; number : int ; month : string ; time : string ; timezone : string}
type uri = {scheme : string ; authority : string ; path : string ; query : string ; fragment : string}
type field = 
	| Server of string
	| ContentLength of int
	| ContentType of string
	| TransferEncoding of transferEncoding
	| Date of date
	| Expires of date
	| LastModified of date
	| Location of uri
type response = {status : status ; headers : field list ; body : string}




(*2.*)
(* Defining all the methods that work with the types described above.
*)
let string_of_status s =
	s.version ^ " " ^
	string_of_int s.code ^ " " ^
	(match s.code with
		| 200 -> "OK"
		| 301 -> "Moved Permanently"
		| 100 -> "Continue"
		| 400 -> "Bad Request"
		| 500 -> "Internal Server Error"
		| 103 -> "Checkpoint"
		| _ -> "")

let string_of_date d = 
	(match d.day with
		| "Monday" -> "Mon" | "Tuesday" -> "Tue" | "Wednesday" -> "Wed" | "Thursday" -> "Thu" | "Friday" -> "Fri" | "Saturdat" -> "Sat" 
		| _ -> "Sun") ^ ", " ^
	(string_of_int d.number) ^ " " ^
	(match d.month with
		| "January" -> "Jan" | "February" -> "Feb" | "March" -> "Mar" | _ -> "Apr") ^ " " ^
	d.time ^ " " ^
	d.timezone

let len_str str = String.length str 

let string_of_uri uri = 
	uri.scheme ^ "://" ^
	(if len_str uri.authority > 0 then uri.authority ^ "/" else "") ^
	uri.path ^ "?" ^
	(if len_str uri.query > 0 then uri.query ^ "#" else "") ^
	(if len_str uri.fragment > 0 then uri.fragment else "")

let string_of_transfer_encoding x =
	match x with
		| Chunked -> "chunked"
		| Compress -> "compress"
		| Deflate -> "deflate"
		| Gzip -> "gzip"
		| Identity -> "identity"

let string_of_field f =
	match f with
		| Server str -> "Server: " ^ str
		| ContentLength x -> "Content-Length: " ^ string_of_int x
		| TransferEncoding x -> "Transfer-Encodind: " ^ string_of_transfer_encoding x
		| (Date x | Expires x | LastModified x)  -> string_of_date x
		| Location x -> string_of_uri x
		| ContentType _ -> "hello world"

let string_of_headers hdrs = List.map string_of_field hdrs
let string_of_response r = (string_of_status r.status) ^ "\n" ^ (String.concat "\n" (string_of_headers r.headers))




(*3.*)
(* Defining all the instances that have been used to test the Data Types and Methods.
*)
let mystatus = { version = "HTTP/1.1"; code = 200 } ;;

let date1 = {
	day = "Monday";
	number = 22;
	month = "January";
	time = "07:28:56";
	timezone = "GMT"
}

let r = {
    status={version="HTTP/1.1"; code=200};
    headers=[Server "nginx/1.6.2"; ContentLength 13];
    body="hello world!\n"
}

















