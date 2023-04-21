module SRGB_16 =
	Color.SRGB_Int (struct
		let max_int = 15;;
	end);;

let parse_srgb_opt (x: string) = (
	let length = String.length x in
	if length = 0 || x.[0] <> '#' then None else
	let ( let* ) = Option.bind in
	match length with
	| 4 -> (* #RGB *)
		let* red = int_of_string_opt ("0x" ^ String.sub x 1 1) in
		let* green = int_of_string_opt ("0x" ^ String.sub x 2 1) in
		let* blue = int_of_string_opt ("0x" ^ String.sub x 3 1) in
		Some (SRGB_16.to_srgb (SRGB_16.make ~red ~green ~blue))
	| 7 -> (* #RRGGBB *)
		let* red = int_of_string_opt ("0x" ^ String.sub x 1 2) in
		let* green = int_of_string_opt ("0x" ^ String.sub x 3 2) in
		let* blue = int_of_string_opt ("0x" ^ String.sub x 5 2) in
		Some (Color.SRGB24.to_srgb (Color.SRGB24.make ~red ~green ~blue))
	| _ -> None
);;

let in_colors = ref [];;

exception Unknown_option of string;;
exception Bad_value of string * string;;
let usage () = (
	let command = Sys.argv.(0) in
	Printf.printf "usage: %s COLORS...\n" command;
	print_endline "Mix sRGB colors in linear RGB space.";
	print_newline ();
	Printf.printf "example: %s '#000000' '#FFFFFF'\n" command
) in
let not_srgb_value s = (
	Bad_value (s, "not sRGB value")
) in
try
	let rec parse_argv i = (
		if i >= Array.length Sys.argv then () else
		let next =
			match Sys.argv.(i) with
			| "--help" ->
				usage ();
				exit 1
			| _ as unknown when String.length unknown > 0 && unknown.[0] = '-' ->
				raise (Unknown_option unknown)
			| _ as color_string ->
				let in_color =
					match parse_srgb_opt color_string with
					| Some in_color -> in_color
					| None -> raise (not_srgb_value color_string)
				in
				in_colors := in_color :: !in_colors;
				i + 1
		in
		parse_argv next
	) in
	parse_argv 1
with
| Unknown_option option ->
	Printf.printf "%s: unknown option: %s\n" Sys.argv.(0) option;
	exit 1
| Bad_value (value, message) ->
	Printf.eprintf "%s: %s: %s\n" Sys.argv.(0) message value;
	exit 1;;

if !in_colors = [] then (
	Printf.eprintf "%s: colors are fewer or too many.\n" Sys.argv.(0);
	exit 1
);;

let out_rgb =
	let r, g, b, n =
		List.fold_left (fun (r, g, b, n) in_color ->
			let in_rgb = Color.rgb_of_srgb in_color in
			let open Color.RGB in
			r +. in_rgb.red, g +. in_rgb.green, b +. in_rgb.blue, n + 1
		) (0.0, 0.0, 0.0, 0) !in_colors
	in
	let n = float_of_int n in
	let open Color.RGB in
	{red = r /. n; green = g /. n; blue = b /. n}
in
let out_color = Color.SRGB24.of_rgb out_rgb in
let open Color.SRGB24 in
Printf.printf "#%02X%02X%02X\n" out_color.red out_color.green out_color.blue;;
