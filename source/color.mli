(* RGB *)

module RGB: sig
	type t = {red: float; green: float; blue: float}
	
	val is_valid: t -> bool
	val make: red:float -> green:float -> blue:float -> t
	val black: t
	val white: t
	val distance: t -> t -> float
end

type rgb_t = RGB.t

val is_valid_rgb: rgb_t -> bool

(* sRGB *)

module SRGB: sig
	type t = {red: float; green: float; blue: float}
	
	val is_valid: t -> bool
	val make: red:float -> green:float -> blue:float -> t
	val black: t
	val white: t
	val of_rgb: rgb_t -> t
	val to_rgb: t -> rgb_t
end

type srgb_t = SRGB.t

val is_valid_srgb: srgb_t -> bool
val srgb_of_rgb: rgb_t -> srgb_t
val rgb_of_srgb: srgb_t -> rgb_t

module type SRGB_IntS = sig
	type t = {red: int; green: int; blue: int}
	
	val is_valid: t -> bool
	val make: red:int -> green:int -> blue:int -> t
	val of_srgb: srgb_t -> t
	val to_srgb: t -> srgb_t
end

module SRGB_Int (_: sig val max_int: int end): SRGB_IntS

module SRGB24 = Color__SRGB24

(* HSV *)

module HSV: sig
	type t = {hue: float; saturation: float; value: float}
	
	val is_valid: t -> bool
	val make: hue:float -> saturation:float -> value:float -> t
	val of_rgb: rgb_t -> t
	val to_rgb: t -> rgb_t
end

type hsv_t = HSV.t

val is_valid_hsv: hsv_t -> bool
val hsv_of_rgb: rgb_t -> hsv_t
val rgb_of_hsv: hsv_t -> rgb_t

(* HSL *)

module HSL: sig
	type t = {hue: float; saturation: float; lightness: float}
	
	val is_valid: t -> bool
	val make: hue:float -> saturation:float -> lightness:float -> t
	val of_rgb: rgb_t -> t
	val to_rgb: t -> rgb_t
end

type hsl_t = HSL.t

val is_valid_hsl: hsl_t -> bool
val hsl_of_rgb: rgb_t -> hsl_t
val rgb_of_hsl: hsl_t -> rgb_t

(* HSY *)

module type HSYS = sig
	type t = {hue: float; saturation: float; intensity: float}
	
	val is_valid: t -> bool
	val make: hue:float -> saturation:float -> intensity:float -> t
	val of_rgb: rgb_t -> t
	val to_rgb: t -> rgb_t
end

module HSY
	(_: sig
		val red: float
		val green: float
		val blue: float
	end): HSYS
