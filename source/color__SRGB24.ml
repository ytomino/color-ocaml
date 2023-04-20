module Uint8 = struct
	let max_int = 255;;
end;;

include Color.SRGB_Int (Uint8);;
