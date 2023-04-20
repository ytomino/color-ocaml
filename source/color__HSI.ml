module Average = struct
	let red = 1.0;;
	let green = 1.0;;
	let blue = 1.0;;
end;;

include Color.HSY (Average);;
