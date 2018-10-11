type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina

type tourna = LEAF of team
	| NODE of tourna * tourna

let rec parenize: tourna -> string = fun tourna ->
	  match tourna with
		|LEAF team->
			(match team with
			| Korea -> "Korea"
			| France -> "France"
			| Usa -> "Usa"
			| Brazil -> "Brazil"
			| Japan -> "Japan"
			| Nigeria -> "Nigeria"
			| Cameroon -> "Cameroon"
			| Poland -> "Poland"
			| Portugal -> "Portugal"
			| Italy -> "Italy"
			| Germany -> "Germany"
			| Norway -> "Norway"
			| Sweden -> "Sweden"
			| England -> "England"
			| Argentina -> "Argentina"
			)
		|NODE (tourna1,tourna2) -> "("^parenize(tourna1)^" "^parenize(tourna2)^")"

(*let _ = print_endline(parenize(LEAF Korea))*)
