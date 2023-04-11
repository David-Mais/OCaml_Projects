let rec member c t l = match l with
|[] -> false
|head::tail -> if(c t head) then true
else member c t tail;;