begin
  x := 2;
  y := 4;
  x := 1;
  if x > 1
    then {
        z := y;
    }
    else  {
        z := x * x;
    }
  x := z;
end