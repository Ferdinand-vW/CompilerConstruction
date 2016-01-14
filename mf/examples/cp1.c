begin
  y := 2; 
  z := 1;
  while x < 0 do
  {
    x := 1;
    y := z * x;
  }

  if y < 0 then {
    x := 1;
    y := z * y;
    }
  else {
    z := z + 1;
    }
end