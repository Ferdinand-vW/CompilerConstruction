begin
a := 100;
i := 1;
while (i <= 10) do{
    if (i < 3) then{
        skip;
    }else{
        a := a - 1;
    }
    i := i + 1;
}
end