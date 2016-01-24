begin
    proc fac(val z,res v) is
        if z == 0 then {
            v :=  0;
        }
        else {
            call fac(z - 1,v);
            skip;
            v := v + 1;
        }
    end
    call fac(3,y);
end