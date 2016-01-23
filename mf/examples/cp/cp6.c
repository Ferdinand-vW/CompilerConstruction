begin
    proc fac(val z, u, res v) is
        if z <= 0 then {
            skip;
        }
        else {
            u := v * z;
            call fac(z-1,0, u);
        }
    end
    call fac(x, 0, v);
end         