begin
    proc passed(val z, res v) is
        if z >= 6 then {
            v := 1;
        }else {
            v := 0;
        }
    end
    proc cco(val z, res v) is
        call passed(z, v);
    end
    call cco(7,y);
    y := y + 1;
end