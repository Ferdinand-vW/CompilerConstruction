begin
    proc fib(val z, u, res v) is
        if z < 3 then {
            v := u + 1;
        }
        else {
            call fib (z - 1,u + 1,v);
        }
    end