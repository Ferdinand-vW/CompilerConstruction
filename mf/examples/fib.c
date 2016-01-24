begin
    proc fib(val z, u, res v) is
        if z < 3 then {
            skip;
        }
        else {
            call fib (z - 1,u,v);
            v := u;
            call fib (z - 2,v,v);
            v := z;
        }
    end
    call fib(4, 0, y);
end