begin
    proc fib(val z, u, res v) is
        if z < 3 then {
            v := u + 1;
        }
        else {
            call fib(z-1, u, v);
            call fib(z-2, v, v);
        }
    end
    call fib(x, 0, y);
end