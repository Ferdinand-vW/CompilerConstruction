begin
    proc square(val z, res v) is
        v := z * z;
    end
    proc doublesquare(val z, res v) is
        call square(z, v);
        call square(v,v);
    end
    call doublesquare(7,y);
end