begin
    proc square(val z, res v) is
        v := z * z;
    end
    call square(5,y);
    call square(4,z);
    v := z * y;
end