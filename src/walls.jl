#revisión 0.0.0 28-01-2020, 01:10 Julia1.1.0
export Wproperties
mutable struct Wproperties{T}
    nod::VolatileArray{T,2}
    elm::VolatileArray{Int64,2}
    prop::VolatileArray{T,2}
    pbreak::Int64
    function Wproperties(nod::VolatileArray{T,2}, elm::VolatileArray{Int64,2},
        prop::VolatileArray{T,2},pbreak::Int64) where {T<:Real}
        if size(prop)[1]==size(elm)[1]
            new{T}(nod,elm,prop,pbreak);
        else
            error("las alturas de elm y prop deben ser iguales")
        end
    end
end
function Base.show(io::IO,x::Wproperties{<:Real})
    print(io,"$(typeof(x))\n");
    print(io,"Fields:\n")
    print(io,"nod   :$(size(x.nod)[1])x$(size(x.nod)[2]) $(typeof(x.nod))\n");
    print(io,"elm   :$(size(x.elm)[1])x$(size(x.elm)[2]) $(typeof(x.elm))\n");
    print(io,"prop  :$(size(x.prop)[1])x$(size(x.prop)[2]) $(typeof(x.prop))\n");
    print(io,"pbreak:$(x.pbreak) $(typeof(x.pbreak))");
end

function gravity_wall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
    B1::Real,B2::Real)

end
