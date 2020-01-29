#revisión 0.0.1 29-01-2020, 00:40 Julia1.1.0
export Wproperties, gravity_wall
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
"""
    gravity_wall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
        b1::Real,b2::Real)
Genera una estructura del tipo `Wproperties`, para un muro de gravedad con los
datos especificados, los datos deben ser ingresados como `keywords`, es decir,
pueden ingresarse en cualquier orden, pero especificando el nombre la variable,
por ejemplo:
`gravity_wall(b1=0.95,b2=0.3,hp=5.7,hz=0.8,t3=1.6,t2=0.3,t1=0.6)`.

                  t1
                 ____            _
                /|  |\\           .
               / |  | \\          .    relleno
              /  |  |  \\        hp
             /   |  |   \\        .
            /    |  |    \\       .
      b1   /  t2 |  |  t3 \\  b2  .
     _____/______|__|______\\______
    |                             |hz
    |_____________________________|

"""
function gravity_wall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
    b1::Real,b2::Real)
    #creando nudos
    nodes=VolatileArray(zeros(1,2));
    push!(nodes,Float64[b1+t2+t1+t3+b2 0]);
    push!(nodes,Float64[b1+t2+t1+t3+b2 hz]);
    push!(nodes,Float64[0 hz]);
    push!(nodes,Float64[b1 hz]);
    push!(nodes,Float64[b1+t2 hz]);
    push!(nodes,Float64[b1+t2+t1 hz]);
    push!(nodes,Float64[b1+t2+t1+t3 hz]);
    push!(nodes,Float64[b1+t2 hz+hp]);
    push!(nodes,Float64[b1+t2+t1 hz+hp]);

    #creando elementos
    elements=VolatileArray([5 6 9 0]);#triángulo izquierdo
    push!(elements,[7 8 10 0]);#triángulo derecho
    push!(elements,[1 2 3 4]);#cuadrilátero base (zapata)
    push!(elements,[6 7 10 9]);#cuadrilatero central (en pantalla)

    props=VolatileArray(zeros(4,3));

    #construyendo muro (obteniendo áreas y centroides)
    #el 2 indica el índice de la fila del último elemento triangular
    build_wall(Array(nodes),Array(elements),Array(props),2);

    return Wproperties(nodes,elements,props,2);
end
